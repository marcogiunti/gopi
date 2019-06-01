(**************************************************************************)
(*                               gopi                                     *)
(*                                                                        *)
(*  Copyright 2019 Marco Giunti. All rights reserved. This file is        *)
(*  distributed under the terms of the GNU Public License version 3.0.    *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(** Pi calculus *)

(* Inspired by: Marco Giunti: Static Semantics of Secret Channel *)
(* Abstractions. NordSec 2014: 165-180 *)
open Ast
open ListOps
       
type value =   
  | Top
  | Str of string

type scope =
  | Static
  | Dynamic
      
(** Syntax of pi calculus processes *)
type process = 
  | OutPi of string * value * process
  | InpPi of string * string * process
  | ParPi of process * process
  | NewPi of string * scope * int * process
  | ReplPi of process
  | PrintPi of value
  | ZeroPi
      
(************** UTILITIES ***************)
      
(** String representation of a process *)
let rec toStringPi = function
  | ZeroPi -> "0"
  | OutPi(x, Top, p) ->
     x ^ "!" ^ "@." ^ (toStringPi p) 
  | OutPi(x, Str y, p) ->
     x ^ "!" ^ y ^ "." ^ (toStringPi p)
  | InpPi(x,y,p) -> 
     x ^ "?" ^ y ^ "." ^ (toStringPi p)
  | NewPi(x,_,_,p) -> 
     "new " ^ x ^ " (" ^ (toStringPi p)^ ")"
  | ReplPi(p) ->
     "*" ^ (toStringPi p)
  | ParPi(p, q) ->
     (toStringPi p) ^ " | " ^ (toStringPi q)
  | PrintPi(Top) ->
     "print::@"
  | PrintPi(Str x) ->
     "print::" ^ x

(** Free names of a process *)
let freeNamesPi p =
  let rec freeNamesPiR acc = function
    | PrintPi (Top)
    | ZeroPi ->
       acc
    | ParPi (p, q) ->
       let accP = freeNamesPiR acc p in
       freeNamesPiR accP q
    | PrintPi (Str x) ->
       add x acc
    | OutPi (x, Str y, p) ->
       let accx = add x acc in
       let accy = add y accx in
       freeNamesPiR accy p
    | OutPi(x,Top, p) ->
       let accx = add x acc in 
       freeNamesPiR accx p
    | InpPi (x, y, p) ->
       let accx = add x acc in 
       let list = freeNamesPiR accx p in
       ListOps.remove y list
    | NewPi (x, _, _, p) ->
       let list = freeNamesPiR acc p in
       ListOps.remove x list
    | ReplPi(p) ->
       freeNamesPiR acc p in 
  let l = freeNamesPiR [] p in
  ListOps.remove "print" l

(** Substitutes x with y in process p *)	 
let rec subst z w =
  let subst_ z w s =
    if z = s then w else s in
  function
  | PrintPi Top
  | ZeroPi as p ->
     p
  | PrintPi (Str s) ->
     PrintPi (Str (subst_ z w s))
  | ParPi (p, q) ->
     ParPi (subst z w p, subst z w q)
  | OutPi (x, Str y, p) ->
     OutPi(subst_ z w x, Str (subst_ z w y), subst z w p)
  | OutPi(x, Top, p) ->
     OutPi(subst_ z w x, Top, subst z w p)
  | InpPi (x, y, p) ->
     InpPi(subst_ z w x, y, subst z w p)
  | NewPi (x, m, i, p) ->
     NewPi (x, m, i, subst z w p)
  | ReplPi(p) ->
     ReplPi (subst z w p) 
	    
(** Catalyzer of a process given a set of linear channels *)
let catalyzer p linear_channels order =
  let rec inputOfOrder x = function
    | 0 -> ZeroPi
    | n when n > 0 ->
       let y = nextVal() in
       InpPi(x, y, inputOfOrder y (n - 1))
    | _ -> raise (Invalid_argument "Order must be non-negative") in  
  let rec catalyzerR fn = function
    | PrintPi(_) | ZeroPi -> ZeroPi
    | OutPi(x, y, p) ->
       if List.mem x fn then 
	 let z = nextVal() in
	 InpPi(x, z, ParPi(inputOfOrder z order, catalyzerR fn p))
       else
	 catalyzerR fn p
    | InpPi(x, y, p) ->
       if List.mem x fn then 
	 let z = nextVal() in
	 OutPi(x, Str z,  ParPi(inputOfOrder z order, catalyzerR (z :: fn) (subst y z p)))
       else
	 catalyzerR fn p 
    | NewPi(x,_,_,p) ->
       catalyzerR fn p
    | ReplPi(p) ->
       ReplPi(catalyzerR fn p)
    | ParPi(p, q) ->
       let cp = catalyzerR fn p in
       let cq = catalyzerR fn q in
       ParPi (cp, cq) in
  let fn = (freeNamesPi p) in
  catalyzerR (listDiff fn linear_channels) p
	     
(** Maps a process to a LSpi process. 
    Used to map catalyzers in Gopi.gopi *)
let rec toLSpiProcess = function
  | ZeroPi -> Zero
  | InpPi(x, y, p) -> Inp(x, y, toLSpiProcess p)
  | OutPi(x, Str y, p) -> Out(x, y, toLSpiProcess p)
  | ParPi(p, q) -> Par(toLSpiProcess p, toLSpiProcess q)
  | ReplPi(p) -> Repl(toLSpiProcess p)
  | NewPi(x, Dynamic, _,p) -> New(x, toLSpiProcess p)
  | NewPi(x, Static, _,p) -> Hide(x, toLSpiProcess p)
  | p -> raise (Invalid_argument (toStringPi p))

(** Translate LS processes to  processes *)
let rec toProcessPi =
  let rec toParProcess acc = function 
    | [] -> acc
    | h::t -> toParProcess (ParPi(acc, toProcessPi h)) t in
  function
  | Zero ->
     ZeroPi
  | Inp(x, y, p) ->
     InpPi(x, y, toProcessPi p)
  | Out(x, y, p) ->
     OutPi(x, Str y, toProcessPi p)
  | Aout(x, y) ->
     toProcessPi (Out(x, y, Zero))
  | Print(y) ->
     PrintPi(Str y)
  | New(x, p) ->
     NewPi(x, Dynamic, 0, toProcessPi p)
  | Hide(x, p) ->
     NewPi(x, Static, Random.int 1000, toProcessPi p)
  | Par(p, q) ->
     ParPi(toProcessPi p, toProcessPi q)
  | Parl(l) ->
     toParProcess ZeroPi l
  | Repl(p) ->
     ReplPi(toProcessPi p)
  | p -> raise (Unsupported (toStringLSpi p))
