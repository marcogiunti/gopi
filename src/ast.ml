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

(** Abstract syntax tree -- the LSpi language and its operations *)
open ListOps
open Param


(** Syntax of linear secret pi calculus (LSpi) processes *)			 
type lsprocess =
  | Set of string list * lsprocess
  | Let of string * lsprocess * lsprocess
  | Print of string
  | Aout of string * string
  | Out of string * string * lsprocess
  | Inp of string * string * lsprocess 
  | Par of lsprocess * lsprocess
  | Parl of lsprocess list
  | Zero 
  | New of string * lsprocess
  | Hide of string * lsprocess
  | Repl of lsprocess
  | VarP of string

(** String representation of LSpi process *)
let rec toStringLSpi = function
  | Set(l, p) ->
     "set::"^ toStringLSpi p
  | VarP(s) -> "Var " ^ s
  | Let(label, p, cont) ->
     "let " ^ label ^ " = "
     ^ (toStringLSpi p) ^ " in "^ (toStringLSpi cont)
  | Inp(x,y,Zero) ->
     x ^ "?(" ^ y ^ ")"
  | Inp(x,y,p) ->
     x ^ "?(" ^ y ^ ").(" ^ (toStringLSpi p) ^ ")"
  | Print(v) ->
     "print:" ^ v 
  | Aout(x,v) ->
     x ^ "!<" ^ v ^ ">"
  | Out(x,v,p) ->
     x ^ "!<" ^ v ^ ">.(" ^ (toStringLSpi p) ^")"
  | Par(p,q) ->
     (toStringLSpi p) ^" | " ^ (toStringLSpi q)
  | Parl(l) ->
     (match l with
      | [] -> ""
      | h::[] -> (toStringLSpi h) 
      | h::t -> 
	 (toStringLSpi h) ^" || " ^ (toStringLSpi (Parl t)))
  | Repl(p) ->
     "*("  ^ (toStringLSpi p) ^")" 
  | New(x, p) ->
     "(new " ^ x ^") (" ^ (toStringLSpi p) ^ ")"
  | Hide (x,  p) ->
     "[hide " ^ x ^"]" ^ " [" ^ (toStringLSpi p) ^ "]"
  | Zero -> "0"

(** [subst p z q] is [q] with Var [z] substituted for process [p]. *)
let rec substP p z = function
  | VarP(y) ->
     if z = y
     then p
     else VarP y
  | Set(l, q) ->
     Set(l, substP p z q)
  | Let(x, q, r) ->
     Let(x, substP p z q, substP p z r)
  | Inp(x,y,q)->
     Inp(x,y, substP p z q)
  | Out(x,y,q) ->
     Out(x,y, substP p z q)
  | Par (q, r) ->
     Par(substP p z q, substP p z r)
  | New(x, q) ->
     New(x, substP p z q) 
  | Hide (x,  q) ->
     Hide(x, substP p z q)
  | Repl(q) ->
     Repl(substP p z q)
  | q -> q

(** Most Ast operations do not support let/open processes 
    and require substP to be executed previously *)
exception Unsupported of string

(** LSpi process contains hide construct *)
let rec secretProcess = function
  | Hide(_, p) -> true
  | Parl l ->
     orList (List.map secretProcess l)
  | Par (p, q) ->
     secretProcess p || secretProcess q
  | Inp (_, _, p) 
  | Out (_, _, p)
  | New (_, p) 
  | Repl(p) 
  | Set (_, p) ->
     secretProcess p
  | Let(_,_,_) 
  | VarP _ as p ->
     raise (Unsupported (toStringLSpi p))
  | _ -> false 

(** Bound variables of LSpi process *)
let boundVars p =
  let rec bvarsR acc = function
    | Parl l ->
       (match l with
	| [] -> acc
	| h::t -> bvarsR (bvarsR acc h) (Parl t))
    | Par (p, q) ->
       let accP = bvarsR acc p in
       bvarsR accP q
    | Inp (x, y, p) ->
       bvarsR (acc @ [y]) p
    | Out (_, _, p)
    | Hide(_, p)
    | New (_, p) 
    | Repl(p) 
    | Set (_, p) ->
       bvarsR acc p
    | Let(_,_,_) 
    | VarP _ as p ->
       raise (Unsupported (toStringLSpi p))
    | _ -> acc in
  bvarsR [] p
	 
(** Bound names of LSpi process *)
let boundNames p = 
  let rec bnamesR acc = function
    | Parl l ->
       (match l with
	| [] -> acc
	| h::t -> bnamesR (bnamesR acc h) (Parl t))
    | Par (p, q) ->
       let accP = bnamesR acc p in
       bnamesR accP q
    | Inp (_, _, p)
    | Out (_, _, p)
    | Repl(p) 
    | Set (_, p) ->
       bnamesR acc p 
    | Hide(x, p)
    | New (x, p) ->
       bnamesR (acc @ [x]) p
    | Let(_,_,_) 
    | VarP _ as p ->
       raise (Unsupported (toStringLSpi p))
    | _ -> acc in
  bnamesR [] p

(** Bound channels of LSpi process *)
let boundChannels p =
  boundVars p @ boundNames p

(** Free names of LSpi process *)
let freeNames p =
  let rec freeNamesR acc = function
    | Par (p, q) ->
       let accP = freeNamesR acc p in
       freeNamesR accP q
    | Parl l ->
       (match l with
	| [] -> acc
	| h::t -> freeNamesR (freeNamesR acc h) (Parl t))
    | Aout (x, y) ->
       add  y (add x acc)
    | Print (x) ->
       add x acc
    | Out (x, y, p) ->
       let accx = add x acc in
       let accy = add y accx in
       freeNamesR accy p 
    | Inp (x, y, p) ->
       let list = freeNamesR (add x acc) p in
       remove y list
    | Hide(x, p)
    | New (x, p) ->
       let list = freeNamesR acc p in
       remove x list
    | Set (_, p) 
    | Repl(p) ->
       freeNamesR acc p
    | Let(_,_,_) 
    | VarP _ as p ->
       raise (Unsupported (toStringLSpi p))
    | _ -> acc in
  let l = freeNamesR [] p in
  remove "print" l

(** Substitutes x with y in LSpi process p *)	 
let rec subst z w =
  (* substitutes z with w if z = s *)
  let subst_ z w s =
    if z = s then w else s in
  function
  | Print s ->
     Print (subst_ z w s)
  | Par (p, q) ->
     Par (subst z w p, subst z w q)
  | Out (x, y, p) ->
     Out(subst_ z w x, subst_ z w y, subst z w p)
  | Inp (x, y, p) ->
     Inp(subst_ z w x, y, subst z w p)
  | New (x, p) ->
     New (x, subst z w p)
  | Hide (x, p) ->
     Hide (x, subst z w p)
  | Repl (p) ->
     Repl (subst z w p)
  | Set (l, p) ->
     Set (List.map (subst_ z w) l, subst z w p)
  | Parl (l) ->
     Parl (List.map (subst z w) l)
  | Let(_,_,_) 
  | VarP _ as p ->
     raise (Unsupported (toStringLSpi p))
  | p -> p

     
	   
(** Short for string_of_int *)
let stri = string_of_int


(** Generate next "fresh" variable, used by Ast.alphaConvert *)
let nextVal =
  let  nextId = ref (0) in
  fun () ->
  nextId := !nextId + 1;
  "x"^(stri ((Random.int rANDOMLIMIT) * !nextId))

(** Alpha-conversion of LSpi process *)
let rec alphaConvert = function
  | Inp(x, y, q) ->
     let ay = nextVal() in
     Inp(x, ay, alphaConvert (subst y ay q)) 
  | New(x,  q) ->
     let ax = nextVal() in
     New(ax, alphaConvert (subst x ax q))		     
  | Hide(x,  q) ->
     let ax = nextVal() in
     Hide(ax, alphaConvert (subst x ax q))
  | Out(x, y, q) -> Out(x, y, alphaConvert q)
  | Repl q ->
     Repl (alphaConvert q)
  | Par (q1, q2) ->
     Par (alphaConvert q1, alphaConvert q2)
  | Set(l, p) ->
     Set (l, alphaConvert p)
  | Parl l ->
     Parl (List.map alphaConvert l)
  | Let(_,_,_) 
  | VarP _ as p ->
     raise (Unsupported (toStringLSpi p))
  | p -> p

(** Remove linear channels from LSpi process *)
let removeSet p =
  let rec removeSetR acc = function
    | Set(l, p) ->
       let l1, q = removeSetR (acc@l) p in
       l1, q
    | Inp(x, y, p) ->
       let l1, q = removeSetR acc p in
       l1, Inp(x ,y ,q)
    | Out(x, y, p) ->
       let l1, q = removeSetR acc p in
       l1, Out(x, y, q)
    | New(x, p) ->
       let l1, q = removeSetR acc p in
       l1, New(x, q)
    | Hide(x, p) ->
       let l1, q = removeSetR acc p in
       l1, Hide(x, q)
    | Repl(p) ->
       let l1, q = removeSetR acc p in
       l1, Repl(q)
    | Par(p1, p2) ->
       let l1, q1 = removeSetR acc p1 in
       let l2, q2 = removeSetR acc p2 in
       (l1@l2), Par(q1, q2)
    | Parl l ->
       let t = List.map (removeSetR acc) l in
       let chans =  acc @ (List.flatten (List.map fst t)) in
       let procs = (List.map snd t) in
       chans, Parl procs
    | Let(_,_,_) 
    | VarP _ as p ->
       raise (Unsupported (toStringLSpi p))
    | p -> acc, p in
  removeSetR [] p
	     
(** Transform parallel LSpi process in parallel list LSpi process *)
let rec toParlProcess = function 
  | Inp(x, y, p) ->
     Inp(x, y, toParlProcess p)
  | Out(x, y, p) ->
     Out(x, y, toParlProcess p)
  | New(x, p) ->
     New(x,toParlProcess p)
  | Hide(x, p) ->
     Hide(x,toParlProcess p)
  | Repl(p) ->
     Repl(toParlProcess p)
  | Par(p, q) ->
     let parlP = toParlProcess p and
	 parlQ = toParlProcess q in
     (match cap (boundNames parlP) (freeNames parlQ) with
      | []  ->
	 Parl(parlP :: [parlQ])
      | _ ->
	 Parl((alphaConvert parlP) :: [alphaConvert parlQ]))
  | Parl l ->
     Parl (List.map toParlProcess l)
  | Let _
  | VarP _ as p ->
     raise (Unsupported (toStringLSpi p))
  | p -> p

(** Flatten nested parallel lists in LSpi process. 
    Used to simplify generation of Go code *)
let rec flatParl = 
  let rec innerParl = function
    | [] -> []
    | h::t ->
       match h with
       | Parl(l) ->
	  innerParl(l@t)
       | _ ->
	  h::(innerParl t) in
  function
  | Parl([]) ->
     Parl([])
  | Parl(h::t) ->
     (match flatParl (Parl(t)) with
      | Parl(l) ->
	 Parl(innerParl((flatParl h)::l))
      | _ ->
	 raise (Failure (__LOC__)))
  | Inp(x,y,p) ->
     Inp(x,y,flatParl p)
  | Out(x,y,p) ->
     Out(x,y, flatParl p)
  | New(x,p) ->
     New(x,flatParl p)
  | Hide(x,p) ->
     Hide(x,flatParl p)
  | Repl(p) ->
     Repl(flatParl p)
  | Let(_,_,_) 
  | VarP _ as p ->
     raise (Unsupported (toStringLSpi p))
  | p -> p
