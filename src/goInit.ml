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

(** Go type declarations and Init *)

open Format
open ListOps
open GoValues
open Ast
open Param

let license =
  ["/**************************************************************************/";
   "/*                               gopi                                     */";
   "/*                                                                        */";
   "/*  Copyright 2019 Marco Giunti. All rights reserved. This file is        */";
   "/*  distributed under the terms of the GNU Public License version 3.0.    */";
   "/*  This software is distributed in the hope that it will be useful,      */";
   "/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */";
   "/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */";
   "/*  GNU General Public License for more details.                          */";
   "/*                                                                        */";
   "/**************************************************************************/"]

let bugs =
  ["\n/* Please report BUGS to: marco dot giunti at gmail dot com               */\n"]
    
(** Indentation of given level *)
let indentN level =
  let rec indentR acc = function 
    | 0 -> sprintf "\n%s" acc
    | n when n > 0 -> indentR (sprintf "%s%s" acc iNDENT) (n - 1)
    | _ -> raise (Failure "wrong parameter in indent") in
  (indentR "" level )

(** Go type declaration of base values in string format *)
let freeStrNames l =
  let rec freeStrNamesR acc = function
    | [] -> acc
    | (n, gv) :: t ->
       (match gv with
	| "base" ->
	   freeStrNamesR (acc @ [sprintf "var %s %s = \"%s\"" n gv n]) t
	(* TODO : Add Parametricity to BASE, now we assume base = string *)
	| _ ->
	   freeStrNamesR (acc @ [sprintf "var %s %s" n gv]) t) in
  let rec addBase = function
    | [] -> []
    | h::t ->
       (h,"base") :: (addBase t) in
  freeStrNamesR [] (addBase l)
		
(** Go type declaration of channel values in string format *)
let freeTypedNames l  =
  let refInferred = ref [] in
  let rec freeTypedNamesR acc = function
    | [] -> acc
    | (n,gv)::t ->
       freeTypedNamesR
	 (refInferred := !refInferred @ [n];
	  acc @ [sprintf "var %s %s" n (toString gv)]) t in
  let typeDefs  = freeTypedNamesR [] l in
  typeDefs, !refInferred

(** [register (n, gv)] returns entry Gamma.register(n, v) *)		     
let register (n, gv) =
  [sprintf
     "%sGamma.register(\"%s\", \"%d\")\n%s%s =  Gamma.chanOf(\"%s\").(%s)"
     iNDENT n (toInt gv) iNDENT n n (toString gv)
  ]    

(**  Marshalling and unmarshalling ooperations of Gamma.ord_n *)    
let initMarshall level = 
  let rec initMarshall_ acc = function
    | (-1) ->
       [iNDENT^"Gamma.ord.toStr = make(map[base]string)";
	iNDENT^"Gamma.ord.fromStr = make(map[string]base)"]
       @acc
    | 0 ->
       initMarshall_
	 ([iNDENT^"Gamma.ord0.toStr = make(map[chan0]string)";
	   iNDENT^"Gamma.ord0.fromStr = make(map[string]chan0)";
	   iNDENT^"Gamma.ord0.queue = make(map[chan0]queueBase)";
	   iNDENT^"Gamma.ord0.dequeue = make(map[chan0]func())"]
	  @acc) (-1)
    | n ->
       initMarshall_ 
	 ([iNDENT^"Gamma.ord"^(stri n) ^ ".toStr = make(map[chan"^(stri n) ^ "]string)";
	   iNDENT^"Gamma.ord"^(stri n) ^ ".fromStr = make(map[string]chan"^(stri n) ^ ")"; 
	   iNDENT^"Gamma.ord"^(stri n) ^ ".queue = make(map[chan"^(stri n) ^ "]queueChan"^(stri (n-1)) ^ ")";
	   iNDENT^"Gamma.ord"^(stri n) ^ ".dequeue = make(map[chan"^(stri n) ^ "]func())"]@
	    acc) (n-1) in
  initMarshall_ [] level

(** Code of Go.init() *)	
let freeInit l max_order =
  let rec freeInitR acc = function
    | [] -> acc
    | (n,gv)::t ->
       freeInitR
	 (acc@(register(n,gv))) t  in
  ["func init(){"; 
   iNDENT^"done = make(chan bool)";
   iNDENT^"key = \"default\"";
   iNDENT^"counter = SafeCounter{v: make(map[string]int)}";
   iNDENT^"rand.Seed(int64(time.Now().Nanosecond()))"] 
  @(initMarshall max_order)
  @["\n"^iNDENT
    ^ "fmt.Printf(\"**********Init*********\\n\")"] 
  @(freeInitR [] l)
  @["}"]
