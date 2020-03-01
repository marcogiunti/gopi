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

(** Inferring types of Pi.process by generating SMT-LIB 
     constraints and by calling Z3 *)

open Format
open ListOps
open Pi
open GoValues
open Param

(************ EXCEPTIONS **********)
       
(** Future extensions *)
exception NotSupported
	    
(*********** Z3 GLOBALS **********)

(** The Z3 constraints *)
let refConstraints = ref []
			 
(** Add entry to refConstraints *)
let addConstr s =
  if s <> "" then
    let l = !refConstraints in
    refConstraints := l@[s]
			  
(** Cat list to refConstraints *)
let rec addConstrList = function
  | [] -> ()
  | h::t -> addConstr h ; (addConstrList t)

(** Map channels to (i, o, delegated_channels)  *)
module Env = Map.Make(String)
		     
(** Global environment refEnv tracks usage of free and bound variables 
    to enforce linearity constraints *)
let refEnv = ref Env.empty

(** Generate Z3 labels *) 
let next_label =
  let  nAssert = ref (0) in
  fun () ->
  nAssert := !nAssert + 1;
  sprintf " :named A%d" (!nAssert) 

(** Generating labeled Z3 entries - 1 param *)
let z3_1 s x =
  sprintf s x (next_label())
(** Generating labeled Z3 entries - 2 param *)  
let z3_2 s x1 x2 =
  sprintf s x1 x2 (next_label())
(** Generating labeled Z3 entries - 3 param *)
let z3_3 s x1 x2 x3 =
  sprintf s x1 x2 x3 (next_label())
(** Generating labeled Z3 entries - 4 param *)
let z3_4 s x1 x2 x3 x4 =
  sprintf s x1 x2 x3 x4 (next_label())
(** Generating labeled Z3 entries - 5 param *)
let z3_5 s x1 x2 x3 x4 x5 =
  sprintf s x1 x2 x3 x4 x5 (next_label())
	  
(** Print environment Env *)
let printEnv x triple = match triple with
  | (i, o, (d : string list)) ->
     printf "%s %d %d @." x i o ;
     printList d
	       
(** Iterating function to generate Z3 linear constraints --  
    usage :  Env.iter (constrEnv boundVars deadlock_detection) !refEnv *)
let constrEnv boundVars deadlock_detection x triple =
  let rec delegConstrI = function
    | [] ->
       " 0 "
    | h::t ->
       sprintf " (+ (i (payload %s ))%s ) " h (delegConstrI t) in
  let rec delegConstrO = function
    | [] ->
       " 0 "
    | h::t ->
       sprintf " (+ (o (payload %s ))%s ) " h (delegConstrO t) in
  match triple with
  | (i, o, l) ->
     addConstrList 
       ([sprintf ";; input constraint of %s" x ;
	 if  deadlock_detection || (List.mem x boundVars)
	 then
	   z3_5
	     "(assert (! (=> (isLinear %s)  (and (= (i %s) 1) (= (i %s) (+ %d%s))))%s))"
	     x x x i (delegConstrI l)
	 else
	   z3_4
	     "(assert (! (=> (isLinear %s) (= (i %s) (+ %d%s)))%s))" x x i (delegConstrI l) 
	]
	@[";; output contstraint of " ^ x ^ "";
	  if deadlock_detection || (List.mem x boundVars)
	  then
	    z3_5
	      "(assert (! (=> (isLinear %s) (and (= (o %s) 1) (= (o %s) (+ %d%s))))%s))"
	      x x x o (delegConstrO l)
	  else
	    z3_4
	      "(assert (! (=> (isLinear %s) (= (o %s) (+ %d%s)))%s))" x x o (delegConstrO l)
	 ])

(** Linearity constraints *)
let linearBound l =
  let rec linearBound_ acc = function
    | [] ->
       if acc <> "" then
	 z3_1 "(assert (! %s%s))" acc
       else
	 acc
    | h::t ->
       let res =
	 sprintf " (and (= (i %s) 1) (= (o %s) 1))" h h in
       linearBound_ (acc ^ res) t in
  linearBound_ "" l
	     
(**  Find entries in refEnv *)
let listed x = try Env.find x !refEnv  
	       with Not_found -> (-3, -3, [])

(**  Add linear output to refEnv *)
let addLinearOutput x =
  let i, o, d = Env.find x !refEnv in
  refEnv := Env.add x (i, o + 1, d) !refEnv;
  o - 1
	
(**  Add linear input to refEnv *)
let addLinearInput x =
  let (i, o, d) = Env.find x !refEnv in
  refEnv := Env.add x (i + 1, o, d) !refEnv;
  i - 1
	
(**  Add linear delegation to refEnv *)
let addLinearDelegation x delegatedOnChan=
  let (i, o, d) = Env.find x !refEnv in
  refEnv := Env.add x (i, o, d@[delegatedOnChan]) !refEnv;
  d@[delegatedOnChan]

(** Generate Z3 isLinear constraints *)
let rec addLinearChannels = function 
  | [] -> ()
  | h::t ->
     addConstrList
       [sprintf "(assert (! (isLinear %s)%s))" h (next_label())];
     addLinearChannels t 

(** Prefix is a set to collect prefixes *)
module Prefix =
  Set.Make(struct
	      let compare = Stdlib.compare
	      type t = string
	    end)
	  
(** Allow is a set to collect allowed integers *)
module Allow =
  Set.Make(struct
	      let compare = Stdlib.compare
	      type t = int
	    end)

(** Clash of linear declaration and bound variables 
    Raised by infer *)
exception AlphaRename of string

(** Iterator on Prefix generate constraints for all prefixes *)
let iterPrefix orders prefix x =
  Prefix.iter
    (fun p ->
     orders :=
       (!orders)@ 
	 [z3_3
	    "(assert (! (=> (isLinear %s) (< (ord %s) (ord %s)))%s))" x p x]) prefix

(*********** Type inference **********)
(** [infer prefixes allowedCasts p ignoreFun deadlock_detection] adds the constraints 
    of p to refConstraints, and track linear usage of p in refEnv.
    Prefix is used to generate order constraints on linear channels.
    AllowedCasts is used to manage secret channels, cf.\[Giunti, NordSec 2014\]
    IgnoreFun is a function used to ignore linear constraints when 
    the process does not contain linear channels 
    Deadlock_detection is on by default, must be switched of with -nf option *)
let infer prefix allowedCasts p ignoreFun deadlock_detection =
  let rec infer_ prefix allowedCasts = function
    | ZeroPi ->
       ZeroPi
	 
    | PrintPi (Top) ->
       PrintPi(Top)
	      
    | PrintPi (Str x) ->
       begin
	 match  listed x with
	 | (-3,-3,_) -> 
	    refEnv := Env.add x ((0,0,[])) !refEnv; 
	    addConstrList
	      [sprintf ";; channel type %s" x ;
	       sprintf "(declare-const %s (Chantype))" x;
	      ];
	    addConstrList
	      (ignoreFun
		 ([z3_4
		     "(assert (! (and (>= (i %s) 0) (<= (i %s) 2) (>= (o %s) 0) (<= (o %s) 2))%s))" x x x x]))
	 | _ -> ()
       end ;
       PrintPi(Str x)
	      
    | ReplPi(p) ->
       let p1 = infer_ prefix allowedCasts p in
       let rec freeNotLin = function
	 | [] ->
	    ()
	 | h::t ->
	    (* breaks linearity under replication *) 
	    let _ = addLinearOutput h and
		_ = addLinearInput h in 
	    freeNotLin t in
       freeNotLin (freeNamesPi p1);
       ReplPi(p1)
	     
    | ParPi(p,q) ->
       let p1 = infer_ prefix allowedCasts p in 
       addConstr "; change of thread";
       let q1 = infer_ prefix allowedCasts q in
       ParPi(p1, q1)
	    
    | NewPi(x, s, i ,p) ->
       begin
	 match  listed x with
	 | (-3, -3, _) -> 
	    refEnv := Env.add x ((0,0,[])) !refEnv; 
	    addConstrList
	      [sprintf ";; channel type %s" x ;
	       sprintf "(declare-const %s (Chantype))" x ;
	       z3_1 "(assert (! (isChannel %s)%s))" x ;
	       z3_2 "(assert (! (= (id %s) %d)%s))" x i];
	    addConstrList
	      (ignoreFun 
		 ([z3_4
		    "(assert (! (and (>= (i %s) 0) (<= (i %s) 2) (>= (o %s) 0) (<= (o
		     %s) 2))%s))" x x x x])) ;
	    
	    begin
	      match s with
	      | Static -> 
		 addConstr (z3_1 "(assert (! (= (scope %s) static)%s))" x) 
	      | _ ->
		 addConstr (z3_1 "(assert (! (= (scope %s) dynamic)%s))" x)
	    end ;

	    let cont = infer_ prefix (Allow.add i allowedCasts) p  in  
	    NewPi(x, s, i, cont)
	 | _ -> raise (AlphaRename x)
       end ;
       
    | OutPi(x, Str y, p) ->
       begin
	 match listed x with
	 | (-3, -3, _)  ->
	    refEnv := Env.add x ((0,0,[])) !refEnv; 
	    addConstrList
	      [sprintf ";; channel type %s" x ;
	       sprintf "(declare-const %s (Chantype))" x ; 
	       z3_1 "(assert (! (isChannel %s)%s))" x ;
	       z3_1 "(assert (! (= (scope %s) dynamic)%s))" x ;
	       z3_1 "(assert (! (= (id %s) 0)%s))" x];
	    addConstrList
	      (ignoreFun
		 ([z3_4
		    "(assert (! (and (>= (i %s) 0) (<= (i %s) 2) (>= (o %s) 0) (<= (o %s) 2))%s))" x x x x]))
	 | _ -> ()
       end ;
       
       begin
	 match deadlock_detection with
	 | true ->
	    let orders = ref [] in
	    iterPrefix orders prefix x;
	    addConstrList
	      (ignoreFun
		 ([";;deadlock prevention on linear channels"]
		  @(!orders)));
	 | _ -> ()
       end ;
       
       let _ = addLinearOutput x in
       begin
	 match listed y with
	 | (-3,-3,_)  ->
	    refEnv := Env.add y ((0,0,[])) !refEnv; 
	    addConstrList
	      [sprintf ";; channel type %s" y ;
	       sprintf "(declare-const %s (Chantype))" y ;
	       z3_1 "(assert (! (= (scope %s) dynamic)%s))" y ;
	       z3_1 "(assert (! (= (id %s) 0)%s))" y];
	    addConstrList
	      (ignoreFun
		 ([z3_4
		    "(assert (! (and (>= (i %s) 0) (<= (i %s) 2) (>= (o %s) 0) (<= (o %s) 2))%s))" y y y y]));

	    begin
	      match deadlock_detection with
	      | true ->
		 addConstrList
		   (ignoreFun
		      ([";;deadlock prevention on linear delegation";
		       z3_3
			 "(assert (! (=> (isLinear %s) (< (ord %s) (ord %s)))%s))"
			 x x y]))
	      | _ -> ()
	    end ;
	    
	 | _ -> ()
       end;
       
       addConstrList
	 [sprintf ";; output mode of %s" x ;
	  z3_1 "(assert (! (isChannel %s)%s))" x ;
	  z3_2 "(assert (! (=> (= (scope %s) dynamic) (= (id %s) 0))%s))" x x 
	 ];
       
       let _ = addLinearDelegation y x in 
       addConstr
	 (z3_2 "(assert (! (equal %s (payload %s))%s))" y x) ;
       
       let q = infer_ (Prefix.add x prefix) allowedCasts p in
       OutPi (x, Str y, q)
	     
    | InpPi(x, y, p) ->
       begin
	 match listed x  with 
	| (-3, -3, _)  ->
	   refEnv := Env.add x ((0, 0, [])) !refEnv; 
	   addConstrList
	     [sprintf ";; channel type %s" x ;
	      "(declare-const "^ x ^" (Chantype))"];
	   addConstrList
	     (ignoreFun
		([z3_4
		   "(assert (! (and (>= (i %s) 0) (<= (i %s) 2) (>= (o %s) 0) (<= (o %s) 2))%s))" x x x x]))
	| _ -> ()
       end ;
       
       let _ = addLinearInput x in
       begin
	 match listed y with
	| (-3, -3, _)  ->
	   begin
	     refEnv := Env.add y ((0, 0, [])) !refEnv; 
	     addConstrList
	       [sprintf ";; channel type %s" y ;
		sprintf "(declare-const %s (Chantype))" y];
	     addConstrList
	       (ignoreFun
		  ([z3_5
		     "(assert (! (=> (isChannel %s) (and (>= (i %s) 0) (<= (i %s) 2) (>= (o %s) 0) (<= (o %s) 2) ))%s))" y y y y y]))
	   end
	| _  -> raise (AlphaRename y)
       end ;
       
       let tmp = ref "" and
	   orAllowedIds = ref "" in 
       let cat id = 
	 tmp := sprintf "%s (= (id %s) %d)" !tmp y id in
       Allow.iter cat allowedCasts ;
       orAllowedIds := z3_2 " (or %s (= (id  %s) 0)))%s))" !tmp y ;
       addConstrList
	 [sprintf ";; input mode of %s" x ;
	  z3_1 "(assert (! (isChannel %s)%s))" x ;
	  z3_2 "(assert (! (=> (= (scope %s) dynamic) (= (id %s) 0))%s))" x x ;
	  sprintf ";; variable of %s" x; 
	  z3_2 "(assert (! (=> (= (scope %s) dynamic) (= (id %s) 0))%s))" y y ;
	  sprintf
	    "(assert (! (and (equal (payload %s) %s) %s" x y !orAllowedIds
	 ] ;
       
       begin
	 match deadlock_detection with
	 | true ->
	   let orders = ref [] in
	   iterPrefix orders prefix x;
	   addConstrList
	     (ignoreFun
		([";;deadlock prevention on linear channels"]@(!orders)@
		   [";;linear binding in input";
		    z3_5
		      "(assert (! (=> (isLinear %s) (and (= (ord %s) (ord (object %s))) (= (object %s) (object (object %s)))))%s))"
		      x y x y x]))
	| _ -> ()
       end;
       
       let q = infer_ (Prefix.add x prefix) allowedCasts p in
       InpPi(x, y, q)
	    
    (*| OutPi(_, Top, _) not yet supported, will supply basis for base values *)    | _ -> raise NotSupported in
  
  infer_ prefix allowedCasts p

(****************** RUN  *******************)
	 
(** [execInfer lsprocess linear_list boundVars freeNames deadlock_detection] 
    transform LSpi process in pi process with Pi.toProcessPi, 
    call infer, collect constraints of LS process with (optional) 
    linear channels, write constraints in Param.constraintFile, 
    runs Param.z3Path, and write answer in Param.modelFile *)
let execInfer sprocess linear_list boundVars freeNames deadlock_detection =
  let testProcess = toProcessPi sprocess in
  let linearMode = isNotEmpty linear_list in
  let ignoreFun =
    match linearMode with
    | true -> fun x -> x
    | false -> fun x -> [] in
  let _ =
    infer (Prefix.add "dummy" Prefix.empty) Allow.empty testProcess ignoreFun deadlock_detection in
  (match linearMode with
   | true -> addLinearChannels linear_list
   | _ -> ());
  if linearMode
  then
    begin
      addConstr ";; Constraints" ;
      Env.iter  (constrEnv boundVars deadlock_detection) !refEnv;
      if deadlock_detection then
	(*addConstr (notFullEnv freeNames "" linear_list)*)
	 addConstr (linearBound (listDiff linear_list freeNames));
    end ;
  addConstrList 
    [";;SATISFIABILITY" ;
     "(push)";
     "(check-sat)";
     "(get-model)"; 
     "(get-unsat-core)"] ;  
  (*Write z3 file*) 
  let z3file = open_out constraintFile in
  writeList (formatter_of_out_channel z3file) !refConstraints ;     
  close_out z3file; 
  (*Run z3*)
  if Sys.command(z3Path ^ " " ^ Param.constraintFile ^ " > " ^ modelFile)= exitCode
  then
    exit 1
    
exception EOF of string
		   
(** Extraction of identifiers from Z3 model *)
let extract_id s =
  let id = ref "" and
      i = ref 0 and
      c = ref s.[0] in
  while (!c != ' ') do
    id := sprintf "%s%s" !id (String.make 1 !c);
    i := !i +1;	  
    c := s.[!i]
  done; 
  !id
   
(** Extraction of variables from Z3 model *)
let extractVar s = 
  if (String.length s > 12 && (String.sub s 3 10) = "define-fun")
  then
    let lx =
      String.split_on_char '(' (String.sub s 14 ((String.length s)-14)) in 
    extract_id (List.hd lx)
  else
    "" 

(** Multi-line parsing *)
let multi_line chan =
  let count symbol s =
    let res = ref 0 in
    let _ =
      String.map
	(fun a -> if a = symbol then (res := !res +1; a) else a) s in 
    !res in
  let moreLines = ref true and
      s = ref "" in
  while !moreLines do
    s := sprintf "%s %s" !s (input_line chan);
    moreLines :=
      (count '(' !s) !=  ((count ')' !s)-1)
  done; 
  !s
   
(** Populate list of entries of the form (varName, type) 
    with t provided by GoValues.extractType *)
let rec populateGivenList fixedList list chan  =
  let s = input_line chan in 
  if String.trim(s) = ")" then
    list
  else
    let varName =  extractVar s in 
    match List.mem varName fixedList with
    | true ->
       let s = (multi_line chan) in
       (* TO BE ADDED : read/write polarities *)
       let t = extractType s in
       let app = [(varName, t)] in
       populateGivenList fixedList (list@app) chan
    |_ -> populateGivenList fixedList list chan


(** UnSat exception raised when Z3 fails *)
exception UnSat of string
		     
(** Z3 model file to list, raise UnSat if unsat *)
let z3ToList fnames file  =
  let z3ToList_  f = 
    let unused_model_line = ref "" and
	list = ref [] in
    try
      let chan  = open_in file in
      if ((input_line chan) = "unsat") then
	let _ = input_line chan in
	raise
	  (UnSat (input_line chan)) 
      else
	(unused_model_line := input_line chan; 
	 list := f [] chan);
      close_in chan; 
      !list
    with
      End_of_file -> raise (EOF file)
  in
  z3ToList_ (populateGivenList fnames) 
