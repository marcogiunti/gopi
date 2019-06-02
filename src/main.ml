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

(** Main is the top-level: receives arguments, parse process file and calls gopi *)

open Arg
open Ast
open ListOps
open Gopi
open Format
open Param

(** Multi step of evaluation. *)
let rec stepP = function
  | Set(l, p)
    -> Set(l, stepP p)
  | Let(x, p, q)
    -> stepP (substP p x q)
  | Inp(x,y,p) ->
     Inp(x,y,stepP p)
  | Out(x,v,p) ->
     Out(x,v, stepP p) 
  | Par(p,q) ->
     Par ((stepP p), (stepP q))
  | Repl(p) ->
     Repl(stepP p) 
  | New(x, p) ->
     New(x, stepP p) 
  | Hide (x,  p) ->
     Hide(x, stepP p)
  | VarP(p) ->
     failwith "Unbound process variable"
  | q -> q

(** Reference to parsed process *)
let processRef = ref Zero
let processLine = ref ""

(** Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(** Extension : Extract a value from an ast node.
   Raises Failure if the argument is a node containing a value. *)
let extract_value = function
  | Int i -> i
  | _ -> failwith "Not a value"
		  
(** Interpret an expression *)
let interpP e =
  e |> parse |> stepP 
		  
(** Stream of channel *)
let line_stream_of_channel channel =
  Stream.from
    (fun _ ->
     try Some (input_line channel) with End_of_file -> None)

(** Process a line *)
let process_line line =
  if String.length line > 0
  then
    if ((String.sub line 0 1) <> "#")
    then processLine:= !processLine ^ " " ^ line
    else ()
  else ()

(** Process stream of lines *)
let process_lines lines =
  Stream.iter process_line lines;
  processRef := interpP !processLine
			
(** Process the lines of a file *)
let parseFile file =
  let in_channel = open_in file in
  try
    process_lines (line_stream_of_channel in_channel);
    close_in in_channel
  with e ->
    close_in in_channel;
    raise e

(** Arg option -debug : default is false *)	  
let debug = ref false
(** Arg option -tc : default is false *)	  
let type_check_only = ref false
(** Arg option -c : default is false *)	  
let compile_only = ref false
(** Arg option -pp : default is false *)	  
let print_process = ref false
(** Arg option -dd-off : default is true *)	  
let deadlock_detection = ref true
(** Arg option -cat n : default is 0 *)
let catalyzer = ref 0
(** Arg option -pc : default is false *)
let print_catalyzer = ref false
(** Arg option -af : default is true *)
let alpha_conversion = ref true
		  
(** Arg options *)
let options = [
    ("-debug", Arg.Set debug, "Enable debug printing") ;
    ("-d", Arg.Set debug, "Enable debug printing") ;
    ("-tc", Arg.Set type_check_only, "Type check only -- Type check process without generating go file");
    ("-c", Arg.Set compile_only, "Compile only -- Generates the go file without run it ") ;
    ("-print-process", Arg.Set print_process, "Print LSpi specification");
    ("-pp", Arg.Set print_process, "Print LSpi specification");
    ("-dd-off", Arg.Clear deadlock_detection, "Deactivate static deadlock detection on free linear channels");
    ("-cat", Arg.Set_int catalyzer, "Enable catalyzer of order n");
    ("-print-cat", Arg.Set print_catalyzer, "Print catalyzer (if process type checks, otherwise use -d)");
    ("-pc", Arg.Set print_catalyzer, "Print catalyzer (if process type checks, otherwise use -d)");
    ("-alpha-off", Arg.Clear alpha_conversion, "Disable alpha conversion");
    ("-af", Arg.Clear alpha_conversion, "Disable alpha conversion")]

(** Wrapper of gopi called by Arg.parse *)
let wrapper fileName =
    try
      parseFile (fileName) ; 
      gopi
	!processRef
	!debug !type_check_only !compile_only
	!print_process
	!deadlock_detection
	!catalyzer !print_catalyzer
	!alpha_conversion
    with
    | Parser.Error ->
       printf
	 "PARSER ERROR: verify the syntax of processes by executing gopi. Sanity check: each commented line must start with the symbol # (in column 0 of the line)\n"
    | e ->
       let msg = Printexc.to_string e
       and stack = Printexc.get_backtrace () in
       Printf.eprintf "There was an error: %s%s\n" msg stack

(** Main *)
let run_main syntax usage_string =
  Random.self_init ();
  print_endline "****************************** GOPI ******************************";
  match Array.length Sys.argv  with
  | 1 ->
     Arg.usage options (string_of_list syntax ^ usage_string)
  | _ ->
     Arg.parse options wrapper usage_string

let lspi_syntax = 
    ["SYNTAX OF LSPI PROCESSES";
     " P = new x {P} | hide x [P] | P|Q | let id = P in Q | (P) ";
     "   | NameP | 0 | x!y | x!y.P | x?y | x?y.P | *P | print x";
     "   | <a,b,...,z> P /*linear declaration (not-binding)*/";
     "COMMENTS: # (FIRST CHAR OF LINE)"]
    
let usage_string =
  "See man ./gopi.man for the synopsis.\n OPTIONS: "
    
let _ =
  run_main lspi_syntax usage_string
