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

(** Module to generate GoValues, it is not included in the build *)

open Format

let outputFile = "goValues.ml"

let maxOrder = 100

let license =
  ["(**************************************************************************)";
   "(*                               gopi                                     *)";
   "(*                                                                        *)";
   "(*  Copyright 2019 Marco Giunti. All rights reserved. This file is        *)";
   "(*  distributed under the terms of the GNU Public License version 3.0.    *)";
   "(*  This software is distributed in the hope that it will be useful,      *)";
   "(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)";
   "(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)";
   "(*  GNU General Public License for more details.                          *)";
   "(*                                                                        *)";
   "(**************************************************************************)"]

let staticCode =
  ["(** Max order of communication is set to 100 *)";
   "let maxOrder = 100\n";
   "(** Max order of actual process *)";
   "let max_order_test_process = ref 0\n";
   "";
   "(** Order of channel given list of tokens extracted from Z3 model *)";
   "let getChannelOrder l =";
   "  let rec getChannelOrderR acc  = function";
   "    | [] ->"; 
   "       let mo = !max_order_test_process in";
   "       max_order_test_process := max acc mo;";
   "       acc";
   "    | h::t ->";
   "       if h = \"(channel\" then";
   "         getChannelOrderR (acc + 1) t";
   "       else";
   "         getChannelOrderR acc t";
   "  in getChannelOrderR 0 l";
   "";
   "exception MaxOrderExceeded of string";
   ""]

let rec genGoValues acc = function
  | -1 ->
     let finalList =
       acc@["  | Base";
	    "type goValue ="]
     in List.rev finalList 
  | n -> genGoValues (acc@[sprintf "  | Chan%d" n]) (n - 1)

let rec genToString acc = function
  | -1 ->
     let finalList =
       acc @
	 ["  | Base -> \"base\" ";
	  "let toString = function"]
     in List.rev finalList 
  | n ->
     genToString
       (acc@[sprintf "  | Chan%d -> \"chan%d\"" n n]) (n - 1)

let rec genToInt acc = function
  | -1 ->
     let finalList =
       acc@
	 ["  | Base -> -1 ";
	  "let toInt = function"]
     in List.rev finalList 
  | n ->
     genToInt
       (acc@[sprintf "  | Chan%d -> %d" n n]) (n - 1)

let rec genExtractType acc = function
  | 0 ->
     let finalList =
       ["  | _ -> raise (MaxOrderExceeded s)"]
       @acc
       @(List.rev ["let extractType s =";
		   "  let split_list =  (String.split_on_char ' ' s) in";
		   "  let order = getChannelOrder split_list in";
		   "  match order with";
		   "  | 0 -> Base"])
     in List.rev finalList
  | n ->
     genExtractType 
       (acc@[sprintf "  | %d -> Chan%d" n (n - 1)]) (n-1)

let rec genReservedKeywords acc = function
  | -1 ->
     let finalList =
       "]" ::
	 acc @
	 ["    \"queueBase\"; ";
	  "    \"basePair\"; ";
	  "    \"base\"; ";
	  "let reservedKeywords = ["
	 ]
     in List.rev finalList 
  | n ->
     genReservedKeywords
       (acc@[sprintf "    \"queueChan%d\";" n ;
	     sprintf "    \"chan%dPair\";" n ;
	     sprintf "    \"chan%d\";" n]) (n - 1)      
       
let () =
  let rec writeMLcode p fmt = function
  |  [] -> fprintf fmt "@."
  |  h::t -> fprintf fmt "@[%s@ %a@]" h (writeMLcode p) t in
  let valuesFile = open_out outputFile in
  let code =
    license
    @["\n(** Types of Go and marshalling / unmarshalling functions. Module generated\n    by GenerateGoValues *)\n"]
    @staticCode
    @(genGoValues [] maxOrder)@[""]
    @(genToString [] maxOrder)@[""]
    @(genToInt [] maxOrder)@[""]
    @(genExtractType [] maxOrder)@[""]
    @(genReservedKeywords [] maxOrder) in
  writeMLcode pp_print_string (formatter_of_out_channel valuesFile) code ;
  close_out valuesFile
