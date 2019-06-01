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

(** List operations *)

open List
open Format

let isEmpty l = List.length l = 0
       
let isNotEmpty l = List.length l > 0

let rec andList = function
  | [] -> true
  | h :: t -> h && andList t

let rec orList = function
  | [] -> false
  | h :: t -> h || orList t

let rec string_of_list = function
  | [] -> ""
  | h :: t -> h ^ "\n" ^  (string_of_list t)
	     
let printList l =
  let rec printListF f = function
  |  [] -> printf "\n";
  |  h::t -> printf "%s\n" (f h) ; printListF f t  in
  printListF (fun x -> x) l
					  
let rec writeList fmt = function
  |  [] -> ()
  |  h::t -> fprintf fmt "%s\n" h;  writeList fmt t

(** [listDiff l1 l2] returns l1 / l2 *)
let listDiff l1 l2 = 
  let rec listDiffR acc l =
    let rec remove x = function
      | h::t -> if x = h then t else h::(remove x t)
      | [] -> [] in
    function
    | [] -> acc
    | h::t ->
       if mem h l && mem h acc
       then
	 listDiffR (remove h acc) l t
       else
	 listDiffR acc l t in
  listDiffR l1 l1 l2 

(** [cap l1 l2] returns l1 \cap l2 *)
let cap l1 l2 =
  let rec capR acc l = function
    | [] -> acc
    | h::t -> 
       if mem h l
       then
	 capR (acc@[h]) l t
       else
	 capR acc l t in
  capR [] l1 l2

(** [add x l] returns l@[x] if x is not in l *)
let add x l =
  match mem x l with
  | true -> l
  | _ -> l@[x] 
 	     
(** [remove x l] remove all occurences of x in l *)
let remove x l =
  match mem x l with
  | false -> l
  | _ ->
     let rec rm x acc = function
       | [] -> acc
       | h::t ->
	  if h=x
	  then acc@t
	  else rm x (acc@[h]) t in
     rm x [] l
