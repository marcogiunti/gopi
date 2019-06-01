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

(** Global Parameters *)

(** Bound for randomly generated variables in Ast *)
let rANDOMLIMIT = 10000  

(** Name of Z3 constraint file *)
let constraintFile = "types.z3"
		    
(** Name of Z3 model file *)
let modelFile = "core.z3"

(** Timeout of dequeue operations in Go channel servers *)
let timeout = 500 (*MILLISECONDS*)

(** The indentation space in Go code *)       
let iNDENT = "    "
		  
(** Name of Go file *)
let goFile = "gopiProcess.go"

(** Path of z3 *)
let z3Path = "z3"

(** Path of go *)
let goPath = "go"

(** Not found exit code *)
let exitCode = 127	       
