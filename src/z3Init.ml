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

(** SMT-LIB definitions of pi calculus 
    types and utility functions*)
let init =
  [";; MBQI - Model-based Quantifier Instantiation";
   "(set-option :smt.mbqi true)";
   ";; UNSAT CORE";
   "(set-option :produce-unsat-cores true)";
   ";; DATATYPES" ;
   "(declare-datatypes () ((Scope static dynamic)))";
   "(declare-datatypes () (";
   "  (Chantype top";
   "    (channel (scope Scope) (payload Chantype) (id Int)))))"]
  @[";; FUNCTIONS";
    ";; isChannel";
    "(define-fun isChannel ((s Chantype)) Bool";
    " (not (= s top)))"]
  @["(declare-fun object ((Chantype)) Chantype)";
    "(declare-fun isReplicated ((Chantype)) Bool)";
    "(define-fun equal ((c Chantype) (d Chantype)) Bool";
    "  (= c d))"] 

(** Z3 Header (Linear mode): SMT-LIB definitions of pi calculus 
    types and utility functions*)  
let linear_init =
  [";; MBQI - Model-based Quantifier Instantiation";
   "(set-option :smt.mbqi true)";
   ";; UNSAT CORE";
   "(set-option :produce-unsat-cores true)";
   ";; DATATYPES" ;
   "(declare-datatypes () ((Scope static dynamic)))";
   ";; i/o capabilities: 2 is used, 1 is used once, 0 is unused";
   "(declare-datatypes () (";
   "  (Chantype top";
   "    (channel (scope Scope) (payload Chantype) (id Int) (i Int) (o Int) (ord Int)))))"]
  @[";; CONSTANTS";
    "(declare-const dummy Chantype)";
    "(assert (! (= (ord dummy) 0) :named Adummy))"]
  @[";; FUNCTIONS";
    ";; isChannel";
    "(define-fun isChannel ((s Chantype)) Bool";
    " (not (= s top)))"]
  @["(declare-fun object ((Chantype)) Chantype)";
    "(declare-fun isReplicated ((Chantype)) Bool)";
    ";; linearity"; 
    "(declare-const linear_constraint (Int))"; 
    "(assert (! (= linear_constraint 1):named Alinearity))";
    "(define-fun isLinear  ((c Chantype)) Bool";
    "  (and ";
    "    (not (= c top))";
    "    (and (>= (i c) 0) (<= (i c) linear_constraint) (>= (o c) 0) (<= (o c) linear_constraint))))";
    "(define-fun equal ((c Chantype) (d Chantype)) Bool";
    "  (= c d))"]
  
