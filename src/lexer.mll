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

{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let id = letter+(digit*letter*)*

rule read = 
  parse
  | white   { read lexbuf }
  | "("     { LEFT_PAREN }
  | ")"     { RIGHT_PAREN }
  | "{"     { LEFT_BRACE }
  | "}"     { RIGHT_BRACE }
  | "<"     { LEFT_ANGLE }	 
  | ">"     { RIGHT_ANGLE }
  | "["     { LEFT_SQUARE }	 
  | "]"     { RIGHT_SQUARE }
  | "let"   { LET }
  | "="     { EQUALS }
  | "in"    { IN }
  | "!"     { SEND }
  | "?"     { RECEIVE }
  | "new"   { NEW }
  | "hide"  { HIDE}
  | "|"     { PARALLEL }
  | "*"     { RECURSION }
  | "print" { PRINT }
  | "."     { DOT }
  | ","     { COMMA }
  | "0"     { ZERO }
  | id      { ID (Lexing.lexeme lexbuf) }
  | eof     { EOF }
