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

%{
open Ast
%}

%token <string> ID
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_SQUARE
%token RIGHT_SQUARE
%token LEFT_ANGLE
%token RIGHT_ANGLE
%token LET
%token EQUALS
%token IN
%token SEND
%token RECEIVE       
%token RECURSION
%token PARALLEL
%token NEW
%token HIDE
%token PRINT
%token ZERO
%token DOT
%token COMMA
%token EOF

%nonassoc IN
%left PARALLEL
%nonassoc DOT
%nonassoc RECURSION	  

%start <Ast.lsprocess> prog

%%

array_values:
        | { [] }
        | vl = rev_values
            { List.rev vl }
        ;

rev_values:
        | v = ID { [v] }
        | vl = rev_values; COMMA; v = ID
            { v :: vl }
        ;

prog:
	| e = exprP; EOF { e }
	;

exprP:
	| x = ID; RECEIVE; y = ID; DOT; e = exprP  { Inp(x, y, e) }
	| x = ID; RECEIVE; y = ID { Inp(x, y, Zero) }
	| x = ID; SEND; y = ID; DOT; e = exprP  { Out(x, y, e) }
	| x = ID; SEND; y = ID  { Out(x, y, Zero) }
	| PRINT;  x = ID;  { Print(x) }
	| NEW; x= ID; LEFT_BRACE; e = exprP; RIGHT_BRACE { New (x, e)}
	| HIDE; x= ID; LEFT_SQUARE; e = exprP; RIGHT_SQUARE { Hide (x, e)}
	| ZERO {Zero}
	| LET; x = ID; EQUALS; e1 = exprP; IN; e2 = exprP { Let(x,e1,e2) } 
	|  e1 = exprP; PARALLEL; e2=exprP {Par(e1,e2)}
	| LEFT_PAREN; e = exprP; RIGHT_PAREN {e}
	| x = ID; { VarP(x) }
	| RECURSION ; e = exprP { Repl(e) }
	| LEFT_ANGLE; av = array_values ; RIGHT_ANGLE;  e = exprP { Set(av, e) }
	;   
