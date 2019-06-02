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

(**************************************************************************)
(* The GoPi compiler                                                      *)
(*                                                                        *)
(* @author Marco Giunti                                                   *)
(* @requires Z3: https://rise4fun.com/z3                                  *)
(* @requires Go: https://golang.org   (not needed with options -tc -c)    *)
(* Type infer LSpi processes with static [1,2] and linear [3] channels    *)
(* Generates Go code and run it                                           *)
(*                                                                        *)
(* Please report BUGS to: marco dot giunti at gmail dot com               *)
(*                                                                        *)
(* REFERENCES                                                             *)
(* [1] Marco Giunti, Catuscia Palamidessi, Frank D. Valencia:             *)
(* Hide and New in the Pi-Calculus. EXPRESS/SOS 2012: 65-79               *)
(* [2] 	Marco Giunti: Static Semantics of Secret Channel Abstractions.    *)
(* NordSec 2014: 165-180                                                  *)
(* [3] Naoki Kobayashi, Benjamin C. Pierce, David N. Turner:              *)
(* Linearity and the pi-calculus. ACM TOPLAS 21(5): 914-947 (1999)        *) 
(**************************************************************************)

(** GoPi - the compiler of LSpi processes in Go *)


open Format
open Sys
open ListOps
open GoValues
open Ast
open Pi
open Z3Init
open Infer
open GoInit
open Param

(** The go code *)
let goCode = ref []
		 
(************* GO CODE **************)
		 
(** [addGo n s] add string to goCode with indentation n *)
let addGo n s  = let l = !goCode in
		 goCode := l@[(indentN n)^s] 

(** [addGo n l] cat list to goCode with indentation n *)
let rec addGoList n = function
  | [] -> ()
  | h::t -> addGo n h ; (addGoList n t)

(** [registerNew level (n, gv)] returns fresh entry Gamma.register(n, v) *)
let registerNew level (n,gv) =
  [(indentN level) ^"Gamma.register(\""^ n ^ "\"+ string(counter.Value(key)), \""
   ^ (stri (toInt gv))^"\")" 
   ^ (indentN level) ^ n
   ^ " := Gamma.chanOf(\"" ^ n ^ "\"+ string(counter.Value(key))).("
   ^ (toString gv) ^ ")" ^ "\n"
   ^ (indentN level) ^ " _ = " ^n
  ]
    
(** [toGoCode linear_channels p] adds go code corresponding to LSpi process 
    and returns (number of timeouts, timeout_is_on), where the timeout is
    deactivated on replication *)
let toGoCode linear_channels p =
  (* counter for done *)
  let countDone = ref 0 in
  (* counter for replies *)
  let counterReply = ref 0 in
  (* counter is active (deactived on replication) *)
  let countActive = ref true in
  let rec toGoCodeR linear_channels level = function
    | Zero -> ()
    | New (x, p)
    | Hide (x, p)  ->
       (match z3ToList [x] modelFile with
	| [(n,t)] -> 
	   addGo  level  "func() {";
	   addGo  (level+1)  "time.Sleep(time.Second/60)";
	   addGoList (level+1)  (registerNew (level+1) (n,t));
	   toGoCodeR linear_channels (level+1) (p); 
	   addGo level  "}()";
	| _ -> raise NotSupported) 
    | Print (y)
    | Out ("print", y, Zero) -> 
       addGoList level
		 ["var v value = " ^ y;
		  "switch v.(type){";
		  "    case base: fmt.Printf(\"Print %v\\n\"," ^ y  ^ ")";
		  "    default: fmt.Printf(\"Print %v with address %v\\n\", " ^ "Gamma.nameOf(" ^ y ^ "), " ^ y ^ ")";
		  "}"];
    | Out(x, y, p) ->
       let reply = x ^ "Reply" ^stri(!counterReply) in
       addGoList level
		 [reply ^ " := make(chan bool)";
		  "Gamma.queue(" ^ x ^ ", "  ^ y ^ ", " ^ reply ^ ")";
		  "_ = <- " ^ reply];
       counterReply := !counterReply+1;
       toGoCodeR linear_channels level p; 
    | Inp (x,y,p) ->
       addGoList level
		 ["Gamma.dequeue(" ^ x ^ ")"; 
		  y ^  " := <- " ^ x;
		  "fmt.Printf(\"Retrieved %v from %v\\n\", Gamma.nameOf("
		  ^ y ^ "), Gamma.nameOf(" ^x ^"))"];
       toGoCodeR linear_channels level p; 
    | Repl(p) ->
       let rec equivZero = function
	 | Zero | Parl([]) -> true
	 | Repl(p) | New(_, p) | Hide(_,p) -> equivZero p
	 | Parl(h::t) ->
	    (equivZero h) && (equivZero (Parl t))
	 | _ -> false in
       if not (equivZero p) then
	 countActive := false;
       addGoList (level) ["key := RandStringRunes(32)";
			  "fmt.Printf(\"KEY: %s\\n\", key)"];
       addGoList (level) ["for{"];
       
       addGoList (level+1) ["time.Sleep(time.Millisecond*"^stri(timeout)^")"];
       addGoList (level+1) ["counter.Inc(key)"];
       toGoCodeR linear_channels (level+1) p; 
       addGoList (level) ["}"]
    | Parl l ->
       (match l with
	| [] -> ()
	| h::t ->
	   countDone := !countDone +1;
	   addGoList  level  ["//Thread " ^(stri !countDone);
			      "go func() {"]; 
	   toGoCodeR linear_channels (level+1) h; 
	   addGoList (level+1) ["done <- true"];
	   addGoList  level  ["}()"]; 
	   toGoCodeR linear_channels level (Parl t))
    | _ -> raise NotSupported
  in toGoCodeR linear_channels 1 p;
     (!countDone, !countActive)

(** Returns go code containing the type declarations of channels *) 
let rec goTypes acc = function
  | -1 -> List.rev acc   
  | 0 ->
     goTypes (acc@List.rev
		    ["type chan0 chan base";
		     "type chan0Pair struct{"; 
		     "    ch chan0";
		     "    replych chan bool";
		     "}"]) (-1)
  | n ->
     goTypes (acc@List.rev
		    ["type chan" ^ (stri n)
		     ^ " chan chan" ^ (stri (n-1));
		     "type chan" ^ (stri n) ^ "Pair struct{";
		     "    ch chan"^ (stri n);
		     "    replych chan bool";
		     "}"]) (n-1) 
	     
(** Returns the go code representing the type declaration of channel servers *)
let rec goTypeEnv acc = function
  | -1 ->
     ["";
      "type typeEnv struct{"; 
      "       ord struct{";
      "	       toStr map[base]string";
      "	       fromStr map[string]base";
      "      }"]
     @acc
     @["";
       "}"]
  | 0 ->
     goTypeEnv 
       (["      ord0 struct{";
	 "		toStr map[chan0]string";
	 "		fromStr map[string]chan0";
	 "		queue map[chan0]queueBase";
	 "		dequeue map[chan0]func()";
	 "                mux sync.Mutex"; 
	 "      }"]@acc) (-1)
  | n ->
     goTypeEnv
       (["      ord"^(stri n) ^ " struct{";
	 "		toStr map[chan"^(stri n) ^ "]string";
	 "		fromStr map[string]chan"^(stri n) ^ "";
	 "		queue map[chan"^(stri n) ^ "]queueChan"^(stri (n-1)) ^ "";
	 "		dequeue map[chan"^(stri n) ^ "]func()";
	 "                mux sync.Mutex";
	 "      }"]@acc) (n-1)



(** Returns the go code of func (t *typeEnv) ignores(s string) error *)
let goIgnore n =
  let rec goIgnore_ acc fix = function
    | -1 ->
       ["func (t *typeEnv) ignores(s string) error{"; 
	" 	_,ex  := t.ord.fromStr[s]"]
       @acc
       @fix
    | n ->
       goIgnore_
	 (acc@["        t.ord"^(stri n)^".mux.Lock()";
	       "        _,ex"^(stri n)^"   := t.ord"^(stri n)^".fromStr[s]";
	       "        t.ord"^(stri n)^".mux.Unlock()"]) fix (n-1) in
  let rec exor acc = function
    | -1 ->
       "ex "^acc
    | n ->
       exor ("|| ex"^(stri n)^"  "^acc) (n-1) in 
  goIgnore_
    [] ["        if (" ^ (exor "" n) ^ " ){";
	"     	     return errors.New(\"Binding Error: \" + s +";
	"                  \"already defined. Please rename \" + s)";
	"         }"; 
	"         return nil   ";
	"}\n"] n  
    
(** Returns the go code containing the body of channel servers *)
let rec goRegister acc = function
  | -1 ->
     ["func (t *typeEnv) register(name string, nameType string) error{";
      "    if err := t.ignores(name); err != nil{";
      "        return err";
      "    }";
      "    for n :=0 ; n<10; n++ {";
      "        if strings.HasPrefix(name,strconv.Itoa(n)){";
      "            return errors.New(\"Syntax Error: identifier starts with digit\")";
      "        }";
      "    }";
      "    var code int";
      "    var err error";
      "    if code,err =strconv.Atoi(nameType); err != nil{";
      "        return errors.New(\"Conversion error\")";
      "    }";
      "    switch code {";
      "    // variables, not allowed at init";
      "    case -1 : ";
      "        var b base";
      "	       b = b.toBase(name)";
      "        t.ord.toStr[b] = name ";
      "        t.ord.fromStr[name] = b ";
      "        return nil"]
     @acc
     @["\n    default: return errors.New(\"Syntax error: invalid type encoding of \" + name + \" of type \" + nameType  )";
       "    }";
       "}\n"]
  | n ->
     goRegister
       (acc@[
	   "    case "^(stri n) ^ " : ";
	   "        c := make(chan"^(stri n) ^ ")";
	   "        t.ord"^(stri n) ^ ".mux.Lock()";
	   "        t.ord"^(stri n) ^ ".toStr[c] = name";
	   "        t.ord"^(stri n) ^ ".fromStr[name] = c";
	   "        defer t.ord"^(stri n) ^ ".mux.Unlock()";
	   "        t.ord"^(stri n) ^ ".dequeue[c] =  func (){";
	   "            timeout := make(chan bool)";
	   "            go func(){";
	   "                time.Sleep(time.Millisecond*"^stri(timeout)^")";
	   "                timeout <- true";
	   "            }()";
	   "            t.ord"^(stri n) ^ ".mux.Lock()";
	   "            qb := t.ord"^(stri n) ^ ".queue[c]";
	   "            size := len(qb)";
	   "            for size == 0 {";
	   "                select {";
	   "                case <- timeout:" ;
	   "                    fmt.Println(\"TIMEOUT\")";
	   "                    size = 1";
	   "                default:";
	   "                    t.ord"^(stri n) ^ ".mux.Unlock()"; 
	   "                    time.Sleep(time.Second/60)	";
	   "                    fmt.Printf(\"Waiting for value on %v\\n\", Gamma.nameOf(c))"; 
	   "                    t.ord"^(stri n) ^ ".mux.Lock()"; 
	   "                    qb = t.ord"^(stri n) ^ ".queue[c]";
	   "                    size = len(qb)";
	   "                }"; 
	   "            }";
	   "            time.Sleep(time.Millisecond*50)";
	   "            qb.shuffle()"; 
	   "            if (len(qb) > 0) {";
	   "                last := qb[len(qb)-1]"; 
	   "                t.ord"^(stri n) ^ ".queue[c] = qb[:len(qb)-1]"; 
	   "                go func(){";
	   "                    c <- last.ch";
	   "                    last.replych <- true"; 
	   "                }()";
	   "            }";
	   "            t.ord"^(stri n) ^ ".mux.Unlock()";
	   "        }";
	   "        return nil"]) (n-1)

(** Returns the goQueueChan type  *)
let rec goQueueChan acc = function
  | -1 ->
     ["";
      "type queueValue interface{";
      "	shuffle()";
      "}";
      ""; 
      "type queueBase []basePair";
      ""] @ acc
  | n ->
     goQueueChan
       (["type queueChan"^(stri n)^ " []chan" ^(stri n) ^"Pair";
	 ""] @ acc)  (n - 1)       
       
(** Returns the go code for randomization inside channel servers *)
let rec goShuffle acc =
  let f x =
    ["func (arr " ^ x  ^ ") shuffle() {";
     " 	for i := len(arr) - 1; i > 0; i-- {";
     " 		j := rand.Intn(i)";
     " 		arr[i], arr[j] = arr[j], arr[i]";
     " 	}";
     "}\n"] in 
  function
  | -1 ->
     f("queueBase") @ acc
  | n ->
     goShuffle ( (f ("queueChan"^(stri n)) ) @ acc) ( n - 1)

(** Returns the go code of func (t *typeEnv) queue(output value, payload value,
    replyCh chan bool) error. The code implements lock/unlock of mutex inside 
    channel servers *)
let rec goQueue acc = function
  | -1 -> []
  | 0 ->
     ["func (t *typeEnv) queue(output value, payload value, replyCh chan bool) error{";
      "    time.Sleep(time.Second/60)";
      "";
      "    switch output.(type) {";
      "    case chan0:";
      "        t.ord0.mux.Lock()";
      "        slice0 := t.ord0.queue[output.(chan0)]"; 
      "        slice0 = append(slice0, basePair{payload.(base), replyCh})";
      "        t.ord0.queue[output.(chan0)] = slice0";
      "        t.ord0.mux.Unlock()"]
     @acc
     @["";
       "        }";
       "";
       "    return nil";
       "}\n"]
  | n ->
     goQueue
       (["    case chan"^(stri n)^ ":";
	 "        t.ord"^(stri n)^".mux.Lock()";
	 "        slice0 := t.ord"^(stri n)^".queue[output.(chan"^(stri n)^")]"; 
	 "        slice0 = append(slice0,  chan"^(stri (n-1))^ "Pair{payload.(chan"^(stri (n-1))^"), replyCh})";
	 "        t.ord"^(stri n)^".queue[output.(chan"^(stri n)^")] = slice0";
	 "        t.ord"^(stri n)^".mux.Unlock()";]
	@acc) (n-1)

(** returns the code for dequeeing (corresponds to LS input prefix) *)
let rec goDequeue acc = function
  | -1 ->
     ["func (t *typeEnv) dequeue(input value) error{";
      "    time.Sleep(time.Second/60)";
      "    switch input.(type) {"]
     @acc
     @["    }";
       "    return nil"; 
       "}\n"]
  | n ->
     goDequeue
       (["    case chan"^(stri n)^":"; 
	 "        t.ord"^(stri n)^".mux.Lock()"; 
	 "        f := t.ord"^(stri n)^".dequeue[input.(chan"^(stri n)^")]"; 
	 "        t.ord"^(stri n)^".mux.Unlock()"; 
	 "        f()"]	  
	@acc) (n-1)
       
(** Go code of func (t *typeEnv) chanOf(s string) value *)
let rec goChanOf acc = function
  | -1 ->
     ["func (t *typeEnv) chanOf(s string) value{";
      "    if ch, ex := t.ord.fromStr[s]; ex{";
      "        return ch";
      "    }"]
     @acc
     @["";
       "    return nil";
       "}\n"]
  | n -> 
     goChanOf
       (["    t.ord"^(stri n)^".mux.Lock()";
	 "    defer t.ord"^(stri n)^".mux.Unlock()";
	 "    if ch, ex := t.ord"^(stri n)^".fromStr[s]; ex {";
	 "        return ch"; 
	 "    }"]@acc) (n-1) 

(** Go code of func (t *typeEnv) nameOf(c value) string *)
let rec goNameOf acc = function
  | -1 ->
     ["func (t *typeEnv) nameOf(c value) string{"; 
      "    switch c.(type) {"; 
      "    case base: ";
      "        return c.(base).toString()";
      "    "]
     @acc
     @["\n    }";
       "    return \"not found\"";
       "\n}"]
  | n ->
     goNameOf
       (["    case chan"^(stri n)^": ";
	 "        t.ord"^(stri n)^".mux.Lock()";
	 "        defer t.ord"^(stri n)^".mux.Unlock()";
	 "        return c.(chan"^(stri n)^").toString()"]
	@acc) (n-1) 

(** Go code of func (c chan_n) toString() string *)
let rec goToString acc = function
  | -1 ->
     ["\nfunc (b base) toString() string{";
      "        return string(b)";
      "}\n"]@acc
  | n ->
     goToString
       (acc@
	  ["func (c chan"^(stri n)^") toString() string{";
	   "        return Gamma.ord"^(stri n)^".toStr[c]";
	   "}\n"]) (n-1) 


(*********************** MAIN ***************************)

(** [initializeGoCode mo] returns header of the Go file
    with maximum order mo *)
let initializeGoCode mo =
  GoInit.license 
  @ GoInit.bugs
  @ ["/* AUTOMATICALLY GENERATED BY THE GOPI TOOL                               */";
     "package main";
     "";
     "import (\"errors\"";
     "        \"fmt\"";
     "        \"math/rand\"";
     "        _ \"os\"";
     "        \"strconv\"";
     "        \"strings\"";
     "        \"sync\"";
     "        \"time\""; 
     ")";
     "";"";
     "/********************************* TYPES ***********************************/";
     "var Gamma typeEnv";
     "";
     "type value interface{";
     "    toString() string"; 
     "}";
     "";
     "type base string";
     "type basePair struct{";
     "    ch base";
     "    replych chan bool";
     "}"]
  @ (goTypes [] mo)
  @ (goQueueChan [] mo)
  @ (goTypeEnv [] mo)
  @ ["/********************************* METHODS **********************************/"]
  @ (goShuffle [] mo)
  @ (goIgnore mo)
  @ (goRegister [] mo)
  @ (goDequeue [] mo)
  @ (goQueue [] mo)
  @ (goChanOf [] mo)
  @ (goNameOf [] mo)
  @ (goToString [] mo)
  @ ["func (b base) toBase(s string) base{";
     "	        return b.comp(base(s))";
     "}\n";
     "func (b1 base) comp(b2 base) base{";
     " 	return b1+b2";
     "}"]
  @ ["\n/********************************* AUX ***********************************/";
     "var letterRunes = []rune(\"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\")\n";
     "func RandStringRunes(n int) string {";
     "   b := make([]rune, n)";
     "   for i := range b {";
     "      b[i] = letterRunes[rand.Intn(len(letterRunes))]";
     "   }";
     "   return string(b)";
     "}\n"]
  @ ["type SafeCounter struct {";
     "   v   map[string]int";
     "   mux sync.Mutex";
     "}\n"]
  @ ["// Inc increments the counter for the given key.";
     "func (c *SafeCounter) Inc(key string) {";
     "    c.mux.Lock()";
     "    c.v[key]++";
     "    c.mux.Unlock()";
     "}\n";
     "// Value returns the current value of the counter for the given key.";
     "func (c *SafeCounter) Value(key string) int {";
     "    c.mux.Lock()";
     "    defer c.mux.Unlock()";
     "    return c.v[key]";
     "}\n";
     "var counter SafeCounter";
     "var key string";
     "var done chan bool"]

      
(************************ GOPI **************************)
      
(** Secret processes must consider full linear channels.
    The -nf option is not allowed  *)
exception SecretAndNotFull
(** All declared linear channels must occur in the process  *)
exception LinearNotInProcess
(** The process is using reserved keyword *)
exception ReservedKeyword of string
			       
(** GOPI - type checks and executes LS process
    $1 process
    $2 -- $9 options 
    Options are described by $man ./gopi.man, or $./gopi: 
    $2 -debug -d Show SML  constraints that cause type check to fail
    $3 -tc Type check only
    $4 -c Compile only: type  checks  the process and generate the go file
    $5 -pp Print process
    $6 -not-full -nf Deactivate static deadlock detetection on linear channels
    $7 -cat n Generates catalyzer of order n
    $8 -pc Print catalyzer
    $9 -af Disable alpha conversion
 *)
let gopi
      lSprocess
      debugMode type_check_only compile_only
      print_process
      deadlock_detection
      order_of_catalyzer print_catalyzer
      alpha_conversion =
  let rec string_of_list = function
    | [] -> ""
    | h::t -> h ^ " " ^(string_of_list t) in
  let rec unique_of_list acc = function 
    | [] -> acc
    | h::t -> if List.mem h acc
	      then unique_of_list acc t
	      else unique_of_list (acc@[h]) t in
  let rec hasDup = function
    | [] -> false
    | h :: t -> List.mem h t || hasDup t in
  let tmp_linear_channels, setFreeLSProcess = removeSet lSprocess in 
  let linear_channels = unique_of_list [] tmp_linear_channels in
  let conversion = ref (fun x -> x) in
  let linear_bound_vars = (cap tmp_linear_channels (boundVars setFreeLSProcess)) in
  (* CLASH ON LINEAR CHANNELS -- IMMEDIATE EXIT *)
  if isNotEmpty linear_bound_vars
  then
    begin
      print_endline
	("******************************************************************\n"
	 ^ "Process has name clash: alpha rename \""
	 ^ (string_of_list linear_bound_vars) ^ "\"\n"
	 ^ "******************************************************************");
      exit 0
    end  ;
  
  (* NO CLASH ON LINEAR CHANNELS *)
  let bn = boundChannels setFreeLSProcess in
  let fn = freeNames setFreeLSProcess in
  if (isNotEmpty (cap fn bn) || hasDup bn) && alpha_conversion
  then
    conversion :=  alphaConvert;
  
  (* runProcess is both passed to execInfer to generate z3 constraints
     and toGoCode to generate Go code. The flat structure is exploited 
     by the Go port *)
  let runProcess = (!conversion) (flatParl (toParlProcess setFreeLSProcess)) in
  let catalyzerProcess =
    if order_of_catalyzer > 0
    then
      (* Catalyzer is generated as process and then mapped into Ast.lsprocess *) 
      toLSpiProcess (catalyzer (toProcessPi runProcess) linear_channels order_of_catalyzer)
    else
      Zero in
  let bv = boundVars runProcess in    
  let fn = freeNames runProcess in
  (* Linear mode *)
  let linearMode = isNotEmpty linear_channels in
  let ignoreFun = ref (fun x -> []) in
  
  (* Setting options for linear channels *) 
  begin
    match linearMode with
    | true ->
       begin
	 ignoreFun := (fun (x:string list) -> x);
	 print_endline
	   ("Symbolic linear channels: " ^ (string_of_list linear_channels));

	 begin
	   match deadlock_detection with
	   | false -> 
	      let freeAndLinear = cap fn linear_channels  in
	      begin
		match isNotEmpty freeAndLinear  with
		| true ->
		   print_endline
		     ("Deadlock detection on " ^ (string_of_list freeAndLinear) ^ "is off")
		| _ ->
		   print_endline
		     ("Not-Full flag ignored : all linear channels are bound")
	      end
	   | true -> print_endline
		       ("Deadlock detection on " ^ (string_of_list linear_channels) ^ "is on")
	 end ;
	 
	 addConstrList Z3Init.linear_init
       end
	 
    | false ->
       begin
	 addConstrList Z3Init.init
       end
  end ;

  (* SANITY CHECK *)
  try
    let names_in_process =  (freeNames runProcess) @ (boundNames runProcess) in
    let rec superList  l = function
      | [] -> true
      | h :: t -> List.mem h l && superList l t in
    if not (superList names_in_process linear_channels)
    then
      raise LinearNotInProcess;
    let goKeywords =
      [(* GO *)
	"break"; "default"; "func"; "interface"; "select"; "case"; "defer";
	"go"; "map"; "struct"; "chan"; "else"; "goto"; "package";
	"switch"; "const"; "fallthrough"; "if"; "range"; "type"; 
	"continue"; "for"; "import"; "return"; "var"; "main"; "make";
	"init"; "error" ; "return"; "rand"; "time";
	(* GOPI *)
	"counter"; "key"; "done"; "value"; "typeEnv"; "letterRunes";
	"SafeCounter"; "dummy"
      ] @ GoValues.reservedKeywords in
    let clashing_channels = cap goKeywords names_in_process in
    
    if isNotEmpty clashing_channels && not type_check_only
    then
      raise (ReservedKeyword (string_of_list clashing_channels));
    
    (************************ PROCESS PRINT *************************)
    if print_process
    then
      print_endline ("Process PRINT:\n" ^ (toStringLSpi runProcess));
    
    (************************ TYPE INFERENCE ************************)
    if secretProcess runProcess
    then
      match (linearMode, deadlock_detection) with
      | true, false ->
	 raise SecretAndNotFull
      | _, _ ->
	 (match order_of_catalyzer with
	  | n when n > 0 ->
	     execInfer
	       (Par(runProcess, catalyzerProcess)) linear_channels bv fn deadlock_detection;
	  | _ ->
	     execInfer runProcess linear_channels bv fn deadlock_detection)
    else
      execInfer runProcess linear_channels bv fn deadlock_detection;

    (************************** GO PORT ******************************)
    let fn_proc_go =  (z3ToList fn modelFile) and
	_ = (z3ToList (boundNames runProcess) modelFile) and
	maxOrder = !GoValues.max_order_test_process in
    
    if maxOrder > GoValues.maxOrder
    then
      raise (MaxOrderExceeded "The order of channels is greater than 100. You may try to recompile GenerateGoValues with a bigger maxOrder, although this is discouraged." );
    
    begin
      match order_of_catalyzer with
      | n when n > 0 ->
	 print_endline
	   ("TYPE-CHECKED (with catalyser of order " ^ stri order_of_catalyzer ^")");
	 if print_catalyzer
	 then
	   print_endline
	     ("CATALYSER: " ^ (toStringLSpi catalyzerProcess)
	      ^ "\n******************************************************************")
      | _ ->
	 print_endline ("TYPE-CHECKED -- MAX ORDER: " ^ stri maxOrder)
    end ;
    
    if not type_check_only
    then
      begin
	print_endline ("GENERATING GO FILE " ^ goFile);
	(* Go order is (maxOrder - 1) since chan0 = chan base *)
	goCode :=
	  initializeGoCode (maxOrder - 1);
	addGoList 0 ["/********************************* INIT ************************************/"];
	let type_defs, inferred = freeTypedNames fn_proc_go in
	addGoList 0 type_defs;
	let baseNames = (listDiff fn inferred) in
	addGoList 0 (freeStrNames baseNames);
	addGoList 0 (freeInit fn_proc_go (maxOrder - 1));
	addGoList 0 ["/********************************* MAIN ************************************/"];
	addGo 0 "func main() {";
	addGo 1 "key = \"0\"";
	addGo 1 "fmt.Printf(\"*****Running process proc1******\\n\")";
	let countDone, countActive = toGoCode linear_channels runProcess in
	if countActive
	then
	  addGoList 1 [("for i := 0; i < " ^ (stri countDone) ^ "; i++ {");
		       "    <-done";
		       "}"]
	else
	  addGoList 1 [("for{");
		       "    <-done";
		       "}"]; 
	addGoList 0 ["}"];
	
	let goChan = open_out goFile in
	writeList (formatter_of_out_channel goChan) !goCode ;
	close_out goChan;
	
	if not (compile_only)
	then
	  begin
	    print_endline ("RUNNING THE PROCESS (go run -race " ^ goFile ^")");
	    if debugMode
	    then
	      print_endline ("Param.goPath is set to: " ^ goPath);
	    exit(Sys.command((Param.goPath ^ " run  " ^ goFile)))
	  end
	    
      end

  with
  | (AlphaRename x) ->
     print_endline
       ("******************************************************************\n"
	^ "Process has name clash: alpha rename \"" ^ x ^ "\"\n"
	^ "******************************************************************")
       
  | LinearNotInProcess ->
     print_endline
       ("******************************************************************\n"
	^ "Not supported: all linear channels must occur in process\n"
	^ "******************************************************************")
       
  | ReservedKeyword s ->
     print_endline
       ("******************************************************************\n"
	^ "Process uses reserved Go/GoPi keyword: rename " ^ s ^"\n"
	^ "*******************************************************************")
       
  | SecretAndNotFull ->
     print_endline
       ("*******************************************************************\n"
	^ "Not supported (not full secret process):\n deactivate -nf option\n"
	^ "******************************************************************")
       
  | (UnSat s) -> 
     print_endline
       ("******************************************************************\n"
	^ "Process does not type check\n"
	^ "******************************************************************");
     
     if secretProcess runProcess && order_of_catalyzer > 0 && debugMode
     then
       print_endline
	 ("CATALYZER: " ^ (toStringLSpi catalyzerProcess)
	  ^ "\n******************************************************************");
     
     if not debugMode
     then
       print_endline
	 ("UNSAT CORE: (use -debug option for more info)\n" ^ s)
     else
       
       (* Inline parsing of Param.modelFile to print the unsat labeled constraints *) 
       let processLine = ref "" and
	   constrList = ref [] in
       let line_stream_of_channel channel =
	 Stream.from
	   (fun _ ->
	    try Some (input_line channel) with End_of_file -> None) in
       let in_channel = open_in Param.modelFile in
       let  _ =
	 input_line in_channel  in
       let  _ =
	 input_line in_channel  in
       let  lastline =
	 input_line in_channel  in
       close_in in_channel;
       
       let n = String.length lastline in
       let tokens = String.sub lastline 1 (n - 2) in
       let unsatLabels = String.split_on_char ' ' tokens in
       let memEquals s l =
	 List.mem s l || List.mem (String.sub s 1 ((String.length s)-1)) l
	 || List.mem ("A" ^ s) l in
       let isNamed line =
	 let n = String.length line in
	 let named = String.sub line (n - 12) 6 in
	 let label = String.sub line (n - 5) 3 in
	 (named = ":named" || named = " :name" || named = "named ") &&
	   (memEquals label unsatLabels) in 
       let process_line line = 
	 if ((String.length line) > 7 &&
	       (String.sub line 0 7) = "(assert" &&
		 isNamed line) then 
	   processLine:= !processLine ^ "\n " ^ line in
       let process_lines lines =
	 Stream.iter process_line lines;
	 constrList := !constrList@[!processLine] in 
       let parseFile file =
	 let in_channel = open_in file in
	 try
	   process_lines (line_stream_of_channel in_channel); 
	   close_in in_channel
	 with e ->
	   close_in in_channel;
	   raise e in
       parseFile Param.constraintFile;
       
       print_endline
	 ("UNSAT CORE: (DebugMode is On)\n" ^ s);
       print_endline "************************ SMT-LIB Header *************************\n";
       
       if linearMode
       then
	 printList Z3Init.linear_init
       else
	 printList Z3Init.init;
       
       print_endline "********************* SMT-LIB Constraints ************************";
       printList !constrList
