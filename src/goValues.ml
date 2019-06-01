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

(** Types of Go and marshalling / unmarshalling functions. Module generated
    by GenerateGoValues *)

(** Max order of communication is set to 100 *)
let maxOrder = 100

(** Max order of actual process *)
let max_order_test_process = ref 0


(** Order of channel given list of tokens extracted from Z3 model *)
let getChannelOrder l =
  let rec getChannelOrderR acc  = function
    | [] ->
       let mo = !max_order_test_process in
       max_order_test_process := max acc mo;
       acc
    | h::t ->
       if h = "(channel" then
         getChannelOrderR (acc + 1) t
       else
         getChannelOrderR acc t
  in getChannelOrderR 0 l

exception MaxOrderExceeded of string

type goValue =
  | Base
  | Chan0
  | Chan1
  | Chan2
  | Chan3
  | Chan4
  | Chan5
  | Chan6
  | Chan7
  | Chan8
  | Chan9
  | Chan10
  | Chan11
  | Chan12
  | Chan13
  | Chan14
  | Chan15
  | Chan16
  | Chan17
  | Chan18
  | Chan19
  | Chan20
  | Chan21
  | Chan22
  | Chan23
  | Chan24
  | Chan25
  | Chan26
  | Chan27
  | Chan28
  | Chan29
  | Chan30
  | Chan31
  | Chan32
  | Chan33
  | Chan34
  | Chan35
  | Chan36
  | Chan37
  | Chan38
  | Chan39
  | Chan40
  | Chan41
  | Chan42
  | Chan43
  | Chan44
  | Chan45
  | Chan46
  | Chan47
  | Chan48
  | Chan49
  | Chan50
  | Chan51
  | Chan52
  | Chan53
  | Chan54
  | Chan55
  | Chan56
  | Chan57
  | Chan58
  | Chan59
  | Chan60
  | Chan61
  | Chan62
  | Chan63
  | Chan64
  | Chan65
  | Chan66
  | Chan67
  | Chan68
  | Chan69
  | Chan70
  | Chan71
  | Chan72
  | Chan73
  | Chan74
  | Chan75
  | Chan76
  | Chan77
  | Chan78
  | Chan79
  | Chan80
  | Chan81
  | Chan82
  | Chan83
  | Chan84
  | Chan85
  | Chan86
  | Chan87
  | Chan88
  | Chan89
  | Chan90
  | Chan91
  | Chan92
  | Chan93
  | Chan94
  | Chan95
  | Chan96
  | Chan97
  | Chan98
  | Chan99
  | Chan100

let toString = function
  | Base -> "base" 
  | Chan0 -> "chan0"
  | Chan1 -> "chan1"
  | Chan2 -> "chan2"
  | Chan3 -> "chan3"
  | Chan4 -> "chan4"
  | Chan5 -> "chan5"
  | Chan6 -> "chan6"
  | Chan7 -> "chan7"
  | Chan8 -> "chan8"
  | Chan9 -> "chan9"
  | Chan10 -> "chan10"
  | Chan11 -> "chan11"
  | Chan12 -> "chan12"
  | Chan13 -> "chan13"
  | Chan14 -> "chan14"
  | Chan15 -> "chan15"
  | Chan16 -> "chan16"
  | Chan17 -> "chan17"
  | Chan18 -> "chan18"
  | Chan19 -> "chan19"
  | Chan20 -> "chan20"
  | Chan21 -> "chan21"
  | Chan22 -> "chan22"
  | Chan23 -> "chan23"
  | Chan24 -> "chan24"
  | Chan25 -> "chan25"
  | Chan26 -> "chan26"
  | Chan27 -> "chan27"
  | Chan28 -> "chan28"
  | Chan29 -> "chan29"
  | Chan30 -> "chan30"
  | Chan31 -> "chan31"
  | Chan32 -> "chan32"
  | Chan33 -> "chan33"
  | Chan34 -> "chan34"
  | Chan35 -> "chan35"
  | Chan36 -> "chan36"
  | Chan37 -> "chan37"
  | Chan38 -> "chan38"
  | Chan39 -> "chan39"
  | Chan40 -> "chan40"
  | Chan41 -> "chan41"
  | Chan42 -> "chan42"
  | Chan43 -> "chan43"
  | Chan44 -> "chan44"
  | Chan45 -> "chan45"
  | Chan46 -> "chan46"
  | Chan47 -> "chan47"
  | Chan48 -> "chan48"
  | Chan49 -> "chan49"
  | Chan50 -> "chan50"
  | Chan51 -> "chan51"
  | Chan52 -> "chan52"
  | Chan53 -> "chan53"
  | Chan54 -> "chan54"
  | Chan55 -> "chan55"
  | Chan56 -> "chan56"
  | Chan57 -> "chan57"
  | Chan58 -> "chan58"
  | Chan59 -> "chan59"
  | Chan60 -> "chan60"
  | Chan61 -> "chan61"
  | Chan62 -> "chan62"
  | Chan63 -> "chan63"
  | Chan64 -> "chan64"
  | Chan65 -> "chan65"
  | Chan66 -> "chan66"
  | Chan67 -> "chan67"
  | Chan68 -> "chan68"
  | Chan69 -> "chan69"
  | Chan70 -> "chan70"
  | Chan71 -> "chan71"
  | Chan72 -> "chan72"
  | Chan73 -> "chan73"
  | Chan74 -> "chan74"
  | Chan75 -> "chan75"
  | Chan76 -> "chan76"
  | Chan77 -> "chan77"
  | Chan78 -> "chan78"
  | Chan79 -> "chan79"
  | Chan80 -> "chan80"
  | Chan81 -> "chan81"
  | Chan82 -> "chan82"
  | Chan83 -> "chan83"
  | Chan84 -> "chan84"
  | Chan85 -> "chan85"
  | Chan86 -> "chan86"
  | Chan87 -> "chan87"
  | Chan88 -> "chan88"
  | Chan89 -> "chan89"
  | Chan90 -> "chan90"
  | Chan91 -> "chan91"
  | Chan92 -> "chan92"
  | Chan93 -> "chan93"
  | Chan94 -> "chan94"
  | Chan95 -> "chan95"
  | Chan96 -> "chan96"
  | Chan97 -> "chan97"
  | Chan98 -> "chan98"
  | Chan99 -> "chan99"
  | Chan100 -> "chan100"

let toInt = function
  | Base -> -1 
  | Chan0 -> 0
  | Chan1 -> 1
  | Chan2 -> 2
  | Chan3 -> 3
  | Chan4 -> 4
  | Chan5 -> 5
  | Chan6 -> 6
  | Chan7 -> 7
  | Chan8 -> 8
  | Chan9 -> 9
  | Chan10 -> 10
  | Chan11 -> 11
  | Chan12 -> 12
  | Chan13 -> 13
  | Chan14 -> 14
  | Chan15 -> 15
  | Chan16 -> 16
  | Chan17 -> 17
  | Chan18 -> 18
  | Chan19 -> 19
  | Chan20 -> 20
  | Chan21 -> 21
  | Chan22 -> 22
  | Chan23 -> 23
  | Chan24 -> 24
  | Chan25 -> 25
  | Chan26 -> 26
  | Chan27 -> 27
  | Chan28 -> 28
  | Chan29 -> 29
  | Chan30 -> 30
  | Chan31 -> 31
  | Chan32 -> 32
  | Chan33 -> 33
  | Chan34 -> 34
  | Chan35 -> 35
  | Chan36 -> 36
  | Chan37 -> 37
  | Chan38 -> 38
  | Chan39 -> 39
  | Chan40 -> 40
  | Chan41 -> 41
  | Chan42 -> 42
  | Chan43 -> 43
  | Chan44 -> 44
  | Chan45 -> 45
  | Chan46 -> 46
  | Chan47 -> 47
  | Chan48 -> 48
  | Chan49 -> 49
  | Chan50 -> 50
  | Chan51 -> 51
  | Chan52 -> 52
  | Chan53 -> 53
  | Chan54 -> 54
  | Chan55 -> 55
  | Chan56 -> 56
  | Chan57 -> 57
  | Chan58 -> 58
  | Chan59 -> 59
  | Chan60 -> 60
  | Chan61 -> 61
  | Chan62 -> 62
  | Chan63 -> 63
  | Chan64 -> 64
  | Chan65 -> 65
  | Chan66 -> 66
  | Chan67 -> 67
  | Chan68 -> 68
  | Chan69 -> 69
  | Chan70 -> 70
  | Chan71 -> 71
  | Chan72 -> 72
  | Chan73 -> 73
  | Chan74 -> 74
  | Chan75 -> 75
  | Chan76 -> 76
  | Chan77 -> 77
  | Chan78 -> 78
  | Chan79 -> 79
  | Chan80 -> 80
  | Chan81 -> 81
  | Chan82 -> 82
  | Chan83 -> 83
  | Chan84 -> 84
  | Chan85 -> 85
  | Chan86 -> 86
  | Chan87 -> 87
  | Chan88 -> 88
  | Chan89 -> 89
  | Chan90 -> 90
  | Chan91 -> 91
  | Chan92 -> 92
  | Chan93 -> 93
  | Chan94 -> 94
  | Chan95 -> 95
  | Chan96 -> 96
  | Chan97 -> 97
  | Chan98 -> 98
  | Chan99 -> 99
  | Chan100 -> 100

let extractType s =
  let split_list =  (String.split_on_char ' ' s) in
  let order = getChannelOrder split_list in
  match order with
  | 0 -> Base
  | 1 -> Chan0
  | 2 -> Chan1
  | 3 -> Chan2
  | 4 -> Chan3
  | 5 -> Chan4
  | 6 -> Chan5
  | 7 -> Chan6
  | 8 -> Chan7
  | 9 -> Chan8
  | 10 -> Chan9
  | 11 -> Chan10
  | 12 -> Chan11
  | 13 -> Chan12
  | 14 -> Chan13
  | 15 -> Chan14
  | 16 -> Chan15
  | 17 -> Chan16
  | 18 -> Chan17
  | 19 -> Chan18
  | 20 -> Chan19
  | 21 -> Chan20
  | 22 -> Chan21
  | 23 -> Chan22
  | 24 -> Chan23
  | 25 -> Chan24
  | 26 -> Chan25
  | 27 -> Chan26
  | 28 -> Chan27
  | 29 -> Chan28
  | 30 -> Chan29
  | 31 -> Chan30
  | 32 -> Chan31
  | 33 -> Chan32
  | 34 -> Chan33
  | 35 -> Chan34
  | 36 -> Chan35
  | 37 -> Chan36
  | 38 -> Chan37
  | 39 -> Chan38
  | 40 -> Chan39
  | 41 -> Chan40
  | 42 -> Chan41
  | 43 -> Chan42
  | 44 -> Chan43
  | 45 -> Chan44
  | 46 -> Chan45
  | 47 -> Chan46
  | 48 -> Chan47
  | 49 -> Chan48
  | 50 -> Chan49
  | 51 -> Chan50
  | 52 -> Chan51
  | 53 -> Chan52
  | 54 -> Chan53
  | 55 -> Chan54
  | 56 -> Chan55
  | 57 -> Chan56
  | 58 -> Chan57
  | 59 -> Chan58
  | 60 -> Chan59
  | 61 -> Chan60
  | 62 -> Chan61
  | 63 -> Chan62
  | 64 -> Chan63
  | 65 -> Chan64
  | 66 -> Chan65
  | 67 -> Chan66
  | 68 -> Chan67
  | 69 -> Chan68
  | 70 -> Chan69
  | 71 -> Chan70
  | 72 -> Chan71
  | 73 -> Chan72
  | 74 -> Chan73
  | 75 -> Chan74
  | 76 -> Chan75
  | 77 -> Chan76
  | 78 -> Chan77
  | 79 -> Chan78
  | 80 -> Chan79
  | 81 -> Chan80
  | 82 -> Chan81
  | 83 -> Chan82
  | 84 -> Chan83
  | 85 -> Chan84
  | 86 -> Chan85
  | 87 -> Chan86
  | 88 -> Chan87
  | 89 -> Chan88
  | 90 -> Chan89
  | 91 -> Chan90
  | 92 -> Chan91
  | 93 -> Chan92
  | 94 -> Chan93
  | 95 -> Chan94
  | 96 -> Chan95
  | 97 -> Chan96
  | 98 -> Chan97
  | 99 -> Chan98
  | 100 -> Chan99
  | _ -> raise (MaxOrderExceeded s)

let reservedKeywords = [
    "base"; 
    "basePair"; 
    "queueBase"; 
    "chan0";
    "chan0Pair";
    "queueChan0";
    "chan1";
    "chan1Pair";
    "queueChan1";
    "chan2";
    "chan2Pair";
    "queueChan2";
    "chan3";
    "chan3Pair";
    "queueChan3";
    "chan4";
    "chan4Pair";
    "queueChan4";
    "chan5";
    "chan5Pair";
    "queueChan5";
    "chan6";
    "chan6Pair";
    "queueChan6";
    "chan7";
    "chan7Pair";
    "queueChan7";
    "chan8";
    "chan8Pair";
    "queueChan8";
    "chan9";
    "chan9Pair";
    "queueChan9";
    "chan10";
    "chan10Pair";
    "queueChan10";
    "chan11";
    "chan11Pair";
    "queueChan11";
    "chan12";
    "chan12Pair";
    "queueChan12";
    "chan13";
    "chan13Pair";
    "queueChan13";
    "chan14";
    "chan14Pair";
    "queueChan14";
    "chan15";
    "chan15Pair";
    "queueChan15";
    "chan16";
    "chan16Pair";
    "queueChan16";
    "chan17";
    "chan17Pair";
    "queueChan17";
    "chan18";
    "chan18Pair";
    "queueChan18";
    "chan19";
    "chan19Pair";
    "queueChan19";
    "chan20";
    "chan20Pair";
    "queueChan20";
    "chan21";
    "chan21Pair";
    "queueChan21";
    "chan22";
    "chan22Pair";
    "queueChan22";
    "chan23";
    "chan23Pair";
    "queueChan23";
    "chan24";
    "chan24Pair";
    "queueChan24";
    "chan25";
    "chan25Pair";
    "queueChan25";
    "chan26";
    "chan26Pair";
    "queueChan26";
    "chan27";
    "chan27Pair";
    "queueChan27";
    "chan28";
    "chan28Pair";
    "queueChan28";
    "chan29";
    "chan29Pair";
    "queueChan29";
    "chan30";
    "chan30Pair";
    "queueChan30";
    "chan31";
    "chan31Pair";
    "queueChan31";
    "chan32";
    "chan32Pair";
    "queueChan32";
    "chan33";
    "chan33Pair";
    "queueChan33";
    "chan34";
    "chan34Pair";
    "queueChan34";
    "chan35";
    "chan35Pair";
    "queueChan35";
    "chan36";
    "chan36Pair";
    "queueChan36";
    "chan37";
    "chan37Pair";
    "queueChan37";
    "chan38";
    "chan38Pair";
    "queueChan38";
    "chan39";
    "chan39Pair";
    "queueChan39";
    "chan40";
    "chan40Pair";
    "queueChan40";
    "chan41";
    "chan41Pair";
    "queueChan41";
    "chan42";
    "chan42Pair";
    "queueChan42";
    "chan43";
    "chan43Pair";
    "queueChan43";
    "chan44";
    "chan44Pair";
    "queueChan44";
    "chan45";
    "chan45Pair";
    "queueChan45";
    "chan46";
    "chan46Pair";
    "queueChan46";
    "chan47";
    "chan47Pair";
    "queueChan47";
    "chan48";
    "chan48Pair";
    "queueChan48";
    "chan49";
    "chan49Pair";
    "queueChan49";
    "chan50";
    "chan50Pair";
    "queueChan50";
    "chan51";
    "chan51Pair";
    "queueChan51";
    "chan52";
    "chan52Pair";
    "queueChan52";
    "chan53";
    "chan53Pair";
    "queueChan53";
    "chan54";
    "chan54Pair";
    "queueChan54";
    "chan55";
    "chan55Pair";
    "queueChan55";
    "chan56";
    "chan56Pair";
    "queueChan56";
    "chan57";
    "chan57Pair";
    "queueChan57";
    "chan58";
    "chan58Pair";
    "queueChan58";
    "chan59";
    "chan59Pair";
    "queueChan59";
    "chan60";
    "chan60Pair";
    "queueChan60";
    "chan61";
    "chan61Pair";
    "queueChan61";
    "chan62";
    "chan62Pair";
    "queueChan62";
    "chan63";
    "chan63Pair";
    "queueChan63";
    "chan64";
    "chan64Pair";
    "queueChan64";
    "chan65";
    "chan65Pair";
    "queueChan65";
    "chan66";
    "chan66Pair";
    "queueChan66";
    "chan67";
    "chan67Pair";
    "queueChan67";
    "chan68";
    "chan68Pair";
    "queueChan68";
    "chan69";
    "chan69Pair";
    "queueChan69";
    "chan70";
    "chan70Pair";
    "queueChan70";
    "chan71";
    "chan71Pair";
    "queueChan71";
    "chan72";
    "chan72Pair";
    "queueChan72";
    "chan73";
    "chan73Pair";
    "queueChan73";
    "chan74";
    "chan74Pair";
    "queueChan74";
    "chan75";
    "chan75Pair";
    "queueChan75";
    "chan76";
    "chan76Pair";
    "queueChan76";
    "chan77";
    "chan77Pair";
    "queueChan77";
    "chan78";
    "chan78Pair";
    "queueChan78";
    "chan79";
    "chan79Pair";
    "queueChan79";
    "chan80";
    "chan80Pair";
    "queueChan80";
    "chan81";
    "chan81Pair";
    "queueChan81";
    "chan82";
    "chan82Pair";
    "queueChan82";
    "chan83";
    "chan83Pair";
    "queueChan83";
    "chan84";
    "chan84Pair";
    "queueChan84";
    "chan85";
    "chan85Pair";
    "queueChan85";
    "chan86";
    "chan86Pair";
    "queueChan86";
    "chan87";
    "chan87Pair";
    "queueChan87";
    "chan88";
    "chan88Pair";
    "queueChan88";
    "chan89";
    "chan89Pair";
    "queueChan89";
    "chan90";
    "chan90Pair";
    "queueChan90";
    "chan91";
    "chan91Pair";
    "queueChan91";
    "chan92";
    "chan92Pair";
    "queueChan92";
    "chan93";
    "chan93Pair";
    "queueChan93";
    "chan94";
    "chan94Pair";
    "queueChan94";
    "chan95";
    "chan95Pair";
    "queueChan95";
    "chan96";
    "chan96Pair";
    "queueChan96";
    "chan97";
    "chan97Pair";
    "queueChan97";
    "chan98";
    "chan98Pair";
    "queueChan98";
    "chan99";
    "chan99Pair";
    "queueChan99";     "chan100";     "chan100Pair";     "queueChan100"; ] 
