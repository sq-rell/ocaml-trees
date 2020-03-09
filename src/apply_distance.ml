


open Util;;
open Distance;;


let costSwap (_, str2a) (_, str2b) =
	if str2a = str2b then (0) else 15
;;

let costInsDel str1 =
	match str1 with (_, scond) -> 
	if scond = "unknown" then 3 else 
	if scond = "constructor" then 3 else 1
;;

let read_file filename = 
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; String.concat "\n" (List.rev !lines)
with End_of_file ->
  close_in chan;
  String.concat "\n" (List.rev !lines) ;;


let tree1 = (Parse.implementation (Lexing.from_string (read_file (String.concat "" ["../sample_files/"; Sys.argv.(1)]))))
in
let tree2 = (Parse.implementation (Lexing.from_string (read_file (String.concat "" ["../sample_files/"; Sys.argv.(2)]))))
in
let convertedTree1 = Distance.makeroot ("dummy","root" )
in 
(Converter.makeDistable tree1 convertedTree1);

let convertedTree2 =  (Distance.makeroot ("dummy","root"))
in
(Converter.makeDistable tree2 convertedTree2);



let size1 = (count (Some(convertedTree1)))
in
let size2 = (count (Some(convertedTree2)))
in




let bigD = Distance.distanceD convertedTree1 convertedTree2 costSwap costInsDel
in
let finalDist = bigD.(size1 - 1).(size2 - 1)
in


print_int finalDist;
print_newline ();

(*
generatePreNum convertedTree1;

traverse convertedTree1 0;
traverse convertedTree2 0;
*)

(*
let x = Parse.implementation (Lexing.from_string ~with_positions:false
"
let rec mapTree f (t: 'a tree) =
match t with
| Empty -> Empty
| Node(l,v,r) -> Node((mapTree f l), (f v), (mapTree f r));;
");;

let x = Parse.implementation (Lexing.from_string ~with_positions:false
"
let rec sum f  =
match f with
| [] -> 0
| x :: xs -> x + (sum xs);;
") in y;;

let x = (Parse.implementation (Lexing.from_string
"
let x = 10;;
"));;

let q = 4;;

*)

