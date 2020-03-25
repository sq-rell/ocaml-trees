


open Util;;
open Distance7;;


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
let convertedTree1 = makeroot ("dummy","root" )
in 
(Converter.makeDistable tree1 convertedTree1);
(*
let tree2 = (Parse.implementation (Lexing.from_string (read_file (String.concat "" ["../"; Sys.argv.(2)]))))
in
*)


traverse  convertedTree1 0
(*
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

