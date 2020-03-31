
open Util;;
(*
open Distance8;;
*)
open Converter8;;

(*
let costSwap (_, str2a) (_, str2b) =
	if str2a = str2b then (0) else 15
;;

let costInsDel str1 =
	match str1 with (_, scond) -> 
	if scond = "constructor" then 3 else 1
;;
*)

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


let tree1 = (Parse.implementation (Lexing.from_string (read_file  Sys.argv.(1))))
in
let convertedTree1 = makeDistable tree1
in

print_int (List.length convertedTree1);
print_newline ()

(*
let tree2 = (Parse.implementation (Lexing.from_string (read_file Sys.argv.(2))))
in
let convertedTree2 =  (makeroot ("dummy","root"))
in
makeDistable tree2 convertedTree2;



let size1 = (count (Some(convertedTree1)))
in
let size2 = (count (Some(convertedTree2)))
in


let myArray = main_treedistloop convertedTree1 convertedTree2 costInsDel costSwap
in

print_int myArray.(size1 - 1).(size2 - 1);
print_newline ()

*)
