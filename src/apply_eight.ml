
open Util;;

open Distance8;;

open Converter8;;


let costSwap (_, str2a) (_, str2b) =
	if str2a = str2b then (0) else 15
;;

let costInsDel str1 =
	match str1 with (_, scond) -> 
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


let tree1 = (Parse.implementation (Lexing.from_string (read_file  Sys.argv.(1))))
in
let convertedTree1 = makeDistable tree1
in


let tree2 = (Parse.implementation (Lexing.from_string (read_file Sys.argv.(2))))
in
let convertedTree2 = makeDistable tree2
in
(*
let collect = fun root newleaf -> addchild root newleaf
in

let ftree1 = makeroot ("dummy", "root") in
List.iter (collect ftree1) convertedTree1;
let ftree2 = makeroot ("dummy", "root") in
List.iter (collect ftree2) convertedTree2;

*)

let cur_interest = Sys.argv.(3) in

let findname name tee =
	match tee.fChi with 
	| None -> false
	| Some(binding) -> match binding.fChi with
		| None -> false
		| Some(var_pat) -> match var_pat.data with
			| (true_name, _) -> true_name = name
in

let ftree1 = List.find (findname cur_interest) convertedTree1
in
let ftree2 = List.find (findname cur_interest) convertedTree2 
in

(*
traverse ftree1 0;
traverse ftree2 0;
*)

let size1 = (count (Some(ftree1)))
in
let size2 = (count (Some(ftree2)))
in


let myArray = main_treedistloop ftree1 ftree2 costInsDel costSwap
in

print_int myArray.(size1 - 1).(size2 - 1);
print_newline ()



