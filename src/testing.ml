
open Util;;
open Distance;;

let tree1 = (Parse.implementation (Lexing.from_string
"
let rec sum f  =
match f with
| [] -> 0
| x :: xs -> x + (sum xs);;
"))
in
let tree2 = Parse.implementation (Lexing.from_string ~with_positions:false
"
let rec sum f  =
match f with
| [] -> 0
| x :: xs -> x + (sum xs);;
")
in
let convertedTree1 = Distance.makeroot "dummyroot" 
in 
(Converter.makeDistable tree1 convertedTree1);

let convertedTree2 =  (Distance.makeroot "dummyroot")
in
(Converter.makeDistable tree2 convertedTree2);



let size1 = (count (Some(convertedTree1)))
in
let size2 = (count (Some(convertedTree2)))
in


let bigD = distanceD convertedTree1 convertedTree2
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
