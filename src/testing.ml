
open Util;;

let x = (Parse.implementation (Lexing.from_string
"
let x = 10
and
y = 12;;
"))
in
let y = Converter.makeDistable x (Distance.makeroot "dummyroot")
in
(Distance.traverse y 0)


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
