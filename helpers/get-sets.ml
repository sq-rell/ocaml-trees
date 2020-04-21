

let read_file filename = 
let lines = ref [] in
let chan = open_in filename in
try
	while true; do
		lines := input_line chan :: !lines
	done; (List.rev !lines)
with End_of_file ->
	close_in chan;
	(List.rev !lines) ;;
  

let list1 = read_file "array/hw4/delete_data.csv"  in

let rec getzeros distes studes =
	match distes with
	| y :: ys -> (match int_of_string_opt y with
		| Some(k) -> 
			(if k = 0 then (print_int studes; print_string ";") else ());
			getzeros ys (studes + 1)
		| _ -> getzeros ys (studes + 1))
	| _ -> ()


and iter acc1 acc2 = 
	match acc1 with
	| [] -> ()
	| x :: xs -> 
		print_string "Student ";
		print_int acc2;
		print_endline ":";
		(getzeros (String.split_on_char ',' x) 1);
		print_newline ();
		iter xs (acc2 + 1)
		
in
iter list1 1
(*
match (iter list1 (0,"0")) with (a,b) ->
print_int a;
print_newline ();
print_string b;
print_newline ();

and oc = open_out "array/hw3p3/sets.txt"*)
