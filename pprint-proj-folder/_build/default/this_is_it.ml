
(*
#use "topfind";;

#load "compiler-libs/ocamlcommon.cma";;
*)


(*
let x = Parse.implementation (Lexing.from_string ~with_positions:false
"
let rec mapTree f (t: 'a tree) =
match t with
| Empty -> Empty
| Node(l,v,r) -> Node((mapTree f l), (f v), (mapTree f r));;
");;

let y = Parse.implementation (Lexing.from_string ~with_positions:false
"
let rec sum f  =
match f with
| [] -> 0
| x :: xs -> x + (sum xs);;
") in y;;

let z = (Parse.implementation (Lexing.from_string
"
let x = 10;;
"));;

let q = 4;;

*)


let x = Parse.implementation (Lexing.from_string ~with_positions:false
"
let rec mapTree f (t: 'a tree) =
match t with
| Empty -> Empty
| Node(l,v,r) -> Node((mapTree f l), (f v), (mapTree f r));;
")

in
let rec small_print parsa = 
  match parsa with
    | x :: _ -> sprint_str_item x 0
    | _ -> ()
and
  sprint_str_item (struc:Parsetree.structure_item) n =
  match struc.Parsetree.pstr_desc with 
    | Pstr_value(a,b) -> (print_n n; print_string "Let statements: "; 
                          match a with 
                            | Recursive -> (print_n n; print_string "Rec\n")
                            | Nonrecursive -> (print_n n; print_string "Nonrec\n"));
        sprint_val_binds b (n+1) 1
    | _ -> ()
and
  sprint_val_binds (vales:Parsetree.value_binding list) n times =
  match vales with
    | [] -> ()
    | x :: xs -> 
        print_n n;
        print_string "let stmt";
        print_int times;
        print_string "\n";
        sprint_pattern x.Parsetree.pvb_pat (n+1);
        sprint_expr x.Parsetree.pvb_expr (n+1);
        sprint_val_binds xs n (times+1)
and
  sprint_expr (expre:Parsetree.expression) n =
  print_n n;
  print_string "expression: ";
  match expre.Parsetree.pexp_desc with
    | Pexp_constant(x) -> (
        match x with
          | Pconst_integer(num, _) -> 
              (print_string "constant: "; 
               print_string num; 
               print_string "\n")
          | _ -> ())
    | Pexp_fun(_, _, innpat, innexp) -> (
        print_string "function.\n";
        sprint_pattern innpat (n+1);
        sprint_expr innexp (n+1)
      )
    | Pexp_match(innexp, caseslist) -> (
        print_string "match.\n";
        sprint_expr innexp (n+1);
        sprint_cases caseslist (n+1) 1
      )
    | Pexp_ident(id) -> (
        print_string "identifier: ";
        (match id.Asttypes.txt with 
          | Longident.Lident(stuff) -> print_string stuff
          | _ -> print_string "weird identifier");
        print_string "\n"
      )
    | Pexp_apply(func_exp, args_exps) ->
        print_string "application.\n";
        sprint_expr func_exp (n+1);
        sprint_args args_exps (n+1) 1
    | Pexp_construct(name, args) -> (
		print_string "constructor: ";
		(match name.Asttypes.txt with 
			| Longident.Lident(stuff) -> print_string stuff
			| _ -> print_string "weird identifier");
		print_string "\n";
		(match args with
			| None -> ()
			| Some(inner_args) -> (
				print_n (n+1);
				print_string "args:\n";
				sprint_expr inner_args (n+2)
			)
		)
	)
    | Pexp_tuple(listicle) -> (print_string "\n"; sprint_expres listicle (n+1) 1)
    | _ -> (print_string "\n")
and
  sprint_pattern (patte: Parsetree.pattern) n = 
  print_n n;
  print_string "pattern:\n";
  (match patte.Parsetree.ppat_desc with
    | Ppat_var(x) -> (
        print_n (n+1); 
        print_string (x.Asttypes.txt); 
        print_string "\n"
      )
    | Ppat_construct(id, nxtpat) ->
        (match id.Asttypes.txt with 
          | Longident.Lident(stuff) -> (print_n (n+1); print_string stuff; print_string "\n")
          | _ -> print_string "weird identifier\n");
        (match nxtpat with
          | None -> ()
          | Some (realpat) -> 
              sprint_pattern realpat (n+1)
        );
    | Ppat_tuple(patts) -> (sprint_patts patts (n+1) 1)
    | Ppat_constraint(innerPattern, _) -> (
		print_n (n+1);
		print_string ("constrained\n");
		sprint_pattern innerPattern (n+2)
	)
    | _ -> ())
and
  sprint_cases (cases_l: Parsetree.case list) n times =
  match cases_l with
    | [] -> ()
    | x :: xs -> 
        print_n n;
        print_string "case ";
        print_int times;
        print_string "\n";
        sprint_pattern x.Parsetree.pc_lhs (n+1);
        sprint_expr x.Parsetree.pc_rhs (n+1);
        sprint_cases xs n (times + 1)
and
  sprint_args args_pair_list n times =
  match args_pair_list with 
    | [] -> ()
    | (_, x) :: xs ->
        print_n n;
        print_string "arg ";
        print_int times;
        print_string "\n";
        sprint_expr x (n+1);
        sprint_args xs n (times + 1)
and
  sprint_patts pat_list n times =
  match pat_list with
    | [] -> ()
    | x :: xs -> 
        print_n n;
        print_string "pattern ";
        print_int times;
        print_string "\n";
        sprint_pattern x (n+1);
        sprint_patts xs n (times + 1)
and
  sprint_expres expre_list n times =
  match expre_list with
    | [] -> ()
    | x :: xs -> 
        print_n n;
        print_string "expression ";
        print_int times;
        print_string "\n";
        sprint_expr x (n+1);
        sprint_expres xs n (times + 1)
and
  print_n x =
  if x > 0 then (Printf.printf "  "; print_n (x-1))
  else ()
in
  small_print x

