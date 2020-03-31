
open Distance8;;

let rec 
makeDistable parsa = 
	let rec aux cur acc = 
		match cur with
		| [] -> acc
		| x :: xs -> aux xs ((makeDistableStructure x) :: acc)
	in
	aux parsa []
and
makeDistableStructure (struc:Parsetree.structure_item) =
	match struc.Parsetree.pstr_desc with 
	| Pstr_value(a,b) -> 
		let returnable =
		(match a with 
		| Recursive -> (makeroot ("declarations", "rec"))
		| Nonrecursive -> (makeroot ("declarations", "nonrec")))
		in
		makeDistableBindings b returnable;
		returnable
	| Pstr_eval(a, _) -> let returnable = makeroot ("eval", "evaluations")
		in
		makeDistableExpression a returnable;
		returnable
	| Pstr_exception(_) -> makeroot ("exception", "definition")
	| _ -> raise (Failure "structure not converted")
and
makeDistableBindings vales topRoot = 
	match vales with
	| [] -> ()
	| x :: xs -> 
		let returnable = makeroot ("binding", "") in
		makeDistablePattern x.Parsetree.pvb_pat returnable;
		makeDistableExpression x.Parsetree.pvb_expr returnable;
		addchild topRoot returnable;
		makeDistableBindings xs topRoot
and
makeDistableExpression expre topRoot =
	match expre.Parsetree.pexp_desc with
	| Pexp_constant(_) -> addchild topRoot (makeroot ("espression", "constant.int"))
	| Pexp_ident(_) -> addchild topRoot (makeroot ("expression", "identifier"))
	| Pexp_fun(_, _, innpat, innexp) ->
		let returnable = makeroot ("expression","function") in
		makeDistablePattern innpat returnable;
		makeDistableExpression innexp returnable;
		addchild topRoot returnable
	| Pexp_match(innexp, caseslist) -> 
		let returnable = makeroot ("expression","match") in
		(makeDistableExpression innexp returnable);
		(makeDistableCases caseslist returnable);
		addchild topRoot returnable
	| Pexp_apply(func_exp, args_exps) ->
		let returnable = makeroot ("expression","application") in
		(makeDistableExpression func_exp returnable);
		(makeDistableArgs args_exps returnable);
		addchild topRoot returnable
	| Pexp_construct(name, args) -> 
		let returnable = makeroot ("expression","constructor") in
		(match name.Asttypes.txt with 
			| Longident.Lident(_) -> addchild returnable (makeroot ("constructor","identifier"))
			| _ -> addchild returnable (makeroot ("constructor","unknown")));
		(match args with
			| None -> ()
			| Some(inner_args) -> makeDistableExpression inner_args returnable
		);
		addchild topRoot returnable
	| Pexp_tuple(listicle) -> 
		let returnable = (makeroot ("expression","tuple"))
		in 
		makeDistableExpList listicle returnable;
		addchild topRoot returnable
	| Pexp_let(isrec, vlues, nextbit) -> (*TODO*)
		let returnable =  
		(match isrec with 
		| Recursive -> makeroot ("declarations", "rec")
		| Nonrecursive -> (makeroot ("declarations", "nonrec")))
			in
			(makeDistableBindings vlues returnable);
			(makeDistableExpression nextbit returnable);
			addchild topRoot returnable
	| Pexp_ifthenelse(a, b, c) -> 
		let returnable = makeroot ("expression","ifthenelse") in
		(makeDistableExpression a returnable);
		(makeDistableExpression b returnable);
		(match c with 
		| None -> ()
		| Some(optExpr) -> makeDistableExpression optExpr returnable);
		addchild topRoot returnable
	| Pexp_constraint(innerExpre, _) -> 
		let returnable = (makeroot ("expression","constraint") )
		in 
		(makeDistableExpression innerExpre returnable);
		addchild topRoot returnable
	| Pexp_assert(assertion) ->
		let returnable = makeroot ("expression", "assertion") in
		makeDistableExpression assertion returnable;
		addchild topRoot returnable
	| Pexp_function(caseslist) -> let returnable = makeroot ("expression","key function") in
		(makeDistableCases caseslist returnable);
		addchild topRoot returnable
	| Pexp_sequence(this_exp, next_exp) -> let returnable = makeroot ("expression", "sequence") in
		(makeDistableExpression this_exp returnable);
		(makeDistableExpression next_exp returnable);
		addchild topRoot returnable
	| Pexp_field(p_object, field_name) -> let returnable = makeroot ("expression","field") in
		makeDistableExpression p_object returnable;
		(match field_name.Asttypes.txt with 
			| Longident.Lident(_) -> addchild returnable (makeroot ("field","identifier"))
			| _ -> addchild returnable (makeroot ("field","unknown")));
		addchild topRoot returnable;
	| Pexp_setfield(p_object, field_name, new_value) -> let returnable = makeroot ("expression","setfield") in
		makeDistableExpression p_object returnable;
		(match field_name.Asttypes.txt with 
			| Longident.Lident(_) -> addchild returnable (makeroot ("field","identifier"))
			| _ -> addchild returnable (makeroot ("field","unknown")));
		makeDistableExpression new_value returnable;
		addchild topRoot returnable
	| Pexp_record (p_fields, other) -> let returnable = makeroot ("expression","record") in
		makeDistableRecordsExp p_fields returnable;
		(match other with
		| None -> ()
		| Some(expre) -> makeDistableExpression expre returnable);
		addchild topRoot returnable
	| Pexp_try (inn_code, caught_cases) -> let returnable = makeroot ("expression", "try") in
		makeDistableExpression inn_code returnable;
		makeDistableCases caught_cases returnable;
		addchild topRoot returnable
	
	| Pexp_variant _ -> print_endline "unimplemented expression: variant"
	
	| Pexp_array _ -> print_endline "unimplemented expression: array"
	
	| Pexp_while _ -> print_endline "unimplemented expression: while"
	| Pexp_for _ -> print_endline "unimplemented expression: for"
	| Pexp_coerce _ -> print_endline "unimplemented expression: coerce"
	| Pexp_send _ -> print_endline "unimplemented expression: send"
	| Pexp_new _ -> print_endline "unimplemented expression: new"
	| Pexp_setinstvar _ -> print_endline "unimplemented expression: setinstvar"
	| Pexp_override _ -> print_endline "unimplemented expression"
	| Pexp_letmodule _ -> print_endline "unimplemented expression"
	| Pexp_letexception _ -> print_endline "unimplemented expression"
	| Pexp_lazy _ -> print_endline "unimplemented expression"
	| Pexp_poly _ -> print_endline "unimplemented expression"
	| Pexp_object _ -> print_endline "unimplemented expression"
	| Pexp_newtype _ -> print_endline "unimplemented expression"
	| Pexp_pack _ -> print_endline "unimplemented expression"
	| Pexp_open _ -> print_endline "unimplemented expression"
	| Pexp_letop _ -> print_endline "unimplemented expression"
	| Pexp_extension _ -> print_endline "unimplemented expression"
	| Pexp_unreachable -> print_endline "unimplemented expression: unreachable"
and
makeDistablePattern (patte: Parsetree.pattern) topRoot = 
	match patte.Parsetree.ppat_desc with
	| Ppat_any -> addchild topRoot (makeroot ("pattern","any"))
	| Ppat_var(_) -> addchild topRoot (makeroot ("pattern","identifier"))
	| Ppat_construct(id, nxtpat) ->
		let returnable = makeroot ("pattern","constructor") in
		(match id.Asttypes.txt with 
		| Longident.Lident(_) -> addchild returnable (makeroot ("constructor","identifier"))
		| _ -> addchild returnable (makeroot ("constructor","unknown")));
		(match nxtpat with
		| None -> ()
		| Some(realpat) -> (makeDistablePattern realpat returnable)
		);
		addchild topRoot returnable
	| Ppat_tuple(patts) -> 
		let returnable = (makeroot ("pattern","tuple") )
		in 
		(makeDistablePatList patts returnable);
		addchild topRoot returnable
	| Ppat_constraint(innerPattern, _) -> 
		let returnable = (makeroot ("pattern","constraint") )
		in 
		(makeDistablePattern innerPattern returnable);
		addchild topRoot returnable
	| Ppat_constant(_) -> addchild topRoot (makeroot ("pattern", "constant"))
	| Ppat_or(pat1, pat2) -> let returnable = makeroot ("pattern", "or") in
		makeDistablePattern pat1 returnable;
		makeDistablePattern pat2 returnable
	| Ppat_record(pList, _) -> let returnable = makeroot ("pattern", "record") in
		makeDistableRecordsPat pList returnable;
		addchild topRoot returnable
	| Ppat_alias(rep_ee, _) -> let returnable = makeroot ("pattern", "alias") in
		addchild returnable (makeroot ("alias", "name"));
		makeDistablePattern rep_ee returnable;
		addchild topRoot returnable
	
	| Ppat_interval _ -> print_endline "unimplemented pattern: interval"
	| Ppat_variant _ -> print_endline "unimplemented pattern: variant"
	| Ppat_array _ -> print_endline "unimplemented pattern: array"
	| Ppat_type _ -> print_endline "unimplemented pattern: type"
	| Ppat_lazy _ -> print_endline "unimplemented pattern"
	| Ppat_unpack _ -> print_endline "unimplemented pattern"
	| Ppat_exception _ -> print_endline "unimplemented pattern"
	| Ppat_extension _ -> print_endline "unimplemented pattern"
	| Ppat_open _ -> print_endline "unimplemented pattern"
and
makeDistableCases (cases_l: Parsetree.case list) topRoot =
	match cases_l with
	| [] -> ()
	| x :: xs -> 
		let returnable = makeroot ("case", "") in
		makeDistablePattern x.Parsetree.pc_lhs returnable;
		makeDistableExpression x.Parsetree.pc_rhs returnable;
		addchild topRoot returnable;
		(makeDistableCases xs topRoot)
		
and
makeDistableArgs args_pair_list topRoot =
	match args_pair_list with 
	| [] -> ()
	| (_, x) :: xs ->
		makeDistableExpression x topRoot;
		makeDistableArgs xs topRoot
and
makeDistablePatList pat_list topRoot =
	match pat_list with
	| [] -> ()
	| x :: xs -> 
		makeDistablePattern x topRoot;
		makeDistablePatList xs topRoot
and
makeDistableExpList expre_list topRoot =
	match expre_list with
	| [] -> ()
	| x :: xs -> 
		makeDistableExpression x topRoot;
		makeDistableExpList xs topRoot
and
makeDistableRecordsExp p_list topRoot =
	match p_list with
	| [] -> ()
	| (p_id, p_exp) :: xs -> 
		let pField = 
		(match p_id.Asttypes.txt with 
		| Longident.Lident(_) -> makeroot ("field","identifier")
		| _ -> makeroot ("field","unknown"));
		in
		makeDistableExpression p_exp pField;
		addchild topRoot pField;
		makeDistableRecordsExp xs topRoot
and
makeDistableRecordsPat p_list topRoot =
	match p_list with
	| [] -> ()
	| (p_id, p_exp) :: xs -> 
		let pField = 
		(match p_id.Asttypes.txt with 
		| Longident.Lident(_) -> makeroot ("field","identifier")
		| _ -> makeroot ("field","unknown"));
		in
		makeDistablePattern p_exp pField;
		addchild topRoot pField;
		makeDistableRecordsPat xs topRoot
		
		
		



