
open Distance;;

let rec 
makeDistable parsa topRoot = 
	match parsa with
	| x :: xs -> 
		 (makeDistableStructure x topRoot);
		makeDistable xs topRoot
	| [] -> ()
and
makeDistableStructure (struc:Parsetree.structure_item) topRoot =
	match struc.Parsetree.pstr_desc with 
	| Pstr_value(a,b) -> 
		let returnable = makeroot "declarations" in
		(match a with 
		| Recursive -> addchild returnable (makeroot "rec")
		| Nonrecursive -> addchild returnable (makeroot "nonrec"));
		makeDistableBindings b returnable;
		addchild topRoot returnable
	| Pstr_eval(a, _) -> makeDistableExpression a topRoot
	| _ -> addchild topRoot (makeroot "unknown.structure")
and
makeDistableBindings vales topRoot = 
	match vales with
	| [] -> ()
	| x :: xs -> 
		let returnable = makeroot "binding" in
		makeDistablePattern x.Parsetree.pvb_pat returnable;
		makeDistableExpression x.Parsetree.pvb_expr returnable;
		addchild topRoot returnable;
		makeDistableBindings xs topRoot
and
makeDistableExpression expre topRoot =
	match expre.Parsetree.pexp_desc with
	| Pexp_constant(_) -> addchild topRoot (makeroot "espression.constant.int")
	| Pexp_ident(_) -> addchild topRoot (makeroot "expression.identifier")
	| Pexp_fun(_, _, innpat, innexp) ->
		let returnable = makeroot "expression.function" in
		makeDistablePattern innpat returnable;
		makeDistableExpression innexp returnable;
		addchild topRoot returnable
	| Pexp_match(innexp, caseslist) -> 
		let returnable = makeroot "expression.match" in
		(makeDistableExpression innexp returnable);
		(makeDistableCases caseslist returnable);
		addchild topRoot returnable
	| Pexp_apply(func_exp, args_exps) ->
		let returnable = makeroot "expression.application" in
		(makeDistableExpression func_exp returnable);
		(makeDistableArgs args_exps returnable);
		addchild topRoot returnable
	| Pexp_construct(name, args) -> 
		let returnable = makeroot "expression.constructor" in
		(match name.Asttypes.txt with 
			| Longident.Lident(_) -> addchild returnable (makeroot "constructor.identifier")
			| _ -> addchild returnable (makeroot "unknown.constructor.identifier"));
		(match args with
			| None -> ()
			| Some(inner_args) -> makeDistableExpression inner_args returnable
		);
		addchild topRoot returnable
	| Pexp_tuple(listicle) -> 
		let returnable = (makeroot "expression.tuple")
		in 
		makeDistableExpList listicle returnable;
		addchild topRoot returnable
	| Pexp_let(isrec, vlues, nextbit) -> 
		let returnable = makeroot "expression.declarations" in 
		(match isrec with 
		| Recursive -> addchild returnable (makeroot "rec")
		| Nonrecursive -> addchild returnable (makeroot "nonrec"));
		(makeDistableBindings vlues returnable);
		(makeDistableExpression nextbit returnable);
		addchild topRoot returnable
	| _ -> addchild topRoot (makeroot "unknown.expression")
and
makeDistablePattern (patte: Parsetree.pattern) topRoot = 
	match patte.Parsetree.ppat_desc with
	| Ppat_any -> addchild topRoot (makeroot "pattern.any")
	| Ppat_var(_) -> addchild topRoot (makeroot "pattern.identifier")
	| Ppat_construct(id, nxtpat) ->
		let returnable = makeroot "pattern.constructor" in
		(match id.Asttypes.txt with 
		| Longident.Lident(_) -> addchild returnable (makeroot "constructor.identifier")
		| _ -> addchild returnable (makeroot "unknown.constructor.identifier"));
		(match nxtpat with
		| None -> ()
		| Some(realpat) -> (makeDistablePattern realpat returnable)
		);
		addchild topRoot returnable
	| Ppat_tuple(patts) -> 
		let returnable = (makeroot "pattern.tuple") 
		in 
		(makeDistablePatList patts returnable);
		addchild topRoot returnable
	| Ppat_constraint(innerPattern, _) -> 
		let returnable = (makeroot "pattern.constraint") 
		in 
		(makeDistablePattern innerPattern returnable);
		addchild topRoot returnable
	| _ -> addchild topRoot (makeroot "unknown.pattern")
and
makeDistableCases (cases_l: Parsetree.case list) topRoot =
	match cases_l with
	| [] -> ()
	| x :: xs -> 
		let returnable = makeroot "case" in
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






