
open Distance;;

let rec 
makeDistable parsa topRoot = 
	match parsa with
	| x :: xs -> 
		addchild topRoot (makeDistableStructure x);
		makeDistable xs topRoot
	| [] -> topRoot
and
makeDistableStructure (struc:Parsetree.structure_item) =
	match struc.Parsetree.pstr_desc with 
	| Pstr_value(a,b) -> 
		let returnable = makeroot "declarations" in
		(match a with 
		| Recursive -> addchild returnable (makeroot "rec")
		| Nonrecursive -> addchild returnable (makeroot "nonrec"));
		addchild returnable (makeDistableBindings b);
		returnable
	| Pstr_eval(_, _) -> makeroot "evaluation"
	| _ -> makeroot "unknown.structure"
and
makeDistableBindings vales = 
	match vales with
	| [] -> makeroot "endBindingList"
	| x :: xs -> 
		let returnable = makeroot "binding" in
		addchild returnable (makeDistablePattern x.Parsetree.pvb_pat);
		addchild returnable (makeDistableExpression x.Parsetree.pvb_expr);
		addchild returnable (makeDistableBindings xs);
		returnable
and
makeDistableExpression expre =
	match expre.Parsetree.pexp_desc with
	| Pexp_constant(_) -> makeroot "espression.constant.int"
	| Pexp_fun(_, _, innpat, innexp) ->
		let returnable = makeroot "expression.function" in
		addchild returnable (makeDistablePattern innpat);
		addchild returnable (makeDistableExpression innexp);
		returnable
	| Pexp_match(innexp, caseslist) -> 
		let returnable = makeroot "expression.match" in
		addchild returnable (makeDistableExpression innexp);
		addchild returnable (makeDistableCases caseslist);
		returnable
	| Pexp_ident(_) -> makeroot "expression.identifier"
	| Pexp_apply(func_exp, args_exps) ->
		let returnable = makeroot "expression.application" in
		addchild returnable (makeDistableExpression func_exp);
		addchild returnable (makeDistableArgs args_exps);
		returnable
	| Pexp_construct(name, args) -> 
		let returnable = makeroot "expression.constructor" in
		(match name.Asttypes.txt with 
			| Longident.Lident(_) -> addchild returnable (makeroot "constructor.identifier")
			| _ -> addchild returnable (makeroot "unknown.constructor.identifier"));
		(match args with
			| None -> ()
			| Some(inner_args) -> addchild returnable (makeDistableExpression inner_args)
		);
		returnable
	| Pexp_tuple(listicle) -> 
		let returnable = (makeroot "expression.tuple") 
		in 
		addchild returnable (makeDistableExpList listicle);
		returnable
	| Pexp_let(isrec, vlues, nextbit) -> 
		let returnable = makeroot "expression.declarations" in 
		(match isrec with 
		| Recursive -> addchild returnable (makeroot "rec")
		| Nonrecursive -> addchild returnable (makeroot "nonrec"));
		addchild returnable (makeDistableBindings vlues);
		addchild returnable (makeDistableExpression nextbit);
		returnable
	| _ -> makeroot "unknown.expression"
and
makeDistablePattern (patte: Parsetree.pattern) = 
	match patte.Parsetree.ppat_desc with
	| Ppat_var(_) -> makeroot "pattern.identifier"
	| Ppat_construct(id, nxtpat) ->
		let returnable = makeroot "pattern.constructor" in
		(match id.Asttypes.txt with 
		| Longident.Lident(_) -> addchild returnable (makeroot "constructor.identifier")
		| _ -> addchild returnable (makeroot "unknown.constructor.identifier"));
		(match nxtpat with
		| None -> ()
		| Some(realpat) -> addchild returnable (makeDistablePattern realpat)
		);
		returnable
	| Ppat_tuple(patts) -> 
		let returnable = (makeroot "pattern.tuple") 
		in 
		addchild returnable (makeDistablePatList patts);
		returnable
	| Ppat_constraint(innerPattern, _) -> 
		let returnable = (makeroot "pattern.constraint") 
		in 
		addchild returnable (makeDistablePattern innerPattern);
		returnable
	| _ -> makeroot "unknown.pattern"
and
makeDistableCases (cases_l: Parsetree.case list)=
	match cases_l with
	| [] -> makeroot "endCasesList"
	| x :: xs -> 
		let returnable = makeDistablePattern x.Parsetree.pc_lhs
		in
		addchild returnable (makeDistableExpression x.Parsetree.pc_rhs);
		addchild returnable (makeDistableCases xs);
		returnable
and
makeDistableArgs args_pair_list =
	match args_pair_list with 
	| [] -> makeroot "endArgsList"
	| (_, x) :: xs ->
		let returnable = makeDistableExpression x
		in
		addchild returnable (makeDistableArgs xs);
		returnable
and
makeDistablePatList pat_list =
	match pat_list with
	| [] -> makeroot "endPatternList"
	| x :: xs -> 
		let returnable = makeDistablePattern x
		in
		addchild returnable (makeDistablePatList xs);
		returnable
and
makeDistableExpList expre_list =
	match expre_list with
	| [] -> makeroot "endExpressionList"
	| x :: xs -> 
		let returnable = makeDistableExpression x
		in
		addchild returnable (makeDistableExpList xs);
		returnable






