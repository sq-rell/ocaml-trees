



type 'a tree = TreeNode of 'a * int ref * 'a tree option ref * 'a tree option ref * 'a tree option ref * 'a tree option ref

let printnode (anyNode: 'a tree option) = 
	match anyNode with 
	| None -> print_endline "Null branch found"
	| Some(TreeNode(x, _, _, _, _, _)) -> print_endline x

let makeroot aValue: 'a tree = TreeNode(aValue, ref 1, ref None, ref None, ref None, ref None)

let rec addsibling (existingChild: 'a tree) (newChild: 'a tree) (hopefulParent: 'a tree)=
	match existingChild with TreeNode(_, _, _, _, nextSib, _) ->
		match !nextSib with
		| None -> (
			nextSib := Some(newChild);
			match newChild with TreeNode(_, _, par, psib, _, _) -> (
				par := Some(hopefulParent);
				psib := Some(existingChild)
			)
		)
		| Some(tryAgain) -> addsibling tryAgain newChild hopefulParent


let addchild (parentNode: 'a tree) (childNode: 'a tree) =
	match parentNode with TreeNode(_, _, _, _, _, existingChild) -> 
		match !existingChild with
		| None -> (
			existingChild := Some(childNode);
			match childNode with TreeNode(_, _, par, _, _, _) -> par := Some(parentNode)
		)
		| Some(tryAgain) -> addsibling tryAgain childNode parentNode;;

let getFirstChild (parentNode: 'a tree) =
	match parentNode with TreeNode(_, _, _, _, _, x) -> !x;;

let rec getRightSibling existingChild =
	match existingChild with TreeNode(_, _, _, _, x, _) -> 
		match !x with
		| None -> existingChild
		| Some(tryAgain) -> getRightSibling tryAgain;;

let rec getRightChildChild parentNode = 
	let x = getFirstChild parentNode
	in
	match x with
	| None -> parentNode
	| Some(tryAgain) -> getRightChildChild(getRightSibling tryAgain);;

(*
let getNodeMinusOne anyNode =
	match anyNode with TreeNode(_, _, par, psib, _, _) -> (
		match !psib with
		| None -> !par
		| Some(tryAgain) -> Some(getRightChildChild tryAgain)
	)
;;
*)

let getNodePlusOne anyNode =
	let rec getRightAunt someNode =
		match someNode with TreeNode(_, _, par, _, nsib, _) ->
			match !nsib with 
			| Some(nextSibling) -> Some(nextSibling)
			| None -> match !par with 
				| None -> None
				| Some(parent) -> getRightAunt parent
	in
	match anyNode with TreeNode(_, _, _, _, _, fchi) -> 
		match !fchi with
		| Some(firstChild) -> Some(firstChild)
		| None -> getRightAunt anyNode

let generatePreNum rootNode = 
	let rec helper rootNode acc =
		match rootNode with TreeNode(_, num, _, _, nsib, fchi) ->
		num := (!acc);
		acc := !acc + 1;
			(match !fchi with
			| None -> ()
			| Some(childNode) -> helper childNode acc
			);
			(match !nsib with
			| None -> ()
			| Some(siblingNode) -> helper siblingNode acc
			)
	in
	helper rootNode (ref 0);;

let rec count rootNode =
	match rootNode with
	| None -> 0
	| Some(TreeNode(_, _, _, _, nsib, fchi)) -> 1 + (count !nsib) + (count !fchi)
;;

let rec getDirectedChild fromAncestor toChild =
	match toChild with TreeNode(_, _, par, _, _, _) ->
		match !par with
		| None -> None
		| Some(parentNode) -> match parentNode with TreeNode(_, num1, _, _, _, _) ->
			match fromAncestor with TreeNode(_, num2, _, _, _, _) ->
				if !num1 = !num2 then Some(toChild) else getDirectedChild fromAncestor parentNode
;;


let rec traverse rootNode n =
	match rootNode with TreeNode(aValue, num, _, _, nsib, fchi) ->
		Pprint.print_n n;
		print_string aValue;
		print_string ": ";
		print_int !num;
		print_newline ();
		(match !fchi with
		| None -> ()
		| Some(childNode) -> traverse childNode (n+1)
		);
		(match !nsib with
		| None -> ()
		| Some(siblingNode) -> traverse siblingNode n
		)
;;


let getParentNum anyNode =
	match anyNode with TreeNode(_, _, par, _, _, _) ->
		match !par with
		| None -> 420
		| Some(TreeNode(_, num, _, _, _, _)) -> !num
;;

let getNodeNum anyNodeOp =
	match anyNodeOp with | Some(TreeNode(_, num, _, _, _, _)) -> !num
		| None -> 0
;;

let minList l1 =
	match l1 with 
	| [] -> -45
	| x :: xs -> List.fold_left (fun a b -> if a < b then a else b) (x) xs
;;

let costSwap str1 str2 =
	if str1 = str2 then (0) else 2
;;

let distanceE tree1 tree2 =
	generatePreNum tree1; generatePreNum tree2;
	let size1 = count (Some(tree1)) and size2 = count (Some(tree2))
	in
	
	let returnable = Array.make_matrix size1 size2 (fun _ -> -1)
	in
	let rec helperE treeS treeU treeI treeT treeV treeJ accum =
		match treeI with TreeNode(valI, numI, _, _, _, _) -> match treeJ with TreeNode(valJ, numJ, _, _, _, _) ->
		match treeU with TreeNode(_, numU, parU, _, _, _) -> match treeS with TreeNode(_, numS, parS, _, _, _) ->
		match treeV with TreeNode(_, numV, parV, _, _, _) -> match treeT with TreeNode(_, numT, parT, _, _, _) ->
		
		(*
		print_int !numS;
		print_int !numU;
		print_int !numI;
		print_int !numT;
		print_int !numV;
		print_int !numJ;
		print_newline ();
		*)
		
		let rval =
		
		(if (!numS = !numU && !numI = !numU) && (!numT = !numV && !numJ = !numV) then
			(costSwap valI valJ)
		else 
		(if (!numS = !numU && !numI = !numU) || (!numT < !numV && !numJ = !numV) then
			(accum.(!numI).(!numJ - 1) (!numS, !numU, !numT, getParentNum(treeJ))) + 1
		else 
		(if (!numS < !numU && !numI = !numU) || (!numT = !numV && !numJ = !numV) then
			(accum.(!numI - 1).(!numJ) (!numS, getParentNum(treeI), !numT, !numV)) + 1
		else
		let numX = getNodeNum (getDirectedChild treeU treeI) in
		let numY = getNodeNum (getDirectedChild treeV treeJ) in
		let rVal1 = accum.(!numI).(!numJ) (!numS, numX, !numT, !numV) in
		let rVal2 = accum.(!numI).(!numJ) (!numS, !numU, !numT, numY) in
		let rVal3 = (accum.(numX - 1).(numY - 1) (!numS, !numU, !numT, !numV)) + (accum.(!numI).(!numJ) (numX, numX, numY, numY)) in
		if rVal1 < rVal2 
			then (if rVal1 < rVal3 then rVal1 else rVal3)
			else (if rVal2 < rVal3 then rVal2 else rVal3))))
		in
		let oldAccum = accum.(!numI).(!numJ) in
		accum.(!numI).(!numJ) <- (fun (s,u,t,v) -> if (s = !numS && u = !numU && t = !numT && v = !numV) then rval else oldAccum (s,u,t,v));
		
		
		match !parT with
		| Some(parentT) -> helperE treeS treeU treeI parentT treeV treeJ accum
		| None ->
		match !parV with
		| Some(parentV) -> helperE treeS treeU treeI parentV parentV treeJ accum
		| None ->
		match !parS with
		| Some(parentS) -> helperE parentS treeU treeI treeJ treeJ treeJ accum
		| None ->
		match !parU with
		| Some(parentU) -> helperE parentU parentU treeI treeJ treeJ treeJ accum
		| None ->
		match (getNodePlusOne treeJ) with
		| Some(nextJ) -> helperE treeI treeI treeI nextJ nextJ nextJ accum
		| None ->
		match (getNodePlusOne treeI) with
		| Some(nextI) -> helperE nextI nextI nextI tree2 tree2 tree2 accum
		| None -> ()
	in
	helperE tree1 tree1 tree1 tree2 tree2 tree2 returnable;
	returnable
;;

let getUnwrappedParent bodyNode = 
	match bodyNode with TreeNode(_, _, par, _, _, _) ->
		match !par with
		| None -> raise (Invalid_argument "expected node to have a parent")
		| Some(parent) -> parent
;;

let distanceMINM tree1 tree2 =
	generatePreNum tree1; generatePreNum tree2;
	let size1 = count (Some(tree1)) and size2 = count (Some(tree2)) in
	let returnable = Array.make_matrix size1 size2 0
	in
	(*
	match getNodePlusOne tree1 with 
	| None -> (fun _ -> raise Not_found)
	| Some(tree1node2) -> 
	match getNodePlusOne tree2 with 
	| None -> (fun _ -> raise Not_found)
	| Some(tree2node2) -> 
	*)
	let matrixE = distanceE tree1 tree2 in
	let rec innerhelperMINM treeI treeJ treeS treeT accum matrixMINM =
		match treeI with TreeNode(_, numI, _, _, _, _) -> match treeJ with TreeNode(_, numJ, parJ, _, _, _) ->
		match treeS with TreeNode(valS, numS, parS, _, _, _) -> match treeT with TreeNode(valT, numT, parT, _, _, _) -> 
		let newAccum = ((matrixMINM.(!numS).(!numT)) + (matrixE.(!numI - 1).(!numJ - 1) (!numS, getParentNum treeI, !numT, getParentNum treeJ)) - (costSwap valS valT)) :: accum
		in
		match !parT with
		| Some(parentT) -> innerhelperMINM treeI treeJ treeS parentT newAccum matrixMINM
		| None ->
		match !parS with
		| Some(parentS) -> (match !parJ with 
			| None -> 117
			| Some(parentJ) -> innerhelperMINM treeI treeJ parentS parentJ newAccum matrixMINM)
		| None -> minList newAccum
	in
	let rec outerhelperMINM treeI treeJ accum =
		match treeI with TreeNode(valI, numI, _, _, _, _) -> match treeJ with TreeNode(valJ, numJ, _, _, _, _) ->
		let rval = 
		if !numI = 0 then
			if !numJ = 0 then
				(costSwap valI valJ)
			else
				1 + accum.(!numI).(!numJ - 1)
		else
			if !numJ = 0 then
				1 + accum.(!numI - 1).(!numJ)
			else
				let innerValue = innerhelperMINM treeI treeJ (getUnwrappedParent treeI) (getUnwrappedParent treeJ) [] accum
				in
				innerValue + (costSwap valI valJ)
		in
		accum.(!numI).(!numJ) <- rval;
		match getNodePlusOne treeJ with 
		| Some(nextJ) -> outerhelperMINM treeI nextJ accum
		| None -> 
		match getNodePlusOne treeI with
		| Some(nextI) -> outerhelperMINM nextI tree2 accum
		| None -> ()
	in
	outerhelperMINM tree1 tree2 returnable;
	returnable
;;


let distanceD tree1 tree2 = 
	let matrixMINM = distanceMINM tree1 tree2
	in
	generatePreNum tree1; generatePreNum tree2;
	let size1 = count (Some(tree1)) and size2 = count (Some(tree2)) in
	let returnable = Array.make_matrix size1 size2 0
	in
	let rec helper treeI treeJ accum = 
		match treeI with TreeNode(valI, numI, _, _, _, _) -> match treeJ with TreeNode(valJ, numJ, _, _, _, _) ->
		let rval = 
		if !numI = 0 then
			if !numJ = 0 then
				(costSwap valI valJ)
			else
				1 + accum.(!numI).(!numJ - 1)
		else
			if !numJ = 0 then
				1 + accum.(!numI - 1).(!numJ)
			else
				minList [accum.(!numI).(!numJ - 1) + 1; accum.(!numI - 1).(!numJ) + 1; matrixMINM.(!numI).(!numJ)]
		in
		accum.(!numI).(!numJ) <- rval;
		match getNodePlusOne treeJ with 
		| Some(nextJ) -> helper treeI nextJ accum
		| None -> 
		match getNodePlusOne treeI with
		| Some(nextI) -> helper nextI tree2 accum
		| None -> ()
	in
	helper tree1 tree2 returnable;
	returnable
;;



(*

let root = makeroot "root";;

let inter1 = makeroot "inter1"
in
let leaf1 = makeroot "leaf1" in
addchild inter1 leaf1;
addchild inter1 (makeroot "leaf2");
addchild root inter1;
let inter2 = makeroot "inter2"
in
addchild inter2 (makeroot "leaf3");
addchild inter2 (makeroot "leaf4");
addchild root inter2;;


let root2 = makeroot "a";;

let inter3 = makeroot "b"
in
let leafk = makeroot "treeee" in
addchild inter3 leafk;
addchild inter3 (makeroot "d");
addchild root2 inter3;
let inter4 = makeroot "e"
in
addchild inter4 (makeroot "q");
addchild inter4 (makeroot "inter2");
addchild root2 inter4;;

*)
(*
let bigE = distanceE root root2 in
print_int (bigE (1, 4, 4, 1, 1, 4));

let bigMINM = distanceMINM root root2 in
print_int (bigMINM (7, 7));
*)






