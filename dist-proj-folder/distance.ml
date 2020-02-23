



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
	helper rootNode (ref 1);;

let rec getDirectedChild fromAncestor toChild =
	match toChild with TreeNode(_, _, par, _, _, _) ->
		match !par with
		| None -> None
		| Some(parentNode) -> match parentNode with TreeNode(_, num1, _, _, _, _) ->
			match fromAncestor with TreeNode(_, num2, _, _, _, _) ->
				if !num1 = !num2 then Some(toChild) else getDirectedChild fromAncestor parentNode
;;

let rec traverse rootNode =
	match rootNode with TreeNode(aValue, num, _, _, nsib, fchi) ->
		print_string aValue;
		print_string ": ";
		print_int !num;
		print_newline ();
		(match !fchi with
		| None -> ()
		| Some(childNode) -> traverse childNode
		);
		(match !nsib with
		| None -> ()
		| Some(siblingNode) -> traverse siblingNode
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
	if String.equal str1 str2 then (1) else 2
;;

let distanceE tree1 tree2 =
	generatePreNum tree1; generatePreNum tree2;
	
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
		
		let newAccum =
			(if (!numS = !numU && !numI = !numU) && (!numT = !numV && !numJ = !numV) then
				(fun coords -> match coords with 
					(s, u, i, t, v, j) -> if (s = !numS && u = !numU && i = !numI && t = !numT && v = !numV && j = !numJ) 
					then (costSwap valI valJ) else accum coords)
			else 
			if (!numS = !numU && !numI = !numU) || (!numT < !numV && !numJ = !numV) then
				let rVal = (accum (!numS, !numU, !numI, !numT, getParentNum(treeJ), !numJ - 1)) + 1 in
				fun coords -> match coords with 
					(s, u, i, t, v, j) -> if (s = !numS && u = !numU && i = !numI && t = !numT && v = !numV && j = !numJ) 
					then rVal else accum coords
			else 
			if (!numS < !numU && !numI = !numU) || (!numT = !numV && !numJ = !numV) then
				let rVal = (accum (!numS, getParentNum(treeI), !numI - 1, !numT, !numV, !numJ)) + 1 in
				fun coords -> match coords with 
					(s, u, i, t, v, j) -> if (s = !numS && u = !numU && i = !numI && t = !numT && v = !numV && j = !numJ) 
					then rVal else accum coords
			else
			let numX = getNodeNum (getDirectedChild treeU treeI) in
			let numY = getNodeNum (getDirectedChild treeV treeJ) in
			let rVal1 = accum (!numS, numX, !numI, !numT, !numV, !numJ) in
			let rVal2 = accum (!numS, !numU, !numI, !numT, numY, !numJ) in
			let rVal3 = (accum (!numS, !numU, numX - 1, !numT, !numV, numY - 1)) + (accum (numX, numX, !numI, numY, numY, !numJ)) in
			let rVal = if rVal1 < rVal2 
				then (if rVal1 < rVal3 then rVal1 else rVal3)
				else (if rVal2 < rVal3 then rVal2 else rVal3)
			in
			fun coords -> match coords with 
				(s, u, i, t, v, j) -> if (s = !numS && u = !numU && i = !numI && t = !numT && v = !numV && j = !numJ) 
					then rVal else accum coords)
		in
		match !parT with
		| Some(parentT) -> helperE treeS treeU treeI parentT treeV treeJ newAccum
		| None ->
		match !parV with
		| Some(parentV) -> helperE treeS treeU treeI parentV parentV treeJ newAccum
		| None ->
		match !parS with
		| Some(parentS) -> helperE parentS treeU treeI treeJ treeJ treeJ newAccum
		| None ->
		match !parU with
		| Some(parentU) -> helperE parentU parentU treeI treeJ treeJ treeJ newAccum
		| None ->
		match (getNodePlusOne treeJ) with
		| Some(nextJ) -> helperE treeI treeI treeI nextJ nextJ nextJ newAccum
		| None ->
		match (getNodePlusOne treeI) with
		| Some(nextI) -> helperE nextI nextI nextI tree2 tree2 tree2 newAccum
		| None -> newAccum
	in
	helperE tree1 tree1 tree1 tree2 tree2 tree2 (fun _ -> raise Not_found)
;;

let getUnwrappedParent bodyNode = 
	match bodyNode with TreeNode(_, _, par, _, _, _) ->
		match !par with
		| None -> raise (Invalid_argument "expected node to have a parent")
		| Some(parent) -> parent
;;

let distanceMINM tree1 tree2 =
	generatePreNum tree1; generatePreNum tree2;
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
		let newAccum = ((matrixMINM (!numS, !numT)) + (matrixE (!numS, getParentNum treeI, !numI - 1, !numT, getParentNum treeJ, !numJ - 1)) - (costSwap valS valT)) :: accum
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
		let newAccum = 
		if !numI = 1 then
			if !numJ = 1 then
				fun (x,y) -> if x = 1 && y = 1 then (costSwap valI valJ) else accum (x,y)
			else
				fun (x,y) -> if x = !numI && y = !numJ then 1 + accum (x, y-1) else accum (x,y)
		else
			if !numJ = 1 then
				fun (x,y) -> if x = !numI && y = !numJ then 1 + accum (x-1, y) else accum (x,y)
			else
				let innerValue = innerhelperMINM treeI treeJ (getUnwrappedParent treeI) (getUnwrappedParent treeJ) [] accum
				in
				fun (x,y) -> if (x = !numI) && (y = !numJ) then innerValue + (costSwap valI valJ) else (accum (x,y))
		in
		match getNodePlusOne treeJ with 
		| Some(nextJ) -> outerhelperMINM treeI nextJ newAccum
		| None -> 
		match getNodePlusOne treeI with
		| Some(nextI) -> outerhelperMINM nextI tree2 newAccum
		| None -> newAccum
	in
	outerhelperMINM tree1 tree2 (fun _ -> 100)
;;

let distanceD tree1 tree2 = 
	let matrixMINM = distanceMINM tree1 tree2
	in
	generatePreNum tree1; generatePreNum tree2;
	let rec helper treeI treeJ accum = 
		match treeI with TreeNode(valI, numI, _, _, _, _) -> match treeJ with TreeNode(valJ, numJ, _, _, _, _) ->
		let newAccum = 
		if !numI = 1 then
			if !numJ = 1 then
				fun (x,y) -> if x = 1 && y = 1 then (costSwap valI valJ) else accum (x,y)
			else
				fun (x,y) -> if x = !numI && y = !numJ then 1 + accum (x, y-1) else accum (x,y)
		else
			if !numJ = 1 then
				fun (x,y) -> if x = !numI && y = !numJ then 1 + accum (x-1, y) else accum (x,y)
			else
				let rVal = minList [accum (!numI, !numJ - 1) + 1; accum (!numI - 1, !numJ) + 1; matrixMINM (!numI, !numJ)]
				in
				fun (x,y) -> if x = !numI && y = !numJ then rVal else accum (x,y)
		in
		match getNodePlusOne treeJ with 
		| Some(nextJ) -> helper treeI nextJ newAccum
		| None -> 
		match getNodePlusOne treeI with
		| Some(nextI) -> helper nextI tree2 newAccum
		| None -> newAccum
	in
	helper tree1 tree2 (fun _ -> 102)
;;





let root = makeroot "root"
in
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
addchild root inter2;


let root2 = makeroot "a"
in
let inter3 = makeroot "b"
in
let leafk = makeroot "x" in
addchild inter3 leafk;
addchild inter3 (makeroot "d");
addchild root2 inter3;
let inter4 = makeroot "e"
in
addchild inter4 (makeroot "q");
addchild inter4 (makeroot "root");
addchild root2 inter4;


(*
let bigE = distanceE root root2 in
print_int (bigE (1, 4, 4, 1, 1, 4));

let bigMINM = distanceMINM root root2 in
print_int (bigMINM (7, 7));
*)
let bigD = distanceD root root2 in
print_int (bigD (7, 7));



print_newline ()










