



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


let getNodeMinusOne anyNode =
	match anyNode with TreeNode(_, _, par, psib, _, _) -> (
		match !psib with
		| None -> !par
		| Some(tryAgain) -> Some(getRightChildChild tryAgain)
	);;

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
		| None -> 0
		| Some(TreeNode(_, num, _, _, _, _)) -> !num
;;

let getNodeNum anyNodeOp =
	match anyNodeOp with | Some(TreeNode(_, num, _, _, _, _)) -> !num
		| None -> 0
;;

let distanceE tree1 tree2 =
	generatePreNum tree1; generatePreNum tree2;
	
	let rec distanceE treeS treeU treeI treeT treeV treeJ accum =
		match treeI with TreeNode(_, numI, _, _, _, _) -> match treeJ with TreeNode(_, numJ, _, _, _, _) ->
		match treeU with TreeNode(_, numU, parU, _, _, _) -> match treeS with TreeNode(_, numS, parS, _, _, _) ->
		match treeV with TreeNode(_, numV, parV, _, _, _) -> match treeT with TreeNode(_, numT, parT, _, _, _) ->
		let newAccum =
			(if (!numS = !numU && !numI = !numU) && (!numT = !numV && !numJ = !numV) then
				(fun coords -> match coords with 
					(s, u, i, t, v, j) -> if (s = !numS && u = !numU && i = !numI && t = !numT && v = !numT && j = !numJ) 
					then 1 else accum coords)
			else 
			if (!numS = !numU && !numI = !numU) || (!numT < !numV && !numJ = !numV) then
				let rVal = (accum (!numS, !numU, !numI, !numT, getParentNum(treeJ), !numJ - 1)) + 1 in
				fun coords -> match coords with 
					(s, u, i, t, v, j) -> if (s = !numS && u = !numU && i = !numI && t = !numT && v = !numT && j = !numJ) 
					then rVal else accum coords
			else 
			if (!numS < !numU && !numI = !numU) || (!numT = !numV && !numJ = !numV) then
				let rVal = (accum (!numS, getParentNum(treeI), !numI - 1, !numT, !numV, !numJ)) + 1 in
				fun coords -> match coords with 
					(s, u, i, t, v, j) -> if (s = !numS && u = !numU && i = !numI && t = !numT && v = !numT && j = !numJ) 
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
				(s, u, i, t, v, j) -> if (s = !numS && u = !numU && i = !numI && t = !numT && v = !numT && j = !numJ) 
					then rVal else accum coords)
		in
		match !parT with
		| Some(parentT) -> distanceE treeS treeU treeI parentT treeV treeJ newAccum
		| None ->
		match !parV with
		| Some(parentV) -> distanceE treeS treeU treeI parentV parentV treeJ newAccum
		| None ->
		match !parS with
		| Some(parentS) -> distanceE parentS treeU treeI treeJ treeJ treeJ newAccum
		| None ->
		match !parU with
		| Some(parentU) -> distanceE parentU parentU treeI treeJ treeJ treeJ newAccum
		| None ->
		match (getNodePlusOne treeJ) with
		| Some(nextJ) -> distanceE treeI treeI treeI nextJ nextJ nextJ newAccum
		| None ->
		match (getNodePlusOne treeI) with
		| Some(nextI) -> distanceE nextI nextI nextI tree2 tree2 tree2 newAccum
		| None -> newAccum
	in
	distanceE tree1 tree1 tree1 tree2 tree2 tree2 (fun _ -> 38)
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
generatePreNum root;

let root2 = makeroot "also root" in
addchild root2 (makeroot "newleaf");
addchild root2 (makeroot "newleaf2");
addchild root2 (makeroot "newleaf3");
addchild root2 (makeroot "newleaf4");


let bigE = distanceE root root2 in
print_int (bigE (1, 7, 7, 1, 1, 5));
print_newline ();











