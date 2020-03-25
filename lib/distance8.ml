
type 'a recordTree = {
	data: 'a; 
	mutable pos: int;
	mutable parent: 'a recordTree option;
	mutable lSib: 'a recordTree option;
	mutable rSib: 'a recordTree option;
	mutable fChi: 'a recordTree option;
	mutable lChiChi: 'a recordTree option;
}

let makeroot aValue: 'a recordTree = {data = aValue; pos = 1; parent = None; lSib = None; rSib = None; fChi = None; lChiChi = None;}

let rec addRightSibling (existingChild: 'a recordTree) (newChild: 'a recordTree) (hopefulParent: 'a recordTree)=
	match existingChild.rSib with
	| None -> (
		existingChild.rSib <- Some(newChild);
		newChild.parent <- Some(hopefulParent);
		newChild.lSib <- Some(existingChild)
	)
	| Some(tryAgain) -> addRightSibling tryAgain newChild hopefulParent


let addchild (parentNode: 'a recordTree) (childNode: 'a recordTree) =
	match parentNode.fChi with
	| None -> (
		parentNode.fChi <- Some(childNode);
		childNode.parent <- Some(parentNode)
	)
	| Some(tryAgain) -> addRightSibling tryAgain childNode parentNode;;

let getlChiChi anyNode = 
	match anyNode.lChiChi with
	| Some(newNode) -> newNode
	| None ->
		let rec aux cur =
		match cur.fChi with
		| None -> cur
		| Some(child) -> aux child
		in
		let x = aux anyNode in
		anyNode.lChiChi <- Some(x);
		x;;

let rec count rootNode =
	match rootNode with
	| None -> 0
	| Some(my_node) -> 1 + (count my_node.rSib) + (count my_node.fChi)
;;

let rec traverse rootNode n =
	(*match rootNode.data with (first, scond) ->*)
	Pprint.print_n n;
	print_string rootNode.data;
	print_string ":";(*
	print_string scond;
	print_string " ? ";*)
	print_int rootNode.pos;
	print_newline ();
	(match rootNode.fChi with
	| None -> ()
	| Some(childNode) -> traverse childNode (n+1)
	);
	(match rootNode.rSib with
	| None -> ()
	| Some(siblingNode) -> traverse siblingNode n
	)
;;

let applyPostorder tree = 
	let x = ref 1 in
	let rec aux cur = 
		(match cur.fChi with 
		| None -> ()
		| Some(child) -> aux child);
		cur.pos <- !x;
		x := !x + 1;
		(match cur.rSib with
		| None -> ()
		| Some(sibling) -> aux sibling)
	in
	aux tree;;


let getKeyRoots root = 
	let rec aux cur acc =
		match (cur.fChi, cur.rSib) with
		| (None, None) -> cur :: acc
		| (Some(kid), None) -> aux kid (cur :: acc)
		| (None, Some(sib)) -> cur :: (aux sib acc)
		| (Some(kid), Some(sib)) -> aux kid (cur :: (aux sib acc))
	in
	(List.filter (fun x -> (x.lSib != None)) (aux root []))
;;

let minList l1 =
	match l1 with 
	| [] -> -45
	| x :: xs -> List.fold_left (fun a b -> if a < b then a else b) (x) xs
;;

let main_treedistloop (tree1: 'a recordTree) (tree2: 'a recordTree) (costInsDel: 'a -> int) (costSwap: 'a -> 'a -> int): (int array) array =
	applyPostorder tree1;
	applyPostorder tree2;
	let final_dists = Array.make_matrix (count (Some(tree1))) (count (Some(tree2))) 0
	and
	temp_dists = Array.make_matrix ((count (Some(tree1))) + 1) ((count (Some(tree2))) + 1) 0
	in
	let resetArray subRoot1 subRoot2 =
		let num1 = (getlChiChi subRoot1).pos in let num2 = (getlChiChi subRoot2).pos in
		temp_dists.(num1 - 1).(num2 - 1) <- 0;
		let rec aux1 cur = 
			(match cur.fChi with 
			| None -> ()
			| Some(child) -> aux1 child);
			temp_dists.(cur.pos).(num2 - 1) <- temp_dists.(cur.pos - 1).(num2 - 1) + (costInsDel cur.data);
			(match cur.rSib with
			| None -> ()
			| Some(sibling) -> aux1 sibling)
		in
		aux1 subRoot1;
		let rec aux2 cur = 
			(match cur.fChi with 
			| None -> ()
			| Some(child) -> aux2 child);
			temp_dists.(num1 - 1).(cur.pos) <- temp_dists.(num1 - 1).(cur.pos - 1) + (costInsDel cur.data);
			(match cur.rSib with
			| None -> ()
			| Some(sibling) -> aux2 sibling)
		in
		aux2 subRoot2
	in
	let keyroots1 = getKeyRoots tree1 in
	let keyroots2 = getKeyRoots tree2 in
	let innerLoop subRoot1 subRoot2 =
		let rec auxI cur1 cur2 =
			(match cur2.fChi with 
			| None -> ()
			| Some(child) -> auxI cur1 child);
			
			let num1 = temp_dists.(cur1.pos).(cur2.pos - 1) + (costInsDel cur2.data)
			and num2 = temp_dists.(cur1.pos - 1).(cur2.pos) + (costInsDel cur1.data)
			in
			(
			if ((getlChiChi cur1).pos = (getlChiChi subRoot1).pos) && ((getlChiChi cur2).pos = (getlChiChi subRoot2).pos) then
				
				let num3 = temp_dists.(cur1.pos - 1).(cur2.pos - 1) + (costSwap cur1.data cur2.data) in 
				temp_dists.(cur1.pos).(cur2.pos) <- minList [num1; num2; num3];
				final_dists.(cur1.pos - 1).(cur2.pos - 1) <- temp_dists.(cur1.pos).(cur2.pos)
			else
				let num3 = temp_dists.((getlChiChi cur1).pos - 1).((getlChiChi cur2).pos - 1) + (final_dists.(cur1.pos - 1).(cur2.pos - 1)) in 
				temp_dists.(cur1.pos).(cur2.pos) <- minList [num1; num2; num3]
			);
			
			(match cur2.rSib with
			| None -> ()
			| Some(sibling) -> auxI cur1 sibling)
		and auxO cur =
			(match cur.fChi with 
			| None -> ()
			| Some(child) -> auxO child);
			
			auxI cur subRoot2;
			
			(match cur.rSib with
			| None -> ()
			| Some(sibling) -> auxO sibling)
		in
		auxO subRoot1
	in
	let outerLoop keys1 keys2 = 
		let rec auxI k keys2 =
			match keys2 with
			| l :: ls -> 
				resetArray k l;
				innerLoop k l;
				auxI k ls
			| [] -> ()
		in
		let rec auxO keys1 =
			match keys1 with
			| k :: ks -> 
				auxI k keys2;
				auxO ks
			| [] -> ()
		in
		auxO keys1
	in
	
	outerLoop (keyroots1 @ [tree1]) (keyroots2 @ [tree2]);
	final_dists
