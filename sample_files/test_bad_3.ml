
let mapTree f (t: 'a tree) =
match t with
| Empty -> Empty
| Node(l,v,r) -> Node(f l, v, f r);;
