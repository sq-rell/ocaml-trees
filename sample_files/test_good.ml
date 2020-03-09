
let rec mapTree f (t: 'a tree) =
match t with
| Empty -> Empty
| Node(l,v,r) -> Node((mapTree f l), (f v), (mapTree f r));;
