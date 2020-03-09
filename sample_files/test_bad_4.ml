
let rec mapTree f (t: 'a tree) =
if t = Empty then Empty else
match t with 
Node(l,v,r) -> Node((mapTree f l), (f v), (mapTree f r));;
