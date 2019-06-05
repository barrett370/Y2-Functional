module Nat = struct
  type z = Z (*the type of zero in the natural numbers (base value*)
  type _ s = S : 'n -> 'n s (*Essentially the successor function*)
end 

module Pbt = struct
  open Nat

  type ('a,_) pbt = 
    |Leaf: ('a, z) pbt 
    |Node : 'a * ('a, 'n) pbt * ('a, 'n) pbt -> ('a, 'n s) pbt 

  let rec flip: type n . ('a, n) pbt -> ('a, n) pbt = function
    | Leaf -> Leaf
    |Node (a, tl,tr) -> Node (a, flip tr, flip tl)

  let rec sum : type n. (int, n) pbt -> (int, n) pbt -> (int, n )pbt = fun t1 t2 -> match t1,t2 with 
    |Leaf, Leaf -> Leaf 
    |Node (a, tl, tr), Node (a', tl',tr') -> Node (a+a', sum tl tl', sum tr tr')



end 

open Pbt 
let t = Node(1,Leaf,Leaf)
let s = (sum t t)

