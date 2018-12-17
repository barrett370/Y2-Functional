
type zero = Zero : zero


type 'n succ = Succ : 'n -> 'n succ;;


type ('a, _) pbt =
  |Empty : ('a, zero) pbt
  | Tree : ('a, 'n) pbt * 'a * ('a, 'n) pbt -> ('a, 'n succ) pbt ;;


let rec flip : type a n.(a,n) pbt -> (a,n) pbt = function
    Empty ->  Empty
  | Tree (lt,v,rt) -> Tree (flip rt, v, flip lt);;





let rec add : type n.((int,n) pbt * (int,n) pbt) -> (int,n) pbt = function 
  |x, Empty -> x 
  |Tree (xlt, xv, xrt), Tree (ylt, yv, yrt) -> Tree ((xlt,ylt)|>add, (xv + yv) , (xrt,yrt)|>add)
