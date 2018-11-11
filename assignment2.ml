

type zero

type one = One

type ('a,'b)sum = Left of 'a | Right of 'b

let f' p = (fun b -> p(Left b)
                   ,fun c -> p(Right c))

let g' (q,r) = function
  |Left b -> q b
  |Right c -> r c


(* let f zero =  fun b -> One *)


type ('a, 'b) product  = Pair of ('a * 'b)

(* let f = fun x -> Pair(fst x,snd x)

   let g = function
   |Pair(a,b) -> a*b
*)

type ('a,'b) exponential = 'a->'b;;



type zero

type one = One




let f'' = fun (x:(zero -> 'a)) -> One;;

let g'' One = (function (zero:zero) -> failwith "");;

f'' (fun zero->2) |>g'';;



let (f : ((zero->'a)->one)) = fun (x:(zero->'a)) -> One;;



let (g : (one ->(zero->'a))) = (fun zero -> failwith "");;








(* f''' (zero->2)  *)

type ('a,'b) sum = Left of 'a | Right of 'b
type ('a,'b) exp = 'a->'b;;
type two = true|false
(* (x:((  (  (  (  (two,'a)  exp  )  ,  (  (two,'a)  product)  )  sum  ), One  )sum) ) *)

(* let x = (x:(((((two,'a)exp,(two,'a)product)sum),One)sum)) in x *)

let x = fun (x:(
    ((((two,'a)exp,(two,'a)product)sum),one)sum
  ))->x;;
let f'' x = (x:(
    (((two,'a)exp),((two,'a)product)option)sum)
  );;
type two = true|false;;
(* type 'a option = None | Some of 'a *)
(* let f y x:(('a option)->two) = function
   |None-> (y:(
    ((((two,'a)exp),((two,'a)product)option)sum)
        ))
   |Some x -> x *)
(* let g x:((((two,'a)exp),((two,'a)product))sum) = x *)

type ('a,'b,'c)sum3 = A of 'a | B of 'b |C of 'c

type 'a t  = two -> ('a option)
type 'a t'  = (two -> 'a, two * 'a,one)sum3

let (f: 'a t -> 'a t') = function
  |true -> ((two->'a),(two*'a),one)sum3

(* let (g: 'a t' -> 'a t) = ... *)
