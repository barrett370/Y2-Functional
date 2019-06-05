
(* (zero -> 'b) iso () *)
type zero

type one = One 



let g (y:one) = fun (x:zero) -> failwith"g";;

let f (x:zero->'b) = One ;;

g One |>f;;
failwith"g" |> f |> g  


(* bool -> ('a option)  iso (bool -> 'a, (bool*'a), One)sum3 *)
type ('a,'b)product = Pair of ('a * 'b);; 

type ('a,'b,'c) sum3 = A of 'a | B of 'b | C of 'c ;;

type 'a t = bool -> ('a option)

type 'a t' = ( (bool->'a), (bool*'a), one) sum3 

let f': 'a t-> 'a t' = fun (p:'a t) ->
  match p true, p false with 
  |Some x,Some x'-> A (fun b -> if b then x else x' )
  |Some x, None -> B (true,x)
  |None, Some x' -> B (false, x')
  |None, None -> C One 

let g' : 'a t' -> 'a t =  function
  |A f -> fun b -> Some (f b) 
  |B (true,x) -> fun b-> if b then Some x else None 
  |B (false,x) -> fun b -> if b then None else Some x 
  |C One -> fun b-> None 




