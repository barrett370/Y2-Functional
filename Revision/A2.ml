(* b^0 =1 

    'b -> zero= ()*)



type one = One 
type zero

let f (a:one) = fun (b:zero) -> failwith "f";;
let g (p:zero-> 'b) = One;;

f One |>g;;
failwith"f"  |> g |> f ;;



(* (a+1)^2 = a^2 +2a + 1  *)

(* (two->('a,one)sum) -> (((two->a) , (bool,'a)product)sum , one)sum *)


;;

type ('a,'b)sum = Left of 'a | Right of 'b;; 

type ('a,'b)product = Pair of ('a * 'b);; 

type ('a,'b,'c)sum3 = A of 'a | B of 'b | C of 'c ;;

(* type two = true|F ;; *)


type 'a t = bool -> ('a option) ;; (*2 -> (a+1) *)

type 'a t' = ((bool -> 'a), bool*'a , one) sum3;; (*2->a + 2a + 1*)

(* type 'a t' = (bool -> 'a, bool * 'a, unit) sum3 *)






let  (f' : 'a t -> 'a t') = fun p -> 
  match p true, p false with 
  |Some a, Some a' -> A (fun b -> if b then a else a')
  |Some a, None -> B (true, a)
  |None , Some a' -> B (false, a')
  |None , None -> C One 
;;

let (g' : 'a t' -> 'a t) = function
  | A f -> fun b -> Some (f b )
  |B (true, a) -> fun b -> if b then Some a else None
  |B (false , a) -> fun b -> if b then None else Some a 
  |C One -> fun b -> None
;;

let a = 2;; 
let b = 3;; 

(* (a, Left b) |> f' |> g' = Left (a, b) |> g' = (a, Left b);; 

(a, Left b) |> f' |> g' = Left (a, b) |> g' = (a, Left b) *)

;; 


