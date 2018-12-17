open Lazy

(* type 'a stream = Cons of 'a * 'a stream *)



(* type 'a cell   = Cons of 'a * 'a stream and 'a stream = ('a cell) Lazy.t *)

type 'a cell   = Cons of 'a * 'a stream
and  'a stream = ('a cell) Lazy.t;;

let list_to_stream xs =
  let rec aux s xs = match xs with 
    |[]-> s 
    |x::xs -> lazy (Cons(x,s))
  in aux xs ;;


let peek = function
  | lazy (Cons (x, _)) -> x


let nats = 
  let rec nats k = lazy (Cons (k, nats (k + 1))) in
  nats 0;;



let next = function
  | lazy (Cons (_, xs)) -> xs

let rec map2 : ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream =
    fun f x y -> match (x,y) with
    | (lazy (Cons (a, s1)), lazy (Cons (b, s2))) ->
          lazy (Cons(f a b, map2 f s1 s2))

let sum :( int stream -> int stream -> int stream) = fun a b -> map2(+) a b
  
let rec dif : int stream -> int stream = function
    | lazy (Cons (a, lazy (Cons (b, s2) ) ) ) ->
          lazy (Cons( (b-a) , lazy (Cons (b, s2) ) ) )

let rec dif' : int stream -> int stream = function 
  (* | a -> lazy (Cons ((map2 (-) (a |> peek) (a |> next |> peek))) , (dif' (lazy (Cons ( (a|> next |> peek), (a |> next |> next)))))) *)
  | a -> lazy (Cons ((peek a)-(a |> next |> peek), 
              lazy (Cons ((a |> next |> peek),a |> next |> next)))) 

(* let rec dif'' : int stream -> int stream = function 
(* | a -> lazy (Cons ((map2 (-) (a |> peek) (a |> next |> peek))) , (dif' (lazy (Cons ( (a|> next |> peek), (a |> next |> next)))))) *)
| a -> lazy (Cons (map2(-) (peek a) (a |> next |> peek), 
            lazy (Cons ((a |> next |> peek), a |> next |> next)))) 
             *)



let rec avg s1 k = match k with
  | 0 -> failwith "Cannot compute for 0 elements"
  | k ->
    let next_s = (next s1) in
      let aux k s1 =
        let rec aux' acc k s1 = match k with
          | 0 -> acc
          | k -> aux' ((s1 |>peek) + acc) (k-1) (s1 |> next)
        in (aux' 0 k s1) / k
      in 
      lazy (Cons(aux k s1, avg (next_s) k))


      let rec avg k stream1 = match k with
      | 0 -> failwith "cannot compute running average where k=0"
      | k ->
        let next_stream = (next stream1) in
          let help_function k stream1 =
            let rec aux acc k stream1 = match k with
              | 0 -> acc
              | k -> aux (peek stream1 + acc) (k-1) (next stream1)
            in (aux 0 k stream1) / k
          in
          lazy (Cons(help_function k stream1, avg k (next_stream)))

(* 
let rec avg s1 k = match k with
| 0 -> failwith "Cannot compute for 0 elements"
| _ ->
  let next_s = (s1 |> next) in
    let aux acc k s1 = match k with 
      | 0 -> acc
      | k -> aux ((s1 |> peek) + acc) (k-1) (s1 |> next)
    in (aux 0 k s1) / k
          in lazy (Cons(aux k s1, avg (next_s) k )) *)
