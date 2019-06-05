(* 1 *)

let rec last = function
  |[]-> None
  |[x] -> Some x 
  |_::xs -> last xs;;

(* 2 *)

let rec last_two = function
  |[]-> None
  |[x;x'] -> Some (x,x')
  |_::xs -> last_two xs ;;

(* 3 *)

let at v xs = 
  let rec aux i xs = match i with 
    |0 -> Some (List.hd xs)
    |_ -> if List.tl xs == [] then None else aux (i-1) (List.tl xs)
  in aux (v-1) xs ;;


(* 4 *)

let rec length = function 
  |[] -> 0 
  |_::xs -> 1 + length xs
;;

(* 5 *)

let rev xs = 
  let rec aux xs acc = match xs with 
    |[]-> acc
    |x::xs -> aux xs ( x :: acc )
  in aux xs [];;

(* 6 *)

let is_palindrome xs = if (rev xs) = xs then true else false ;;

(* 7 *)

type 'a node = 
  |One of 'a 
  |Many of 'a node list ;;

let (flatten: 'a node list -> 'a list)   = fun (ns:'a node list) ->
  let rec aux (acc:'a list) (ns:'a node list) = match ns with 
    |[]-> acc 
    |One x :: ns -> aux (x :: acc) ns 
    |Many xs :: ns -> aux ((aux [] xs)@acc) ns 
  in List.rev (aux [] ns) ;;

(* 8 *)

let compress xs = 
  let rec aux xs acc = match xs, acc with 
    |[],_ -> acc
    | x::xs, [] -> aux xs (x::acc)
    |x::xs, _ -> if (List.hd acc) = x  then aux xs acc else aux xs (x::acc)
  in List.rev (aux xs []);;

(* 9 *)

let pack xs = 
  let rec aux xs acc = match xs, acc with 
    |[],_ -> acc
    | x::xs, [] -> aux xs ([x]::acc)
    |x::xs, _ -> if (List.hd (List.hd acc)) = x   then
        aux xs ((x::(List.hd acc))::(List.tl acc)) 
      else
        aux xs ([x]::acc)
  in List.rev(aux xs []);;

(* 10 *)

let encode xs = 
  let packed_xs = xs |> pack in 
  let rec aux acc = function
    |[]-> acc
    |xs::pxs -> aux ((List.length xs,List.hd xs)::acc) pxs 
  in List.rev(aux [] packed_xs);;

(* 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode' xs = 
  let packed_xs = xs |> pack in 
  let rec aux acc = function
    |[]-> acc
    |xs::pxs -> let t = (List.length xs,List.hd xs) in 
      if (fst t) =1 then aux ((One (snd t))::acc) pxs else aux ((Many ((fst t,snd t)))::acc) pxs
  in List.rev(aux [] packed_xs);;


(* 12 *)
let rec gen_list c =  function
  |0-> []
  |n -> c :: gen_list c (n-1)

let decode xs = 
  let rec aux acc = function
    |[]-> acc
    |One c :: xs-> aux (c::acc) xs 
    |Many (i,c)::xs -> aux ((gen_list c i)@acc) xs 
  in List.rev(aux [] xs);;

(* 13 *)

let encode'' xs = 
  let rec aux acc xs = match xs, acc with 
    |[],_ -> acc
    |x::xs, [] -> aux ((One x)::acc) xs 
    |x :: xs, One a::acc -> 
      if x = a then 
        aux (Many (2,x)::acc) xs 
      else 
        aux ((One x)::(One a)::acc) xs 
    |x::xs , Many (n,a)::acc -> 
      if x = a then
        aux (Many (n+1,x)::acc) xs 
      else  
        aux ((One x)::(Many (n,a))::acc) xs 
  in List.rev(aux [] xs );;

(* 14 *)

let duplicate xs = 
  let rec aux acc = function
    |[]-> acc
    |x::xs -> aux (x::x::acc) xs 
  in List.rev(aux [] xs) ;;

(* 15 *)

let replicate xs n = 
  let rec aux acc = function
    |[]-> acc 
    |x::xs -> aux ((gen_list x n)@acc) xs 
  in List.rev(aux [] xs );;


(* 16 *)

let drop xs n = 
  let rec aux acc c xs = match xs,c with 
    |[],_ -> acc
    |_::xs, 1 -> aux acc n xs
    |x::xs , c -> aux (x::acc) (c-1) xs 
  in List.rev(aux [] n xs );;

(* 17 *)

let split xs n = 
  let rec aux acc xs c = match xs , c with 
    |[],_ -> (List.rev acc,[])
    |x::xs, c -> if c= n then (x::acc|> List.rev ,xs) else aux (x::acc) xs (c+1)
  in aux [] xs 1;;

(* 18 *)

let slice xs i k = 
  let rec aux acc xs c = match xs, c with
    |[],_-> acc
    |x::xs, c -> if c>=i && c<k then aux (x::acc) xs (c+1) else if c=k then (x::acc) else 
        aux acc (xs) (c+1)
  in List.rev(aux [] xs 0);;

(* 19 *)

let rotate xs n = if n>0 then 
    let hd,tl = split xs n in tl@hd 
  else 
    let hd,tl = split xs (List.length xs + n) in tl@hd ;;

(* 20 *)

let remove_at i xs = 
  let rec aux acc xs c = match xs,c with 
    |[],_ -> acc
    |x::xs, c -> if c = i then aux acc xs (c+1) else aux (x::acc) xs (c+1)
  in List.rev(aux [] xs 0);;

