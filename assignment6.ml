  module type SET = sig
    type 'a t
    val empty     : 'a t
    val singleton : 'a -> 'a t
    val intersect : 'a t -> 'a t -> 'a t
    val union     : 'a t -> 'a t -> 'a t
    val member    : 'a -> 'a t -> bool
    val map       : ('a -> 'b) -> ('a t -> 'b t)
    val fold      : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val eq        : 'a t -> 'a t -> bool
  end


  module Set:SET = struct

    type 'a t = 'a list ;;

    let setify xs =
      let rec aux acc xs = match xs with
        |[]-> acc
        |x::xs -> if List.mem x acc then aux acc xs else aux (x::acc) xs
      in aux [] xs

    let (empty:'a t) = [];;

    let (singleton:'a -> 'a t) = fun x -> x::[];;

    let intersect xs ys =
      let rec aux acc xs ys = match xs with
        |[] -> acc
        |x::xs -> if List.mem x ys  then aux (x::acc) xs ys else aux acc xs ys
      in aux [] xs ys |> setify;;



    let union xs ys = xs@ys |> setify;;

    let member x ys = List.mem x ys ;;

    (* let map f =
       let rec aux acc xs = match xs with
        |[]->acc
        |x::xs -> aux ((f x)::acc) xs
       in aux [] ;; *)

    let map f xs = List.map f xs  |> setify

    let fold a xs= List.fold_left a xs

    let eq xs ys = xs = ys ;;

  end;;


  (*  Q2 :
    this directive is a form of destructive subsitution and tells the signature that the it needs to pass through the type 'a t and points it to the implementation of the type it wants it to use
  *)

  module type M = sig
    type _ t
    val map : ('a -> 'b) -> ('a t -> 'b t)
    val unit : 'a -> 'a t
    val mult : ('a t) t -> 'a t
  end;;


  module Monad_e (Monad : M) = struct
    include Monad

    (* bind *)
    let ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t =
      fun at f -> at |> (map f) |> mult

    (* lift *)
    let lift f at bt =
      at >>= fun a ->
      bt >>= fun b ->
      unit (f a b) 
  end


  module type SM = sig
    include M
    include SET with type 'a t := 'a t
  end;;


  module Set_m:SM = struct

    type 'a t = 'a list

    let rec map (f: 'a->'b) = function
      | []  -> []
      |  x::xs -> (f x)::(map f xs)

    let unit a = a::[]

    let mult = function
      | [] -> []
      | x::xs -> x


    let setify xs =
      let rec aux acc xs = match xs with
        |[]-> acc
        |x::xs -> if List.mem x acc then aux acc xs else aux (x::acc) xs
      in aux [] xs

    let (empty:'a t) = [];;

    let (singleton:'a -> 'a t) = fun x -> x::[];;

    let intersect xs ys =
      let rec aux acc xs ys = match xs with
        |[] -> acc
        |x::xs -> if List.mem x ys  then aux (x::acc) xs ys else aux acc xs ys
      in aux [] xs ys |> setify;;



    let union xs ys = xs@ys |> setify;;

    let member x ys = List.mem x ys ;;

    (* let map f =
       let rec aux acc xs = match xs with
        |[]->acc
        |x::xs -> aux ((f x)::acc) xs
       in aux [] ;; *)

    let map f xs = List.map f xs  |> setify

    let fold a xs= List.fold_left a xs

    let eq xs ys = xs = ys ;;





  end
;;


module N = struct
  exception NegNat
  type nat = Zero | Succ of nat;;

  let rec add_nat x y = match x with
    |Zero -> y
    |Succ x -> add_nat x (Succ y) ;;

  let rec sub_nat x y = match x,y with
    |Zero,Succ y -> raise NegNat
    |x,Zero-> x
    |Succ x, Succ y -> sub_nat x y  ;;

  let int_to_nat x =
    let rec aux x n = match x with
      |0 -> n
      |x -> aux (x-1) (Succ n)
    in aux x Zero ;;

  let nat_to_int n =
    let rec aux x n = match n with
      | Zero -> x
      | Succ n -> aux (x+1) n
    in aux 0 n ;;

  let rec modulo x y =
    let rec aux x =
      try
        let s = sub_nat x y in aux s
      with NegNat -> x
    in aux x


end
