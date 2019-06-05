module type M = sig
  type _ t

  val map : ('a -> 'b) -> ('a t -> 'b t)
  val unit : 'a -> 'a t       
  val mult : ('a t) t -> 'a t
end

module Monad_e (Monad : M) = struct
  include Monad 

  (* bind *)
  let ( >>= ) = fun at f -> at |> (map f) |> mult

  (* lift *)
  let lift f at bt = at >>= fun a -> bt >>= fun b ->
    unit (f a b)
end

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

module Set : SET = struct
  (* QUESTION 1 answer goes here *)
  type 'a t = 'a list 

  let empty = []

  let singleton a = [a]

  let setify xs = 
    let rec aux acc = function
      |[] -> []
      |x::xs -> if (List.mem x acc) then aux acc xs else aux (x::acc) xs 
    in aux [] xs 

  let intersect xs ys =
    let rec aux acc xs =  match xs with 
      |[]-> acc 
      |x::xs -> if List.mem x ys then aux (x::acc) xs else aux acc xs 
    in aux [] xs |> setify

  let union xs ys = xs@ys |> setify

  let member x (xs: 'a t ) = List.mem x xs 

  let rec map f  = function []->[]
                          |x::xs -> (f x) :: (map f xs) |> setify

  let rec fold f z = function []-> z 
                            |x::xs -> fold f (f z x) xs 
  let eq xs ys = if intersect xs ys = intersect xs xs then true else false 




end

module type SM = sig
  include M
  include SET with type 'a t := 'a t
end

(* QUESTION 2 answer goes here, as a comment:
  the annotation with type 'a t := 'a t allows the type 'a t defined in the SET signature to be accessable in the derivative of the signature SM 
*)
(* 
module Set_m : (SM with type 'a t = 'a Set.t) struct
  (* QUESTION 3 answer goes here *)
  (* type 'a t = 'a list  *)
  include Set
  
  let unit a = singleton a 

  let mult att = List.concat att |> setify

end *)

(* module N 
  (* QUESTION 4 answer goes here *)
end *)

(* The code below is used for testing *)
(* module Test = struct
  open N

  let _ =  
    assert (is_prime 7);
    assert (not (is_prime 6));
    print_endline "1"
end *)
