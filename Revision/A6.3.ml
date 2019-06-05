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
  type 'a t = 'a list 

  let empty = []

  let singleton x = [x]

  let intersect xs ys = 
    let rec aux acc xs = function
      |[]-> acc
      |y::ys -> if List.mem y xs then aux (y::acc) xs ys else aux acc xs ys 
    in aux [] xs ys 

  let setify xs = 
    let rec aux acc = function
      |[]-> acc
      |x::xs -> if List.mem x acc then aux acc xs else aux (x::acc) xs 
    in aux [] xs

  let union xs ys = xs @ ys |> setify

  let member x xs = List.mem x xs 

  let rec map f = function
    |[] -> []
    |x::xs -> (f x )::(map f xs)

  let rec fold (f:'a->'b->'a) (x0:'a) (xs:'b t)= match xs with 
    |[]-> x0
    |x::xs -> fold f (f x0 x) xs 

  let eq xs ys = xs=ys

end 
;;

module type M = sig
  type _ t
  val map : ('a -> 'b) -> ('a t -> 'b t)
  val unit : 'a -> 'a t       
  val mult : ('a t) t -> 'a t
end
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
end;;

module type SM = sig
  include M
  include SET with type 'a t := 'a t
end


module Set_m : (SM with type 'a t = 'a Set.t)  = struct
  include Set


  let unit x = singleton x 

  let rec mult = function
    |x when x = empty -> empty 
    |x -> (fold(fun acc curr -> union curr acc) empty x )




end ;;



module N = struct
  open Set_m
  include Monad_e(Set_m)


  let ( % ) = lift ( mod )
  let ( - ) = lift ( - )
  let ( <= ) = lift ( <= )
  let ( / ) = lift ( / )

  let modulo x s = map (fun y -> x % (unit y)) s |> mult

  let rec get_numbers = function
    | x when x = empty -> empty
    | x when ((x <= unit(1)) = unit(true)) -> empty
    | x when (x = unit(2)) -> unit(2)
    | x -> union x (get_numbers (x - (unit 1)))


    let is_prime n = 
    if n =0 then false else
      let n = unit(n) in 
      let ns = get_numbers ( n / (unit 2) ) in
      modulo n ns |> member 0 |> not 




end