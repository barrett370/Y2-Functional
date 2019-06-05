module type F = sig (*Functor Signature*) 
  type _ t

  val map : ('a -> 'b) -> ('a t -> 'b t)
end

module type M = sig (*Monad Signature, dependant on F*)
  include F

  val unit : 'a -> 'a t       

  val mult : ('a t) t -> 'a t
end

module Monad_e (Monad : M) = struct 
  (*Monadic extension, dependant on a Monad implementation*)
  include Monad

  (* bind *)
  let ( >>= ) at f = at |> (map f) |> mult

  (* lift *)
  let lift f at bt =
    at >>= fun a ->
    bt >>= fun b ->
    unit (f a b)
end

module Option_Monad : (M with type 'a t = 'a option) = struct

  type 'a t = 'a option 


  let map f  = function None -> None 
                      |Some a -> Some (f a)

  let unit a = Some a 

  let mult = function 
    |None -> None
    |Some a -> a 

end 


module Arth_Option = struct

  module OME = Monad_e(Option_Monad)
  open OME 

  let (+) = lift (+)
  let (/) = lift (/)
  let ( * ) = lift ( * )
  let (-) = lift (-)

  let (<) = lift (<)


  let (&&) = lift (&&)
  let not = map not 




end 

module List_Monad : (M with type 'a t = 'a list) = struct
  type 'a t = 'a list 

  let rec map f = function []-> []
                         |x::xs -> f x :: (map f xs)

  let unit a = [a]

  let rec mult xs = List.concat xs 



end 


module List_Monad_E = struct

  module LME = Monad_e(List_Monad)
  open LME 

  let (+) = lift (+)
  let (/) = lift (/)
  let ( * ) = lift ( * )
  let (-) = lift (-)
  let (<) = lift (<)


  let (&&) = lift (&&)
  let not = map not 


end 
