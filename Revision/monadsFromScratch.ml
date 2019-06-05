


module type F = sig
  type _ t 

  val map:('a->'b)->('a t -> 'b t)

end

module type M = sig
  include F 
  val unit : 'a -> 'a t 
  val mult : 'a t t -> 'a t 
end 


module Monad_e (Monad : M) = struct 
  include Monad 

  let (>>=) at f = at |> (map f) |> mult 

  let lift f at bt = 
    at >>= fun a ->
    bt >>= fun b -> 
    unit (f a b)

end 


module Option_Monad : (M with type 'a t = 'a option) =
struct
  type 'a t = 'a option

  let map f  = function 
    |None -> None 
    |Some a -> Some (f a)

  let unit a = Some a 

  let mult = function
    |None -> None 
    |Some a -> a 



end 


module Option_Monad_E = struct


  module OME = Monad_e(Option_Monad) 
  open OME 

  let (+) = lift (+)

  let (-) = lift (-)
  let ( * ) = lift ( * )
  let ( / ) = lift (/)
  let (<) = lift (<)
  

end
