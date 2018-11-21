module type STATE =
sig
  type s
  val emp : s
end
module type MONAD =
  sig
    type 'a m
    val map  : ('a -> 'b) -> ('a m -> 'b m)
    val mult : 'a m m -> 'a m
    val unit : 'a -> 'a m
  end
  
module type STATE_MONAD = 
  functor(State : STATE) ->
  sig
    include MONAD
    (* Special operations *)
    val run  : 'a m -> 'a 
    val set  : State.s -> unit m
    val get  : State.s m
    val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
    val commit : unit -> unit m
    val rollback : unit -> unit m
  end
  
module type TRANS_STATE_MONAD = 
  functor(State : STATE) ->
  sig
    include MONAD
    (* Special operations *)
    val run  : 'a m -> 'a 
    val set  : State.s -> unit m
    val get  : State.s m
    val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
    val commit : unit -> unit m
    val rollback : unit -> unit m
  end


module TransStateMonad:(STATE with type s = int) = struct 
  type s = int 
  let emp = 0 
end 

module Trans_Stateful = struct 
  module SM = StateMonad(TransStateMonad)


end 
