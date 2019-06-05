

module type STATE =
sig
  type s
  val emp : s
end

module type MONAD =
sig
  type 'a m
  (* val map  : ('a -> 'b) -> ('a m -> 'b m) *)
  (* val mult : 'a m m -> 'a m *)
  (* val unit : 'a -> 'a m *)
end

module type STATE_MONAD = 
  functor(State : STATE) ->
  sig
    include MONAD
    (* Special operations *)
    (* val run  : 'a m -> 'a  *)
    (* val set  : State.s -> unit m *)
    (* val get  : State.s m *)
    (* val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m *)
  end

(* Implementation of the state monad goes here: *)
module IntState : STATE = struct 
  type s = int option 
  let emp = None 
end

module StateMonad : STATE_MONAD = 
functor (State:STATE) -> struct 

  type 'a m = State.s -> ('a * State.s)

  
  

end 
  

  


end





(* Add the short text answer here as a comment: *)

module Stateful = struct
  (* Change the code so that x produces the value Some 6.
      let x =
          let foo = inc() + inc () + inc () in 
          run foo
      Implementation goes here:
  *)
end
