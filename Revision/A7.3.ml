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
  end

(* Implementation of the state monad goes here: *)
module State_Monad : STATE_MONAD = 
  functor (State: STATE) -> 
  struct 
    type 'a m = State.s -> ('a * State.s)
    (* ('a -> 'b) -> 
      ((State.s -> ('a*State.s))->(State.s->('b*State.s))) *)
    let map : ('a -> 'b) -> ('a m -> 'b m) = 
      fun f a s -> let (a', s') = a s in (f a', s')

    (* State.s -> (State.s -> ('a *State.s)*State.s) *)
    let mult : 'a m m -> 'a m = 
      fun att s0 -> let (at, s1) = att s0 in at s1
    (* 'a -> State.s -> ('a * State.s) *)
    let unit : 'a -> 'a m = 
      fun a s -> (a,s)

    (* State.s -> ('a * State.s) -> 'a *)
    let run : 'a m -> 'a = 
      fun m -> m State.emp |> fst 
    
    (* State.s -> (State.s -> (unit*State.s))) *)
    let set : State.s -> unit m = 
      fun s _ -> ((),s)

    (* State.s -> (State.s*State.s) *)
    let get : State.s m = 
      fun s -> (s,s)

  (* (State.s -> ('a*State.s)) -> 
            ('a -> (State.s -> ('b* State.s))) -> 
                (State.s -> ('b * State.s)) *)
    let (>>=) : 'a m -> ('a -> 'b m) -> 'b m = 
      fun at f -> at |> (map f) |> mult 


  end 

  module Int_State:(STATE with type s = int) = struct
    type s = int 
    let emp = 0 
  end


(* Add the short text answer here as a comment: *)

module Stateful = struct
  module ISM = State_Monad(Int_State)
  open ISM 

  
  
  (* Change the code so that x produces the value Some 6.
      let x =
          let foo = inc() + inc () + inc () in 
          run foo
      Implementation goes here:
  *)

  let inc s = 
    get >>= fun n -> 
    set (n+1) >>= fun () -> 
    get 

  
end

