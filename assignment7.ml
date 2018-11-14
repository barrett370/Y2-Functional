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

module StateMonad : (STATE_MONAD) =
   functor (State : STATE) ->
     struct
       type 'a m = State.s -> ('a * State.s)
                               
       let (map : ('a -> 'b) -> ('a m -> 'b m)) =
         fun f a s -> let (a', s') = a s in (f a', s')
                                                
       let (mult : 'a m m -> 'a m) =
         fun att s0 -> let (at, s1) = att s0 in at s1
           
       let (unit : 'a -> 'a m) =
         fun a s -> (a, s)
                        
       let (run : 'a m -> 'a ) =
         fun m -> m State.emp |> fst

       let (set : State.s -> unit m) =
         fun s _ -> ((), s)

       let (get : State.s m) =
         fun s -> (s, s)

       (* Bind *)
       let (>>=) at f = at |> (map f) |> mult

     end


module IntOptionState : (STATE with type s = int option) = struct 
  type s = int option
  let emp = None  
end 



module Stateful = struct
  module SM = StateMonad(IntOptionState)
  open SM

  let seq x y =
    x >>= fun x ->
    y >>= fun y ->
    unit y
  
  (* lift that takes 2 int option states and returns one *)

  (* let (+) x y = match x,y with
    |None,_ -> unit None 
    |_,None -> unit None 
    |Some x , Some y -> unit(Some (x+y))
     *)
  (* let (+) = lift  *)

  let (+)  x  y =
    x >>= fun x ->
    y >>= fun y ->
    match x,y with 
      |Some x , Some y -> unit(Some (x+y))  
      |_,_ -> unit None 
      
  

  let inc s =
    let n = get in
    (n + (unit( Some 1))) >>= fun n ->
    seq (set n) get  
    

  let u = unit ()
  
  let _ =
    let foo = inc() + inc () +inc () in 
    run foo
end;;


(*Value of 
  let _ =
    let foo = inc() + inc () +inc () in 
    run foo
    : int option = None *)




module type TRANS_STATE_MONAD = 
functor(State : STATE) ->
sig
  include MONAD
  (* Special operations *)
  val run  : 'a m -> 'a 
  val set  : State.s -> unit m
  val get  : State.s m
  val (>>=) : 'a m -> ('a -> 'b m) -> 'b m
  val commit : unit -> unit m
  val rollback : unit -> unit m
end
  
module Trans_State_Monad:TRANS_STATE_MONAD = struct 
  
  
  let (map : ('a -> 'b) -> ('a m -> 'b m)) =
    fun f a s -> let (a', s') = a s in (f a', s')
                                           
  let (mult : 'a m m -> 'a m) =
    fun att s0 -> let (at, s1) = att s0 in at s1
      
  let (unit : 'a -> 'a m) =
    fun a s -> (a, s)
                   
  let (run : 'a m -> 'a ) =
    fun m -> m State.emp |> fst

  let (set : State.s -> unit m) =
    fun s _ -> ((), s)

  let (get : State.s m) =
    fun s -> (s, s)

  (* Bind *)
  let (>>=) at f = at |> (map f) |> mult

  let commit () =  unit()
  
  let rollback () =  unit()

end 
