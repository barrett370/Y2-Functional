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



module TransStateMonad:(TRANS_STATE_MONAD) = 
  functor(State:STATE) -> 
  struct

    

    type 'a m = (State.s -> ('a * State.s))list 

    let rec map  = fun f a ms ->
      let rec aux ret xs = match ms with  
        |[]->t 
        |x::xs -> aux ((StateMonad.map f a x)::ret) xs 
      in aux [] ms 

    let mult att ms = 
      let rec aux ret xs = match ms with 
        |[]-> ret 
        |x::xs -> aux (StateMonad.mult att x)::ret xs 
      in aux [] ms 

    let unit = fun a ms -> 
      let rec aux ret a xs = match ms with 
        |[]-> ret
        |x::xs -> aux (StateMonad.unit x)::ret a xs
      in aux [] a ms 

    let run = fun ms -> 
      |[]-> failwith"Nothing to run" 
      |x::ms -> StateMonad.run x 

    let set = fun s ms -> s::ms 

    let get = fun ms -> x::ms 

    let (>>=) at f = at |> (map f) |> mult

    let commit ms = function 
    |[]-> State.emp::[]
    |x::ms -> x::[]

    let rollback ms = function 
      |[]-> State.emp::[]
      |_ -> x::ms -> ms 
    

    




end 


