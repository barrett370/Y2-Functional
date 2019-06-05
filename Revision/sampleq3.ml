module type QUEUE =
sig
  type _ t
  val empty: _ t
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a option * 'a t
  val size: 'a t -> int
end
module Queue : QUEUE =
struct
  type 'a t = {
    inbox: 'a list;
    outbox: 'a list;
  }
  let empty = { inbox = []; outbox = [] }
  let push v q = {q with inbox = v :: q.inbox }
  let pop q = match q with
      { outbox = x :: xs } -> Some x, { q with outbox = xs }
    | { outbox = [] } ->
      match List.rev q.inbox with
        [] -> None, empty
      | x :: xs -> Some x, { outbox = xs; inbox = [] }
  let size {inbox;outbox} = List.length inbox + List.length outbox
end

type z = Z (*the type of zero in the natural numbers (base value*)
type _ s = S : 'n -> 'n s (*Essentially the successor function*)

module type LQUEUE = sig
  type (_,_) t
  val empty: (_,z) t
  val push : 'a -> ('a, 'n) t -> ('a, 'n s) t
  val pop : ('a, 'n s) t -> 'a * ('a, 'n) t
  val size : ('a, 'n) t -> 'n
end

module Liv = struct

  type ('a,_) vec = 
    |Emp: ('a, z) vec 
    |Cons: 'a * ('a, 'n) vec -> ('a, 'n s) vec 

  let hd : type n . ('a,n s) vec -> 'a = function
    |Cons (a,_) -> a 

  let tl : type n . ('a, n s) vec -> ('a, n) vec = function
    |Cons (_, xs) -> xs 

  let rec rev : type n. ('a, n)vec  -> ('a , n) vec = function
    |Emp -> Emp 
    |Cons (a, Emp) -> Cons(a,Emp)
    |Cons (a, xs) -> Cons(rev xs, a ) 

  let rec map : type n. ('a -> 'b) -> ('a, n s)vec -> ('b, n s )vec = fun f -> fun ns -> match ns with 
    |Cons (a, xs) -> Cons ((f a), (map f xs))

end 