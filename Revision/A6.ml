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
  type 'a t = 'a list 

  let setify xs =  
    let rec aux xs acc = match xs with 
      |[]-> acc
      |x::xs -> if List.mem x acc then aux xs acc else aux xs (x::acc)
    in aux xs []

  let empty = []

  let singleton x = [x]

  let intersect (xs:'a t) (ys:'a t) = 
    let rec aux xs ys acc = match xs with 
      |[] -> acc
      |x::xs -> if List.mem x ys then aux xs ys (x::acc) else aux xs ys acc 
    in aux xs ys []

  let union (a: 'a t) (b: 'a t) = a@b |> setify

  let member x xs = List.mem x xs

  let map f xs = List.map f xs 

  let fold f xs = List.fold_left f xs 

  let eq xs ys = xs == ys 




end

module type SM = sig
  include M
  include SET with type 'a t := 'a t
end

(* QUESTION 2 answer goes here, as a comment:
   it allows it to be transparent to the user rather than abstracted away 
*)


module Set_m : SM = struct 
  (* QUESTION 3 answer goes here *)
  type 'a t = 'a list 

  let setify xs =  
    let rec aux xs acc = match xs with 
      |[]-> acc
      |x::xs -> if List.mem x acc then aux xs acc else aux xs (x::acc)
    in aux xs []

  let empty = []

  let singleton x = [x]

  let intersect (xs:'a t) (ys:'a t) = 
    let rec aux xs ys acc = match xs with 
      |[] -> acc
      |x::xs -> if List.mem x ys then aux xs ys (x::acc) else aux xs ys acc 
    in aux xs ys []

  let union (a: 'a t) (b: 'a t) = a@b |> setify

  let member x xs = List.mem x xs

  let map f xs = List.map f xs 

  let fold f xs = List.fold_left f xs 

  let eq xs ys = xs == ys 

  let mult xss = 
    let rec aux xss acc = match xss with
      |[]-> acc
      |xs::xss -> aux xss xs@acc
    in aux xss []

  let unit x = [x] 




end




module N = struct 
  open Set_m 
  include Monad_e (Set_m)

  let (%) = lift (mod)
  let (-) = lift (-)
  let (<=) = lift (<=)
  let (/) = lift (/)

  let modulo x s = map (fun y -> x % (unit y )) s |> mult 



  (* let build_set x = 
     let rec aux c acc = match c with 
      |1 -> acc
      |_ -> aux (c+1) (union (unit c) acc)
     in aux (x/2) empty

     let is_prime x = 
     (* let xs = unit x in  *)
     let ts = build_set x in 
     let rec aux ts acc = match ts with 
      |[] -> member 0 acc 
      |t::ts -> aux ts ((x mod t) @ acc )
     in aux ts empty *)



  (* Generates a set of numbers from 2 to x *)
  let rec get_numbers = function
    | x when x = empty -> empty
    | x when x <= unit(1) = unit(true) -> empty
    | x when x = unit(2) -> unit(2)
    | x -> union x (get_numbers (x - (unit 1)))

  let is_prime n = 
    if n = 0 then false else
      let n = unit (n) in
      let numbers = get_numbers ( n / (unit 2) ) in
      modulo n numbers |> member 0 |> not


end

(* The code below is used for testing *)
module Test = struct
  open N


  let f =  
    assert (is_prime 7);
    assert (not (is_prime 6));
    print_endline "1"

end
