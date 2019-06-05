(* (* (* type 'a value =
   | B : bool -> bool value
   | I : int  -> int  value

   type 'a expr =
   | Value : 'a value -> 'a expr 
   | Plus  : int expr * int expr -> int expr
   | And   : bool expr * bool expr -> bool expr
   | Lt    : int expr * int expr -> bool expr
   | Eq    : 'a expr * 'a expr -> bool expr


   let (|>) x f = f x ;;

   type ('a,'b) sum = Left of 'a | Right of 'b


   (* let f () = Left ()

   let f' () = Right ()


   let f'' = function
   |true -> Some true  
   |false -> None  *)

   type ('a,'b,'c)sum3 = A of 'a | B of 'b | C of 'c
   type one = One 
   type ('a,'b)product = Pair of ('a * 'b)
   type 'a t = bool -> 'a option  
   type 'a t' = ( (bool->'a), (bool*'a), One) sum3 

   let (f:'a t -> 'a t') = fun (p:'a t) ->
   match p true , p false with 
   |Some a, Some a' -> A (fun b -> if if b then x else x')
   |Some a, None -> B (true,a)
   |None, Some a' -> B (false,a')
   |None,None -> C One 

   let (g:'a t'-> 'a t) = function 
   | A f -> fun b -> Some (f b)
   | B (true ,x) -> fun b -> if b then Some x else None
   | B (false,x) -> fun b -> if b then None else Some x 
   | C One -> fun b -> None 

   (fun true -> Some true) |> f |> g  *)

   (* 
   type z = Z
   type _ s = S: 'n -> 'n s 

   type ('a,_) vec = 
   |Emp : ('a, z) vec 
   |Cons : 'a * ('a, 'n) vec -> ('a, 'n s) vec 

   let head: type n.('a,n s) vec -> 'a = function 
   |Cons (a,_) -> a

   let tail: type n. ('a, n s) vec -> ('a, n) vec = function 
   |Cons (_,xs) -> xs

   let rec map : type n. ('a->'b) -> ('a, n s ) vec -> ('b, n s) vec = fun f -> function 
   |Emp (a ,z) -> Emp (f a, Z)
   |Cons(a,xs) -> Cons (f a, map f xs)

   let rec rev : type n. ('a, n) vec -> ('a, n) vec = function 
   |Emp (a,z) vec -> a       
   |Cons (a ,xs) -> Cons (rev xs, Emp(a,z))) *)

   type z = Z
   type _ s = S: 'n -> 'n s 
   type ('a,_)vec = 
   |Emp: ('a, z) vec 
   |Cons : 'a * ('a, 'n) vec -> ('a, 'n s) vec 
   module type LQUEUE = sig
   type z 
   type _ s 
   type (_,_) t
   val empty: ('a, z ) t
   (* val push : 'a -> ('a, 'n) t -> ('a, 'n s) t *)
   (* val pop : ('a, 'n s) t -> 'a * ('a, 'n) t *)
   (* val size : ('a, 'n) t -> 'n *)
   end

   module LQueue: LQUEUE = struct 

   type z = Z
   type _ s = S: 'n -> 'n s 
   type ('a,_)t= 
    |Emp: ('a, z) t 
    |Cons : 'a * ('a, 'n) t -> ('a, 'n s) t 

   (* type ('a, _ ) t = ('a,  ) vec  *)

   let empty: (_,z) t = Emp

   let push: type n.'a -> ('a, n) t -> ('a, n s) t = function 
    |( a , Cons(a', n ) ) -> Cons (a, Cons(a',n)) 

   let pop: type n. ('a, n s) t -> 'a * ('a, n) t = function 
    |Cons(a,xs) -> (a,xs)

   let rec size : type n. ('a ,n)t -> n = function
    |Emp(_,_) -> 0
    |Cons(_,xs) -> 1 + size xs 

   end  *)


   type 'a m = int -> ('a* int )

   let x = fun a (s:int) -> (a,s)


   let (x:'a -> 'a m) = x  *)

module LiV = struct 
  type z = Z 
  type _ s = S : 'n -> 'n s

  type ('a,_)vec = 
    |Emp : ('a,z) vec 
    |Cons : 'a * ('a, 'n) vec -> ('a , 'n s )vec 

  let head : type n. ('a,n s) vec -> 'a = function Cons(a,_) -> a 

  let tail : type n. ('a, n s )vec -> ('a, n) vec = function Cons(_,xs) -> xs 

  let rec map : type n.('a->'b) -> (('a, n)vec -> ('b, n)vec) = fun f -> function
    |Emp -> Emp 
    |Cons(a,xs) -> Cons(f a, (map f xs))

  let rev : type n . ('a,_) vec -> ('a,_)vec = fun v ->
    let rec aux: type n.('a,n)vec -> ('a,n) vec -> ('a,_)vec  = fun acc -> function
      |Emp -> acc 
      |Cons (a,xs) -> aux (Cons(a,acc)) xs 
    in aux Emp v 



end 


module type LQUEUE = sig
  (* type (_,_) t *)
  (* val empty: (_,z) t *)
  (* val push : 'a -> ('a, 'n) t -> ('a, 'n s) t *)
  (* val pop : ('a, 'n s) t -> 'a * ('a, 'n) t *)
  (* val size : ('a, 'n) t -> 'n *)
end


module LQueue : (LQUEUE) = struct
  include LiV 

  type ('a,_) t = 
    |Emp : ('a,z)t 
    |Cons : 'a * ('a, 'n ) t -> ('a, 'n s )t 




  let empty = Emp  

  let push : type n . 'a -> ('a,n) t -> ('a, n s) t = fun a -> 
    function
    |Emp -> Cons(a,Emp) 
    |v -> Cons(a,(rev v)) |> rev 



end

