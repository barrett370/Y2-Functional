
# Question 1 

1. ,
   1. - Derivation from the lambda calculus developed by Alonzo Church 
      - Higher order functions 
      - Use of currying to have multi-variate functions 
      - use of pattern matching 
   2. ```ocaml
        let (|>) x f = f x 
        val (|>) : 'a -> ('a->'b) -> 'b 
        ```
   3.  $$0^0 = 1 $$ 
        this is true as translated into logic this expression reads:
        $$\bot \land \bot \supset \top $$ 
        which is trivially true logical expression 

    4. ```ocaml 
        let f () = Left ()

        let f' () = Right ()
        ```
    5. By the use of module signatures, one can obscure the types and method definitions contained in the derivative modules from the user at run time and the programmer. 


   6. ```ocaml 
      x := !x +1
      ```
   7. Monads are used to lift computation and instrument types uniformly. They allow for cleaner more robust code to be written and allow for making some operations pure. 
   8. `lazy (ref (1+1))` will evauluate the entire expression once necessary. `ref(lazy(1+1))` will assign a reference to the result but the result will not be computed until required. 
   9. A GADT is a generalised algebraic data type. They work as functions on types to eliminate dead code and run-time errors in more complicated applications 

2.  $$(x+1)^2 = x^2 + 2x +1 $$

$$ (\top \vee \top) \implies (x\vee \top) \equiv ((\top \vee \top) \implies x) \vee ((\top \vee \top) \land x) \vee \top $$


```ocaml 
    bool -> ('a option) | bool -> ('a,())sum 
    ((bool -> 'a , (bool*'a))sum, ())sum 

    type ('a,'b,'c)sum3 = A of 'a | B of 'b | C of 'c
    type 'a t = bool -> 'a option 
    type 'a t' = (bool -> 'a, bool * 'a, ())sum3

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

    (fun true -> Some true) |> f |> g 
```

3. 

```ocaml
    type z : Z
    type _ s : S: 'n -> 'n s 

    type ('a,_) vec = 
        |Emp : ('a, z) vec 
        |Cons : 'a * ('a, 'n) vec -> ('a, 'n s) vec 

    let (head: type n.('a,n s) vec -> 'a ) = function 
        |Cons (a,_) -> a
    
    let (tail: type n. ('a, n s) vec -> ('a, n) vec) = function 
        |Cons (_,xs) -> xs
    
    let rec (map : type n. ('a->'b) -> ('a, n s ) vec -> ('b, n s)) = fun f -> function 
    Cons(a,xs) -> Cons (f a, map f xs)

    let rec (rev : type n. ('a, n) vec -> ('a, n) vec) = function 
        |Emp (a,z) vec -> a       
        Cons (a ,xs) -> Cons (rev xs, Emp(a,z)))
   
   


```

```ocaml 

module type LQUEUE = sig
  type (_,_) t
  val empty: (_,z) t
  val push : 'a → ('a, 'n) t → ('a, 'n s) t
  val pop : ('a, 'n s) t → 'a * ('a, 'n) t
  val size : ('a, 'n) t → 'n
end

module LQueue: LQUEUE = struct 
    type z = Z
    type _ s = S: 'n -> 'n s 

    type ('a, _) t 
    
    let empty: (_,z) t = Emp (None, Z)

    let push: type n.'a -> ('a, n) t -> ('a, n s) t= fun a v -> 
        Cons (a, v) 
    
    let pop: type n. ('a, n s) t -> 'a * ('a, n) t = function 
        |Cons(a,xs) -> (a,xs)
    
    let rec size : type n. ('a ,n)t -> n = function
        |Emp(_,_) -> 0
        |Cons(_,xs) -> 1 + size xs 


    
end
```