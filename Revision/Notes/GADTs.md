# Generalised Algebraic Data Types

- A common application of functional languages is in the development of interpreters and compilers.

- Both require the creation of evaluators on expressions.

We can implement a language of Arth-log expressions using Algebraic Data Types (ADTs)

```ocaml
type types = Bool | Int

type value = B of bool | I of int

type expr =
  | Value of value
  | Plus  of expr * expr (* e + e *)
  | And   of expr * expr (* e & e *)
  | Lt    of expr * expr (* e < e *)
  | Eq    of expr * expr (* e = e *)
```

with the following type-checker and expression evaluator:
```ocaml
let rec check : expr -> types option = function
  | Value (B _)  -> Some Bool
  | Value (I _)  -> Some Int
  | Plus (e, e') ->
    if check e = Some Int  && check e' = Some Int
    then Some Int  else None
  | And  (e, e') ->
    if check e = Some Bool && check e' = Some Bool
    then Some Bool else None
  | Lt   (e, e') ->
    if check e = Some Int  && check e' = Some Int
    then Some Bool else None
  | Eq   (e, e') -> match check e, check e' with
    | Some t, Some t' when t = t' -> Some Bool
    | _, _ -> None


let add (Int x) (Int y) = Int (x + y)

let mul (Int x) (Int y) = Int (x * y)

let conj (Bool x) (Bool y) = Bool (x && y)

let lt (Int x) (Int y) = Bool (x < y)

let eq x y = Bool (x = y)

let rec eval : expr -> value = function
  | Value v -> v
  | Plus (e, e') -> add  (eval e) (eval e')
  | Mul  (e, e') -> mul  (eval e) (eval e')
  | And  (e, e') -> conj (eval e) (eval e')
  | Lt   (e, e') -> lt   (eval e) (eval e')
  | Eq   (e, e') -> eq   (eval e) (eval e')
```

However, the above system will yield many compie-time warnings. for in-exhaustive pattern matchings

This is due to the fact that we only look for legal expressions

To avoid this we could make our code a lot more verbose and introduce exceptions during run-time. 

For example :

```ocaml 
# let add x y = match x, y with 
  |Int x, Int y -> Int (x+y)
  | _,_ -> failwith "add" ;;
- : val add : value -> value -> value = <fun>

```
### Issues

  - Verbose
  - Dead code as we will always call type checker before compiler so compiler should never receive ill-typed expressions
  - By doing this we make ourselves unable to check the types of our expressions until run-time resulting in run-time exceptions rather than compile-time.

  - Essentially, we require *dead code* even when we know we are constructing well-typed expressions 

### Solution : GADTs

- **Using GADTs we write types as functions on types**
as follows: 

```ocaml 
 type 'a value =
    | B : bool -> bool value
    | I : int  -> int  value
        
  type 'a expr =
    | Value : 'a value -> 'a expr
    | Plus  : int expr * int expr -> int expr
    | And   : bool expr * bool expr -> bool expr
    | Lt    : int expr * int expr -> bool expr
    | Eq    : 'a expr * 'a expr -> bool expr
```
Now if we attempt to construct an ill-typed expression we get compile-time warnings. 

```ocaml
# let e_bad = Plus (Value (B true), Value (B true));;
Characters 24-32:
  let e_bad = Plus (Value (B true), Value (B true));;
                          ^^^^^^^^
Error: This expression has type bool value
       but an expression was expected of type int value
       Type bool is not compatible with type int
```

The result of this is we now no longer need a function to perform type-check as the compiler will do it for us. 
We also no longer need to extract values from our encapsulated type. 

It is important to realise that type inference is much more difficult on GADTs therefore we often need to stipulate the types of our functions and variables explicitly in order for the inbuilt type-checker to work correctly. 

To get around this we need to locally abstract the types used in functions such as `eval` using the syntax `type a . a expr -> a`

## Generic Functions 

- OCaml does not support a generic `print` function such as in Python
- We can take steps towards one using GADTs:


```ocaml
type _ t =
    | Int  : int t
    | Bool : bool t
    | Pair : 'a t * 'b t -> ('a * 'b) t

  let rec to_string : type a. a t -> a -> string =
    fun t x ->
      match t with
      | Int -> string_of_int x
      | Bool -> if x then "tt" else "ff"
      | Pair (t1, t2) ->
        let (x1, x2) = x in
        "( " ^ (to_string t1 x1) ^
        ", " ^ (to_string t2 x2) ^ " )" 
        
  let x = to_string (Pair (Int, Bool)) (3, true)

# x;;
- : string = "( 3, tt )"
# 
```
However we annoyingly need to provide the types of the items we want to print as seen by  our argument `(Pair (Int, Bool)) (3, true)` 

- GADTs can often produce rather impenetrable errors 


## Dependant Types 

- Using GADTs we can write types which are dependant on a value. 
- For example in the type of length-indexed vectors:

```ocaml 
type z = Z (*the type of zero in the natural numbers (base value*)
type _ s = S : 'n -> 'n s (*Essentially the successor function*)
```

Using the above we can now parameterise a vector by its size: 
```ocaml 
type ('a, _) vec = 
  |Emp: ('a, z) vec (*has length of type z(ero)  (val Z))*)
  |Cons : 'a * ('a, 'n) vec -> ('a, 'n s ) vec (*simply increments the size parameter by using the successor function s : S  *)
```
Using this type we can write our `hd` and `tl` functions without having to catch exceptions from edge cases such as `hd [] -> failwith"hd"` 

```ocaml 
let hd : ('a, 'n s) vec -> 'a = function Cons(a,_) -> a 
val hd : ('a, 'n s) vec -> 'a = <fun> 

let tl : ('a, 'n s) vec -> ('a, 'n) vec = function Cons (_, xs) -> xs
val tl : ('a, 'n s) vec -> ('a, 'n) vec = <fun>
```




