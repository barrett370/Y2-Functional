# Monads


## An aside - OCaml type restriction

```ocaml
# let x = [];;
val x : 'a list = []
# let y = ref [];;
val y : '_a list ref = {contents = []}
# let y' = !y;;
val y' : '_a list = []
# x;;
- : 'a list = []
# y';;
- : '_a list = []
#
```

in the above definitions, x is of type `'a list ` and y is of type `'_a list` this subtle distinction essentially means that once an element has been added to `y` its type *hardens* meaning it no longer behaves as a generic `'a` but as the type of whatever was inserted.
```ocaml
# y';;
- : '_a list = []
# 1 :: y';;
- : int list = [1]
# y';;
- : int list = []
# 'a' :: y';;

Characters 7-9:
  'a' :: y';;
         ^^
Error: This expression has type int list
       but an expression was expected of type char list
       Type int is not compatible with type char

```
whereas with x of type `'a list`
```OCaml
# x;;
- : 'a list = []
# 1 :: x;;
- : int list = [1]
# 'a' :: x;;
- : char list = ['a']
```

To avoid this issue you can simple stipulate all variables being when defining functions. This process is called an *eta-expansion* or *$\eta$-expansion*

given :
```ocaml
# let f x = x;;
val f : 'a -> 'a = <fun>
```
This creates the difference between:


```ocaml
# let g = f f;;
val g : '_a -> '_a = <fun>
```
and
```ocaml
# let g x = f f x;;
val g : 'a -> 'a = <fun>
```


## Monads

- Particularly important in Haskell as without them Haskell is devoid of interaction (in an attempt to make it pure)

- The general pattern is to take functions defined on normal (un-instrumented ) types `'a` and to *lift* them to work with instrumented types, `'a t `.

- A simple example of an event where we may want to lift computation (in this case to make the function pure) is the following

```ocaml
# 1/0;;
Exception: Division_by_zero
```

- We can simply solve this by the instrumentation of the option type:

```ocaml
# let ( / ) x y = if y = 0 then None else Some (x/y)
- : val ( / ) : int -> int -> int option = <fun>
```

- However, by instrumenting our data and lifting only one function ( `/` ) to work with it, we render all other arithmetic operators useless

```ocaml
# 1/0;;
- : int option = None
# 4/2;;
- : int option = Some 2
# 4/2 + 2;;
Characters 0-3:
  4/2 + 2;;
  ^^^
Error: This expression has type int option
       but an expression was expected of type int
```

- Manually lifting all functions would be very tedious.
- Do we have to lift each operation in the same way as we did for ( `/` )? Or can we design machinary to do it for us? Such as:

```ocaml
 let ( + ) = lift ( + )
 let ( * ) = lift ( * )
 let ( - ) = lift ( - )
 let ( < ) = lift ( < )
 ...
```

- We can uniformly lift functions already defined for `'a` to work on our instrumented type `'a t` in this case `'a option` we do this using Monads.


### What are monads?

- using functors we have already seen a form of lifting of computation

```ocaml
module type F = sig
  type _ t

  val map : ('a -> 'b) -> ('a t -> 'b t)
end
```
- Here one can see that the `map` operation produces instrumented (lifted) types `'a t`.

- The `map` function is a requirement for a methematical functor, which is defined as "maps between types". The `map` function allows a function to work on an instrumented type of `'a t`

- However, we cannot lift all computation with this. We require 2 extra operators which the Monad pattern provides. (`unit` and `mult`)

**Monads are just functors with 2 extra operations**

**The monad extension adds a further 2**

```ocaml
module type M = sig
  include F

  val unit : 'a -> 'a t       

  val mult : ('a t) t -> 'a t
end
```
- The monad signature, M is an extesion to the Functor signature, F, therefore must include it at the start

To produce

```ocaml
module type M =
  sig
    type _ t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val unit : 'a -> 'a t
    val mult : 'a t t -> 'a t
  end
```
Once compiled.


#### Our two new operations

Unit  
- "lift" data from `'a` up to `'a t`


Mult
- "squash" data from `'a t t` down to `'a t`


### Option Monad
```ocaml
module Option_m : (M with type 'a t = 'a option) = struct
  type 'a t = 'a option

  let map (f : 'a -> 'b) = function
    | None   -> None
    | Some a -> Some (f a)

  let unit a = Some a

  let mult = function
    | None   -> None
    | Some x -> x
end
```

This is the condensed form using the predefined Monad (and by proxy, functor) signatures, simply stipulating the instrumented type as `'a option`

The full, explicit signature is given by:
```ocaml
module Option_m :
  sig
    type 'a t = 'a option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val unit : 'a -> 'a t
    val mult : 'a t t -> 'a t
  end
```
With the same resulting implementation, simply taking the above to be its explicit signature.

With the above machinary we could lift all our functions uniformly. However, the monadic extension, as mentioned earlier, adds a futher 2 functions useful when lifting many functions.

### Our two new operations : Monad_extension

```ocaml
module Monad_e (Monad : M) = struct
  include Monad

  (* bind *)
  let ( >>= ) at f = at |> (map f) |> mult

  (* lift *)
  let lift f at bt =
    at >>= fun a ->
    bt >>= fun b ->
    unit (f a b)
end

module Monad_e :
  functor (Monad : M) ->
    sig
      type 'a t = 'a Monad.t
      val map : ('a -> 'b) -> 'a t -> 'b t
      val unit : 'a -> 'a t
      val mult : 'a t t -> 'a t
      val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
      val lift : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    end
```

Bind
- `bind` `(>>=)` takes a monadic value `at : 'a t` and processes it via a function of type `f : 'a -> 'b t` by lifting the function to `map f : 'a t -> 'b t t`. However, we have to use our `mult` function to squash the resulting double instrumentation.

- The monadic extension now features our theorised `lift` method which can lift any 2 valued (curried) function of type `'a -> 'b -> 'c` and lift it to `'a t -> 'b t -> 'c t`. The arguments (`'a` and `'b`) are lifted using `bind` and the result (`'c`) via `unit`.


___

## Other common Monads 

___

### Identity 

- Simplest Monad, does no lifting 
- Used when operations are parameterised by monads, acts like () or [] 

```ocaml 
module Id_m : M = struct 
  type 'a t = 'a 
  let map f = f 
  let unit a = a 
  let mult a = a 
end
```

### Writer Monad 

- allows us to enhance types with logging information 


```ocaml 

module Writer_m : (M with type 'a t = 'a * string) = struct
  type 'a t = 'a * string
  let map f (a, s) = (f a, s)
  let unit a = (a, "")
  let mult ((a, s), s') = (a, s' ^ s)
end
module Writer_m :
  sig
    type 'a t = 'a * string
    val map : ('a -> 'b) -> 'a t -> 'b t
    val unit : 'a -> 'a t
    val mult : 'a t t -> 'a t
  end

```

- we can now perform logging by using the monad extension to create `Logger` module 

```ocaml 
module Logger = struct  
  module M = Monad_e (Writer_m)      
  open M

  let (<) (x, s) (y, s') =
    (x < y,
     (if x < y
      then ((string_of_int x) ^ "<"  ^ (string_of_int y) ^ "\n")
      else ((string_of_int x) ^ ">=" ^ (string_of_int y) ^ "\n"))
    ^ s ^ s')
end
module Logger :
  sig
    module M :
      sig
        type 'a t = 'a Writer_m.t
        val map : ('a -> 'b) -> 'a t -> 'b t
        val unit : 'a -> 'a t
        val mult : 'a t t -> 'a t
        val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
        val lift : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
        val lift2 : ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
      end
    val ( < ) : int * string -> int * string -> bool * string
  end

```



### State Monad


- `run: 'a m -> 'a ` removes instrumentation which is unusual 
  - Operations of this type are forms of *Evaluation* and functors featuring these operations are called *algebras*

- represents a simple singleton state
  - Therefore, `set` and `get` do not require parameters 
- The state is typed therefore, we need to parameterise the state monad by a state module outlining the state of a singleton cell.
- The type of the state monad is `type 'a m = State.s -> ('a * State.s)`. Better referred to as a **State transformer** as it is a function of a state to a state and a value 
- `unit` acts as identity on the state 
- `mult` composes the *state transformers* associated with the inner and outer monad, in order to reflect sequential execution 
- `run` executes (evaluates) the beginning to end composition for the monadic value, starting with the default state defined in the State module 



```ocaml 
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
```


An example usage: 

```ocaml 

module IntState : (STATE with type s = int) = struct
  type s = int
  let emp = 0
end

module Stateful = struct
  module SM = StateMonad(IntState)
  open SM

  let inc s =
    get         >>= fun n  ->
    set (n + 1) >>= fun () ->
    get
end
```