# Functional programming 2019
## Sample Exam

**Notes**: Time allowed 90 minutes.   
Answer all questions. Each question will be marked out of 20.  
The paper will be marked out of 60, which will be rescaled to a mark out of 100.  
All programming questions are in reference to OCaml. If code is required, it must be OCaml.

### Question 1 (20 marks)

Give short answers to the following questions:

* What are the key features of a functional language?
* Define the function application operator `x |> f` and give its type.
* Why should $0^0=1$?
* Construct the two (pure) functions that have the type corresponding to the proposition $\top \supset \top\vee\top$.
* How can the definition of a type be _hidden_ in an OCaml module?
* In _imperative_ OCaml, how do you write "_Increment the value of variable `x` by 1._"?
* What are _Monads_ used for in OCaml?
* What is the difference between `lazy (ref (1+1))` and `ref (lazy (1+1))`?
* What is a GADT in OCaml?

### Question 2 (20 marks)

Consider the following axiom about the interaction between addition, multiplication, and exponentiation:

$$ (x+1)^2 = x^2 + 2\cdot x + 1 $$

* Express this equation as a _proposition_ involving conjunction, disjunction, and implication.
* Express the types for product, summation, and exponentiation as OCaml types.
* Construct isomorphisms between the type corresponding to the left-hand side of the equation and that of the right-hand side.
* Show that the above are indeed isomorphisms.
* Derive the type of a _zipper_ for the data-type(s) above.

### Question 3 (20 marks)

* Define the type of length-indexed vectors using GADTs.
* Write analogues of `head`, `tail`, `map`, and `rev` for this type.
* It is common to implement a functional queue as a pair of lists:

```ocaml
module type QUEUE =
sig
  type _ t
  val empty: _ t
  val push : 'a → 'a t → 'a t
  val pop : 'a t → 'a option * 'a t
  val size: 'a t → int
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
      { outbox = x :: xs } → Some x, { q with outbox = xs }
    | { outbox = [] } →
      match List.rev q.inbox with
        [] → None, empty
      | x :: xs → Some x, { outbox = xs; inbox = [] }
  let size {inbox;outbox} = List.length inbox + List.length outbox
end
```

Show how to implement the following signature of length-indexed queues
by taking a similar approach, but using a pair of vectors (Question 5)
rather than a pair of lists as the underlying data structure:

```ocaml
module type LQUEUE = sig
  type (_,_) t
  val empty: (_,z) t
  val push : 'a → ('a, 'n) t → ('a, 'n s) t
  val pop : ('a, 'n s) t → 'a * ('a, 'n) t
  val size : ('a, 'n) t → 'n
end
```
