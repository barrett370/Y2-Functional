# Algebraic Data Types

## OCaml Types

### Uninhabited/ Empty types
- don't give a constructor
```ocaml
#type zero;;
```

###Unit type
- 1 inhabitant
- built in type called Unit, denoted by `()`
```OCaml
  # ();;
  - : unit = ()
```
- can be imitated for use in Algebric types by the type
```OCaml
# type one = Unit;;
- : type one = Unit
```

###Boolean
- 2 inhabitants
- `True | False `
- Can be imitated by the type :
```OCaml
# type two = Left | Right
- : type two = Left | Right
```

###Variant Types
- Similar to enums
- e.g weekdays
```ocaml
# type weekday = Monday | Tuesday | Wednesday | Thursday | Friday ;;
- : type weekday = Monday | Tuesday | Wednesday | Thursday | Friday
```
- we can essentilly think of this type as the type of `5` as its cardinality is 5

```ocaml
# let next_day = function
  |Monday -> Tuesday
  |Tuesday ->Wednesday
  |Wednesday -> Thursday
  |Thursday -> Friday
  |Friday -> Monday;;

- : val next_day : weekday -> weekday = <fun>
```
```ocaml
# next_day Monday;;
- : weekday = Tuesday

```

###Product types
- built in type using pairs `('a,'b)` or more generally tuples `('a, 'b , 'c)`
```ocaml
-: (1,'a') : int * char
-: (True, Monday) : bool * weekday
```
- the tuple has the carinality = to the result of the type definition. e.g. `bool * weekday` = `2 * 7 = 14 `

###Option Type

```ocaml
# type 'a option = None | Some of 'a;;
```
- often used as an error checking measure
- Number of inhabitants = `a+1` where a is the number of inhabitants in type `'a`

### Sum Type

```ocaml
# type ('a, 'b)sum = Left of 'a | Right of 'b;;
```
- $\therefore$ #inhabitants = `a+b`


###Exponential Types
- all type constructors correspond to a mathematical operation.
What about functions?
- how many inhabitants does a function of signature `'a -> 'b` have?
- Answer : $a^b$

## Type isomorphism

- Two types are isomorphic if their inhabitants can be converted, without loss of information, from one type to the other (and back)

$`g(f(x)) = x \equiv (g \circ f)x = x`$

- where `'a -> 'b` via `f` and `'b -> 'a` via `g`

- These two data types `'a` and `'b` are essentilly the same

###Equations

$`1+1 =2`$
is equivalent to
$`(Unit, Unit)sum \simeq bool`$

```ocaml
# let f = (function
| Left () -> true
| Right () -> false)

# let g = (function
| true -> Left ()
| false -> Right ())

```




Algebra | Proposition | Type  | Constructor | Destructor (pattern) |
--------|------------|-------|-------------|-------|
$`0`$ | $`\bot`$ | `empty` | None | language-dependent |
$`1`$ | $`\top`$ | `unit` | `()` | `()` |
$`a\times b`$ |$`A\land B`$  | `'a * 'b` | `(a, b)` | `match (a, b) with ...` |
$`a+b`$ | $`A\vee B`$ | `('a, 'b)sum` | `Left a`, `Right b` |  `match ... with Left a -> ... | Right b -> ...` |
$`b^a`$ | $`a\supset b`$ | `'a->'b` | `fun x -> ...` | function application |
