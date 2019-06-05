
type 'a cell   = Cons of 'a * 'a stream
and  'a stream = ('a cell) Lazy.t

let rec map2 : ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream = fun f s s' -> match s,s' with 
  |lazy(Cons(x,xs)), lazy(Cons(x',xs')) -> lazy(Cons(f x x', map2 f xs xs'))
  

