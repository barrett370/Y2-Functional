type 'a cell   = Cons of 'a * 'a stream
and  'a stream = ('a cell) Lazy.t


let rec from_const k = lazy (Cons (k, from_const k));;
let rec nats k = lazy (Cons (k, nats (k + 1)))
let rec from_f f = lazy (Cons (f 0, from_f (fun n -> f (n + 1))))


let rec (map2: ('a->'b->'c) -> 'a stream -> 'b stream -> 'c stream) = fun f s s' -> match s, s' with 
  |lazy( Cons(x, xs) ), lazy(Cons (x',xs')) -> lazy (Cons(f x x', map2 f xs xs'));;


let peek = function
  |lazy (Cons(x,_)) -> x 


let next = function
  |lazy(Cons (_,xs)) -> xs 


let rec (dif:int stream -> int stream) = fun s -> match s,next s with 
  |lazy(Cons(x,xs)) , lazy(Cons(x',xs')) -> lazy(Cons((x'-x), (dif xs)));;


let rec (avg : int -> int stream -> int stream) = fun k s -> 
  let rec aux acc c s= match c,s  with
    |c, lazy(Cons(x,xs)) -> if c = k then lazy(Cons((acc+x)/k,avg k xs)) else aux (x+acc) (c+1) xs
  in aux 0 0 s ;;

  