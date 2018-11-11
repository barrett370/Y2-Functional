type 'a tree = Empty | Node of ('a * 'a tree * 'a tree)


type direction = Left | Right


type 'a tzipper =
  (direction * 'a * 'a tree) list * 'a tree * 'a tree


let tzipper_up (trail, tl, tr) =
  if trail = [] then None else
    match List.hd trail with
    | Left, x, t -> Some (List.tl trail, Node (x, tl, tr), t)
    | Right, x, t -> Some (List.tl trail, t, Node (x, tl, tr))

let tzipper_left = function
  | (trail, Node (x, tl', tr'), tr) ->
    Some ((Left, x, tr) :: trail, tl', tr')
  | _ -> None

let tzipper_right = function
    | (trail, tl, Node (x, tl', tr')) ->
    Some ((Right, x, tl) :: trail, tl', tr')
  | _ -> None

(* assignment implementation *)

(* type 'a btree = Leaf of 'a | Node of ('a * 'a btree * 'a btree) *)

(*
                    T(a) = a + aT^2(a)
                             T'(a) = 1 +  2aT'(a)T(a) + T^2(a)
                T'(a) - 2aT'(a)T(a) = 1 + T^2(a)
              T'(a)[1-2aT(a)] = 1 + T^2(a)
              T'(a) = (1+T^2(a))/(1-2aT(a))
              T'(a) = 1/1-2aT(a) + T^2(a) /1-2aT(a)
              T'(a) = L(2aT(a)) + L(2aT(a))*T^2(a)
                btree_zipper =           (bool * 'a * 'a btree)list | (bool * 'a * 'a btree)list * ('a list * 'a list)



 *)


(* T(a) = a + T^2(a)
     (bool * T(a))list


*)



type 'a btree = Leaf of 'a | Node of ('a btree * 'a btree)
type direction = Left | Right
type 'a bzipper = (direction * 'a btree)list


let rec bzipper_left = function
  |[] -> None
  |(d, Node _ )::xs -> bzipper_left xs
  |(d, Leaf a)::xs -> Some(Left,Leaf a)



let rec bzipper_right = function
  |[]-> None
  |(d,Node _)::xs -> bzipper_right xs
  |(d, Leaf a)::xs -> Some(Right, Leaf a)



type 'a bzipper' = 'a bzipper*'a bzipper


let rec bzipper_right' left right = match right with
  |[]-> None
  |(d,Node (tl, tr))::right -> bzipper_right' ((d,Node( tl,tr))::left) right
  |(d, Leaf a)::right -> Some(Right, Leaf a)


let rec bzipper_left' left right = match left with
  |[]-> None
  |(d,Node a )::left -> bzipper_left' left ((d,Node a)::right)
  |(d,Leaf a)::left -> Some(Left, Leaf a)


type 'a btree = Leaf of 'a | Node of ('a btree * 'a btree)
type direction = Left | Right
type 'a bzipper = (direction * 'a btree)list


let bzipper_first b =
  let rec aux t zip = match t with
    |(d,Node (a,b)) -> let x = (aux a zip)::(aux b zip)::(d,Node(a,b))::zip in let a = a::x in a*x
    |(b, Leaf a) -> (b,Leaf a)
  in let z = (d,Leaf a)::(aux b []) in z

let bzipper_last t =
  let rec aux t zip = match t with
    |(d,Node (a,b)) -> zip::(aux a zip )::(aux b zip)

    |(b, Leaf a) -> a * zip
  in let z = aux t [] in z
(* val bzipper_first : 'a btree -> 'a * 'a bzipper *)

(* val bzipper_last : 'a btree -> 'a * 'a bzipper  *)
