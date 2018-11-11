

type 'a btree = Leaf of 'a | Node of ('a btree * 'a btree)
type direction = Left | Right
type 'a bzipper = (direction * 'a btree) list



let bzipper_first t =
   let rec aux = function
     |(Node (tl,tr),zip) -> (aux(tl,(Left,tr)::zip))
     |(Leaf a,zip) -> (a,zip)
   in aux (t,[])



let bzipper_last t =
  let rec aux = function
    |(Node (l,r), zip) -> (aux(r,(Right,l)::zip))
    |Leaf a,zip -> (a,zip)
  in aux (t,[])


let rec bzipper_left t = function
  |[]-> failwith "cannot move left of this positon"
  |(Right,tr)::z -> bzipper_left tr z
  |(Left,tl)::z-> bzipper_last tl


let rec bzipper_right t = function
  |[]-> failwith "cannot move right of this position"
  |(Left,tl)::z-> bzipper_right tl z
  |(Right,tr)::z -> bzipper_first tr;;



let btree_of_bzipper h =
  |(Left,Node (tl,tr)) -> 
