type 'a btree = Leaf of 'a | Node of ('a btree * 'a btree)

type direction = Left | Right 

type 'a bzipper = (direction * 'a btree)list 

module Solution = struct 
  (* type 'a btree = Leaf of 'a | Node of ('a btree * 'a btree)

  type direction = Left | Right 

  type 'a bzipper = (direction * 'a btree)list  *)

  let bzipper_first (bt:'a btree) =  
    let rec aux zip = function
      |Leaf a -> a,zip
      |Node (tl,tr) -> aux ((Left, tl)::zip) tl
    in aux [] bt     

  let bzipper_last bt = 
    let rec aux (zip: 'a bzipper) = function
      |Leaf a -> a,zip
      |Node (tl,tr) -> aux ((Right, tr)::zip) tr 
    in aux [] bt 


  let rec bzipper_left bt = function
    |[]-> failwith"zipper_left" 
    |(Right ,tr)::zip -> bzipper_left (Node(bt,tr)) zip 
    |(Left, tl) :: zip -> let (a , new_zip) = bzipper_last tl in (a, new_zip@((Right, tl)::zip))


  let rec bzipper_right bt = function
    |[]-> failwith"zipper_right"
    |(Left,tl):: zip -> bzipper_right (Node (bt,tl)) zip
    |(Right, tr) ::zip -> let (a, new_zip) = bzipper_first tr in (a, new_zip@((Left,tr)::zip))


end 