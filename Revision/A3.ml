type 'a btree = Leaf of 'a | Node of ('a btree * 'a btree)

(* L(a) = L(a) + L^2(a)  *)
type direction = Left | Right 
type 'a bzipper = (direction* 'a btree)list ;;

module type SOLUTION = sig 

  val bzipper_first : 'a btree -> 'a * 'a bzipper
  val bzipper_last : 'a btree -> 'a * 'a bzipper 
  val bzipper_left :   'a btree -> 'a bzipper -> 'a * 'a bzipper
  val bzipper_right :   'a btree -> 'a bzipper -> 'a * 'a bzipper

end 




module Solution: SOLUTION = struct

  let rec bzipper_first = function
    |Leaf a -> a, []
    |Node (btl, btr) -> let a,z = bzipper_first btl in 
      a,z @ [Right, btr]



  let rec bzipper_last = function 
    |Leaf a -> a ,[] 
    |Node (btl,btr) -> let a,z = bzipper_last btr in 
      a,z @ [Left, btl]

      

  let rec bzipper_left bt zip = match zip with 
    |[] -> failwith "zipper_left"
    |(Right, btr) :: zip -> bzipper_left (Node (bt,btr)) zip
    |(Left, btl) :: zip -> 
      let a, z' = bzipper_last btl in 
      a ,z' @ (Right, bt ) :: zip 

  let rec bzipper_right bt zip = match zip with 
    |[]-> failwith "zipper right"
    |(Left, btl) :: zip -> bzipper_right (Node (bt, btl)) zip
    |(Right, btr) :: zip -> 
      let a , z' = bzipper_first btr in 
      a, z' @ (Left, bt) :: zip

  ;;

end 
;;