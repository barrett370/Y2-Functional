
(* 21 *)

let insert_at y i xs = 
  let rec aux acc xs c = match xs,c with 
    |[],_ -> acc
    |x::xs, c -> if c = i then aux (y::acc) xs (c+1) else aux (x::acc) xs (c+1)
  in List.rev(aux [] xs 0);;


(* 22 *)

let range i k = 
  let rec aux acc = function
    |c -> if c >=i && c<=k then aux (c::acc) (c+1) else if c>k then acc else aux acc (c+1)
  in List.rev(aux [] 0);;



