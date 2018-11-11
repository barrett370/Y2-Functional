let rec cont ys x = match ys with
  |[]->false
  |y::ys -> if x=y then true else cont ys x

let count_dup xs =
  let rec aux xs ys c = match xs with
    |[] -> c
    |x::xs -> if cont ys x then aux xs ys (c+1) else aux xs (x::ys) c
  in aux xs [] 0;;

count_dup [1;1;2;3;2;3;4];;
count_dup [1;11];;

count_dup [1;1;2;3;2;3;4;1];;

count_dup [1;1;2;3;2;3;4];;

count_dup [1;1;2;3;2;3;4;1];;




let find_nodes ys x =
  let rec aux ys acc= match ys with
    |[]-> (x,acc)
    |(y,y')::ys -> if (x=y) then aux ys (y'::acc)  else aux ys acc
  in aux ys [];;


let rec contains xs x = match xs with
  |[]->false
  |(x',xs')::xs -> if (x=x') then true else contains xs x;;


let trans_list xs =
  let rec aux xs acc = match xs with
    |[]->acc
    |(x,x')::xs -> if contains acc x then aux xs acc else aux xs ((find_nodes ((x,x')::xs) x )::acc)
  in aux xs [];;


let get_adj adj_m x =
  let rec aux adj_m = match adj_m with
    |[]->[]
    |(y,ys)::adj_m -> if (y=x) then ys else aux adj_m
  in aux adj_m ;;


let rec search xs x adj_m = match xs with
  |[]->false
  |x'::xs -> if (x=x') then true else (search xs x adj_m)||(search (get_adj adj_m x' ) x adj_m);;


let rec test_loop' adj_m = match adj_m with
  |[]->false
  |(x,adj)::adj_m -> ((search adj x adj_m ) || (test_loop' adj_m));;


let test_loop xs = trans_list xs |> test_loop' ;;

test_loop [(1,2); (1,3); (3,2); (3,4); (4,3)]
