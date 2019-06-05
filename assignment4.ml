

(*
  1. agda & Coq
  2. 2
  3. 1 3 4 ??
  4. ref
  5. a.(i)
  6. 3
  7. 1,4 & (2?)
  8. 3??
*)


exception Emptyq
type 'a queue_element = {value:  'a option ; mutable next_pointer: 'a queue_element option;}

type 'a queue = {mutable head_pointer : 'a queue_element; mutable tail_pointer: 'a queue_element;};;


let newq () =
  let value = None in
  let next_pointer = None in
  let head_pointer = {value ; next_pointer} in
  let tail_pointer = head_pointer in
  let queue = {head_pointer;tail_pointer} in
    queue ;;

(* let newq = newq' () *)

let enq (q:'a queue) (x:'a) = 
  let value = Some x in
  let next_pointer = None in
  let new_elem = {value;next_pointer} in
  if q.head_pointer.value = None then
    let old_tail = q.tail_pointer in
    q.head_pointer <- new_elem ;
    old_tail.next_pointer <- Some(new_elem) ;
    q.tail_pointer <- new_elem ;
  else
    let old_tail = q.tail_pointer in
    old_tail.next_pointer <- Some(new_elem) ;
    q.tail_pointer <- new_elem ;
  ();;



let contents_of_q = function
  |None -> let x = newq () in x.head_pointer
  |Some c -> c ;;

let contents = function
  |None -> failwith"cannot extract value"
  |Some c -> c

let deq q =
  if
    q.head_pointer.value = None then raise Emptyq
  else
    let old_head = q.head_pointer in
    q.head_pointer <- (contents_of_q old_head.next_pointer) ;
    old_head.value |> contents

let isempty q = if q.head_pointer.value = None && q.head_pointer.next_pointer = None then true else false
