module type IMPQ = sig 
  exception Emptyq 
  type 'a queue
  val newq : unit -> 'a queue   (* create new empty queue *)
  val enq : 'a queue -> 'a -> unit  (* add to the back of the queue *)
  val deq : 'a queue -> 'a  (* remove to the front of the queue, throw Emptyq if empty *)
  val isempty : 'a queue -> bool  (* test if a queue is empty *)
end 


module ImpQ : IMPQ = struct

  exception Emptyq

  type 'a element = {value : 'a ; mutable next : 'a element option;}
  type 'a queue = {mutable head : 'a element option ; mutable tail : 'a  element option;} 

  let mult = function Some x -> x | None -> failwith"None type cannot be stripped" 

  let newq () = {head =  None ; tail = None ;}

  let enq (q: 'a queue) (v:'a)  = 
    let value = v in 
    let next = None in 
    let new_element = {value; next} in 
    if q.head = None  && q.tail = None then 
      begin
        q.head <- Some new_element;
        q.tail <- Some new_element
      end
    else 
      begin
        (mult q.tail).next <- Some new_element;
        q.tail <- Some new_element
      end 



  let deq q = 
    if q.head = None || q.tail = None then 
      raise Emptyq 
    else if q.head = q.tail then 
      begin
        let v = (mult q.head).value in 
        q.head <- None;
        q.tail <- None; 
        v
      end 
    else
      let v = (mult q.head).value in
      q.head <- (mult q.head).next;
      v 







  let isempty q = if q.head = None then true else false;;


end


