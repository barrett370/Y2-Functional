
exception Empty 

type 'a circular_element = {mutable rev_pointer: 'a circular_element option  ; value : 'a option ; mutable fwd_pointer : 'a circular_element option}

type 'a circular = {mutable current : 'a circular_element}

let mult = function Some x -> x | None -> failwith"None type cannot be stripped" 

let (create:'a -> 'a circular)  = fun v ->  
  let rev_pointer = None in 
  let fwd_pointer = None in 
  let value = Some v in 
  let element = {rev_pointer; value; fwd_pointer} in 
  element.rev_pointer <- Some element;
  element.fwd_pointer <- Some element;
  {current = element}


let (empty: 'a circular -> bool) =fun c ->  if c.current.value = None then true else false 


let (peek:'a circular -> 'a ) = fun c -> match c.current.value with 
  | None -> raise Empty
  |Some x -> x

let (ins: 'a -> 'a circular ->unit) = fun v -> fun c ->
  if c |> empty then begin
    let new_c = create v in  
    c.current <- new_c.current;
    ()
  end   
  else 
    let old_head = c.current.rev_pointer in 
    let new_element = {rev_pointer = old_head ; value = Some v ; fwd_pointer = Some c.current} in 
    c.current.rev_pointer <- Some new_element;
    (mult c.current.fwd_pointer).fwd_pointer <- Some new_element;
    (* if mult c.current.fwd_pointer = c.current then c.current.fwd_pointer <-Some new_element; *)
    c.current <- new_element;
    ()

let (del: 'a circular -> unit) = fun c -> 
  let elem_2 = c.current.fwd_pointer in 
  let elem_n = c.current.rev_pointer in 
  if (mult c.current.rev_pointer ) == (mult c.current.fwd_pointer)then 
    begin
      let rev_pointer = None in 
      let fwd_pointer = None in 
      let value = None in 
      let element = {rev_pointer; value; fwd_pointer} in 
      element.rev_pointer <- Some element;
      element.fwd_pointer <- Some element;
      c.current <- element;
      ()
    end 
  else 
    begin
      (mult elem_n).fwd_pointer <- elem_2;
      (mult elem_2).rev_pointer <- elem_n;
      c.current <- elem_2 |> mult;
      ()
    end 

let (fwd: 'a circular -> unit) = fun c -> 
  if c |> empty then
    raise Empty 
  else 
    c.current<- mult c.current.fwd_pointer;
  ()

let (rev: 'a circular -> unit) = fun c ->
  if c |> empty then  
    raise Empty 
  else 
    c.current <- mult c.current.rev_pointer;
  ()

(* let (fold: 'a circular -> 'b -> ('b -> 'a -> 'b) -> 'b ) = fun c f -> 
   let rec aux c f acc = 
    if (empty c) then 
      acc; 
   else 
   f (mult c.current.value) (aux (del c) f acc) ;

   in aux c f 0
*)
