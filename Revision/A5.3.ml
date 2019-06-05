

(* 1. 5
   2. 2
   3. 5
   4. 1
   5. 1 ?*)


module type DBLCIRCULAR =
sig
  exception Empty

  type 'a circular

  (* Create a CDLL initialised to a single element. *)
  val create : 'a -> 'a circular

  (* Return true only if the CDLL is empty. *)
  val empty  : 'a circular -> bool

  (* Return the current value pointed at. If the list is empty throw Empty. *)
  val peek   : 'a circular -> 'a

  (* Insert at the current location. *)
  val ins    : 'a -> 'a circular -> unit

  (* Remove the current element pointed at. If the list is empty throw Empty. *)
  val del    : 'a circular -> unit       

  (* Move the pointer towards the back of the CDLL. If the list is empty throw Empty. *)
  val fwd    : 'a circular -> unit

  (* Move the pointer towards the front of the CDLL. If the list is empty throw Empty. *)
  val rev    : 'a circular -> unit

  (* Fold a function over the contents of the CDLL. *)
  val fold   : 'a circular -> 'b -> ('b -> 'a -> 'b) -> 'b
end



module DbCircular: DBLCIRCULAR = struct
  exception Empty 
  type 'a circular_element = {mutable rev_ptr : 'a circular_element option ;
                              value : 'a option ;
                              mutable fwd_ptr : 'a circular_element option; }
  type 'a circular = {mutable current : 'a circular_element option}

  let val_of_option = function Some x -> x | None -> failwith"Value of Option" 

  let create v = 
    let value = Some v in 
    let rev_ptr = None in 
    let fwd_ptr = None in
    let new_element = {rev_ptr; value; fwd_ptr} in
    new_element.rev_ptr <- Some new_element;
    new_element.fwd_ptr <- Some new_element;
    let current = Some new_element in 
    {current}

  let empty xs = (xs.current == None)

  let peek xs = if empty xs then raise Empty else 
      (xs.current |> val_of_option).value |> val_of_option

  let ins v xs = 
    if empty xs then 
      begin
        let rev_ptr = None in 
        let fwd_ptr = None in 
        let value = Some v in 
        let new_element = {rev_ptr;value;fwd_ptr} in 
        new_element.rev_ptr <-Some  new_element;
        new_element.fwd_ptr <- Some new_element;
        let current = Some new_element in 
        xs.current <- current;
        ()
      end 
    else 
      begin
        let rev_ptr = (xs.current |> val_of_option).rev_ptr in
        let fwd_ptr = xs.current in 
        let value = Some v in 
        let new_element = {rev_ptr;value; fwd_ptr} in 
        (xs.current |> val_of_option).rev_ptr <- Some new_element; 
        xs.current <- Some new_element;
        ()
      end 


  let del xs = if empty xs then raise Empty else 
      begin
        let rev_ptr = (xs.current |> val_of_option).rev_ptr in 
        let fwd_ptr = (xs.current |> val_of_option).fwd_ptr in 
        if (xs.current |> val_of_option) == (rev_ptr |> val_of_option) then 
          begin
            xs.current <- None;
            ()
          end 
        else 
          begin
            (rev_ptr |> val_of_option).fwd_ptr <- fwd_ptr;
            (fwd_ptr |> val_of_option).rev_ptr <- rev_ptr;
            xs.current <- fwd_ptr;
            ()
          end 
      end 


  let fwd xs = if empty xs then raise Empty else 
      xs.current <- (xs.current |> val_of_option).fwd_ptr

  let rev xs = if empty xs then raise Empty else 
      xs.current <- (xs.current |> val_of_option).rev_ptr

  let pop xs = let x = peek xs in 
    del xs; 
    x

  let rec fold xs x0 f = match empty xs with  
    |true -> x0
    |false -> let x1 = pop xs in 
      fold xs (f x0 x1) f 
      


end
