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


module DbCircular : DBLCIRCULAR = struct
  exception Empty 

  type 'a element = {mutable rev_pointer : 'a element option ; value : 'a option ; mutable fwd_pointer : 'a element option}

  type 'a circular = {mutable current : 'a element option}

  let mult = function Some a-> a | None -> failwith"mult"

  let create v = 
    let value = Some v in 
    let rev_pointer = None in 
    let fwd_pointer = None in 
    let current = {rev_pointer; value; fwd_pointer} in 
    begin
      current.rev_pointer <- Some current;
      current.fwd_pointer <- Some current;

      {current = Some current}
    end 

  let empty xs = if xs.current = None then true else false 



  let peek xs = if not (empty xs) then ((mult xs.current).value)|>mult else raise Empty

  let ins v xs = 
    let value = Some v in 
    let rev_pointer = (mult xs.current).rev_pointer in
    let fwd_pointer = xs.current in 
    let new_elem = {rev_pointer; value; fwd_pointer} in 
    begin
      (xs.current |> mult).rev_pointer <- Some new_elem;
      xs.current <- Some new_elem;
      ()
    end 

  let del xs = 
    let rev_pointer = (xs.current |> mult).rev_pointer in 
    let fwd_pointer = (xs.current |> mult).fwd_pointer in 
    if (rev_pointer |> mult) == (fwd_pointer |> mult) then
      begin
        xs.current <- None;
        ()
      end 
    else
      begin
        (rev_pointer |>mult).fwd_pointer <- fwd_pointer;
        (fwd_pointer |>mult).rev_pointer <- rev_pointer;
        xs.current <- fwd_pointer;
        ()
      end 

  let fwd xs = 
    if not (empty xs) then 
      begin
        xs.current <- (xs.current|>mult).fwd_pointer;
        ()
      end
    else 
      raise Empty

  let rev xs = 
    if not (empty xs) then begin
      xs.current <- (xs.current |> mult).rev_pointer;
      ()
    end
    else 
      raise Empty

  let rec fold xs x0 f = match (empty xs) with 
    |true -> x0
    |false -> del xs; fold xs (f x0 (mult (mult xs.current).value)) f ;

end