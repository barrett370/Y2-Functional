

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


module CircSpec : DBLCIRCULAR = struct
  exception Empty

  type 'a circular =
    { mutable data : 'a list; }

  let create : 'a -> 'a circular = fun a ->
    let data = [a] in
    {data;}
    
  let empty  : 'a circular -> bool = fun q ->
    q.data = []
    
  let peek   : 'a circular -> 'a = fun q ->
    List.hd q.data

  let ins    : 'a -> 'a circular -> unit = fun a q ->
    q.data <- a :: q.data

  let del    : 'a circular -> unit = fun q ->
    q.data <- List.tl q.data

  (* Helper function, not exported *)
  let rotate xs = (List.tl xs) @ [List.hd xs]
  
  let fwd    : 'a circular -> unit = fun q ->
    q.data <- rotate q.data
      
  let rev    : 'a circular -> unit = fun q -> 
    q.data <- q.data |> List.rev |> rotate |> List.rev
      
  let fold   : 'a circular -> 'b -> ('b -> 'a -> 'b) -> 'b = fun q b f ->
    List.fold_left f b q.data
end
