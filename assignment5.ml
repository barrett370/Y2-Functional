module type DBLCIRCULAR = sig

  exception Empty
  type 'a circular
  val create : 'a -> 'a circular
  val empty : 'a circular -> bool
  val peek : 'a circular -> 'a
  val ins : 'a -> 'a circular -> unit
  val del: 'a circular -> unit
  val fwd : 'a circular -> unit
  val rev: 'a circular -> unit
  (* val fold : 'a circular -> 'b -> ('b -> 'a -> 'b) -> 'b *)

end





module CircSpec:DBLCIRCULAR = struct

  exception Empty;;
  type 'a circular_element = {mutable previous: 'a circular_element option; mutable value : 'a option ; mutable next : 'a circular_element option}
  type 'a circular = {mutable current: 'a circular_element option }

  let create  = fun x ->
    let previous, next = None, None in
    let value = Some x in
    let current = {previous ; value; next} in
    current.previous <- Some current ;
    current.next <- Some current ;
    let current = Some current in
    let ret = {current} in
    ret;;

  let (empty:'a circular -> bool) = fun (x:'a circular) ->
    x.current = None;;

  let extract_element = function
    |None -> failwith "can't extract"
    |Some x -> x;;



  let (peek:'a circular -> 'a) = fun x ->
    if
      empty x then raise Empty
    else
      let ret = (extract_element x.current).value in
      extract_element ret;;

  let ins x = fun (c:'a circular) ->
    let next  = c.current in
    let previous = (extract_element next).previous in
    let value = Some x in
    let current = {previous; value; next} in
    (extract_element next).previous <- Some current;
    c.current <- Some current ;
    ();;

  let del c =
    if
      empty c then raise Empty
    else
      let current' = extract_element((extract_element c.current).next) in
      current'.previous <- (extract_element c.current).previous ;
      c.current <- Some current' ;
      ();;


  let fwd (c:'a circular) =
    let previous =  extract_element c.current in
    let current' = previous.next in
    let np = previous.next in
    let pp = previous.previous in
    previous.next <- pp ;
    previous.previous <- np ;
    (extract_element current').previous <- Some previous;
    c.current <- current' ;
    ();;


  let rev (c:'a circular) =
    let next = extract_element c.current in
    let current' = next.previous in
    let np = next.next in
    let pp = next.previous in
    next.next <- pp;
    next.previous <- np;
    (extract_element current').next <- Some next ;
    c.current <- current' ;
    ();;

  (* let rec fold c b= fun f ->
    let current' = c.current in
    let previous = (extract_element current').previous in
    if
      empty c then raise Empty
    else
      let rec aux acc l=
        if  l.next = None then
          begin
            (extract_element previous).next <-current';
            f acc  l.value
          end
        else
          aux (f acc  l.value)   (extract_element l.next)
      in aux b (extract_element current')
 *)

end
;;
