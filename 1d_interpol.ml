
let (+..) = (+)
let ( * ) = ( *. )
let (+) = (+.)
let (-) = (-.)
let (/) = (/.)

let hat level index x = let index, level = (index |> float_of_int), (level |> float_of_int)
  in
  if x <=(index-1.0)/2.0**level then 0.0
  else if x<=index/2.0**level then 2.0**level*x+1.0-index
  else if x<=(index+1.0)/2.0**level then index+1.0-2.0**level*x
  else 0.0



module type Map = sig
  type ('a,'b) t
  val create: unit -> ('a, 'b) t
  val set: 'a -> 'b -> ('a,'b) t -> ('a, 'b) t
  val get: 'a -> ('a,'b) t -> 'b option
  val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
  (* val keys : ('a, 'b) t -> 'a list *)
end


module ListMap = struct
  type ('a,'b) t = ('a * 'b) list
  let create () = []
  let get k l = try Some (List.assoc k l)
                with Not_found -> None
  let remove k l = List.remove_assoc k l
  let set k v l = (k,v)::(remove k l)
  let keys l = List.fold_left (fun acc (k,_) -> k::acc) [] l
end


module TreeMap = struct
  type ('a, 'b) t = Node of ('a * 'b * ('a ,'b) t * ('a ,'b) t )
                  | Leaf

  let create () = Leaf
  
  let rec get (index, level) t = 
    let levelf = float_of_int level 
    in
    match t with
    | Leaf -> None
    | Node((index_, level_),v,l,r) ->
      let resf = (2.0**(1.0+levelf-float_of_int level_))
      in
      if float_of_int (index mod (int_of_float resf)) < resf/2.0 then
        get (index, level) l
      else if float_of_int (index mod (int_of_float resf)) > resf/2.0 then
        get (index, level) r
      else Some v


  let rec get_path (x:float) t = 
    match t with
    | Leaf -> []
    | Node((index_, level_),v,l,r) ->
      let resf = (float_of_int index_)/(2.0**(float_of_int level_))
      in
      if x<resf then 
        (index_,level_,v)::(get_path x l)
      else if x>resf then 
        (index_,level_,v)::(get_path x r)
      else [(index_,level_,v)]



  let rec set (index, level) (value:float) t = 
    let levelf = float_of_int level 
    in
    match t with
    | Leaf -> Node((index, level), value, Leaf, Leaf)
    | Node((index_, level_),v,l,r) ->
      let resf = (2.0**(1.0+levelf-float_of_int level_))
      in
      if float_of_int (index mod (int_of_float resf)) < resf/2.0 then
        Node((index_, level_), v,set (index, level) value l, r)
      else if float_of_int (index mod (int_of_float resf)) > resf/2.0 then
        Node((index_, level_), v,l, set (index, level) value r)
      else failwith "should not happen set"
    
   let keys t = 
    let rec aux acc t= match t with
      | Leaf -> acc 
      | Node(k,v,l,r) -> aux (k::aux acc l) r
    in
    aux [] t
end



let eval x m = 
  let path = TreeMap.get_path x m in
  List.fold_left (fun acc (i,l,v) -> (hat l i x)*v+acc) 0.0 path 


let hierarchize max_level f =  
  let rec aux_l (l:int) t = 
    if l>max_level then t
    else
      let rec aux_i (i:int) t = 
        let prev_surp = (eval ((float_of_int i)/2.0**(float_of_int l)) t)
        in
        let f_v = (f ((float_of_int i)/2.0**(float_of_int l)))-prev_surp 
        in
        if i>int_of_float (2.0**(float_of_int l)) 
        then aux_l (l+..1) t
        else 
        if (abs_float(f_v) < 0.0*10.0**(-9.0)) then
          aux_i (i+..2) t 
        else  aux_i (i+..2) (TreeMap.set (i,l) f_v t) 
      in
      aux_i 1 t
  in
  aux_l 1 (TreeMap.create ());;


let hierarchize2 max_level f =
  let rec aux i l t acc = 
    (* if l>max_level then t *)
    (* else if i>(int_of_float (((float_of_int i))*2.0-1.0) *)
    (* if i>int_of_float (2.0**(float_of_int l)) *) 
    (* then t *) 
    (* else *) 
      let prev_surp = (eval ((float_of_int i)/2.0**(float_of_int l)) t)
      in
      let f_v = (f ((float_of_int i)/2.0**(float_of_int l)))-prev_surp 
      in
      if l>max_level || (abs_float(f_v) < abs_float (1.0*10.0**(-5.0)*acc)) then
        (TreeMap.set (i,l) f_v t)
      else
        let new_index = int_of_float (((float_of_int i))*2.0-1.0) 
        in
        aux (new_index+..2) (l+..1) (aux new_index (l+..1) (TreeMap.set (i,l) f_v t) (acc+f_v)) (acc+f_v)
  in
  aux 1 1 (TreeMap.create ()) 0.0




let piecewise level f = 
  let levelf = float_of_int level
  in
  let rec aux c acc = 
    if c < 2.0**levelf then 
      aux (c+1.0) (fun x ->  acc x + (f ( (c)/2.0**levelf )) * hat level (int_of_float c) x)
    else fun x -> (acc x)
  in
  aux 1.0 (fun x -> 0.0 );;



(* (*evalulation for debug*) *)
(* let rec loop x border step = *) 
(*   if x<border then ((Printf.printf "%f\t%f\n" x (hat 4 15 x)); loop (x+step) border step) *)
(*   else ();; *)

(* let pw = piecewise 12 (fun x -> (x-1.0)*x*2.0*sin (4.0*x)) *)

(* let rec loop x border step = *)  
(*   if x<border then ((Printf.printf "%f\t%f\n" x (pw x)); loop (x+step) border step) *)
(*   else ();; *)

let m = hierarchize2 20 (fun x -> (x-1.0)*x*2.0*sin(2.0**(10.0*x) ))

let rec loop x border step =  
  if x<border then ((Printf.printf "%f\t%f\n" x (eval x m)); loop (x+step) border step)
  else ();;


let () = loop (-0.1) 1.1 0.0001


(* (1* let l = ListMap.create () *1) *)
