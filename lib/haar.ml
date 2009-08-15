(*****************************************************************************)
(*                   Haar wavelet module // M Korotyaev                      *)
(*   A note on terminology: Haar transform here refers to the vector of      *)
(*   Haar wavelet coefficients                                               *)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)

open Tree;;

(** Auxilliary functions *)

let rec print_list = function
  | []          -> (Printf.printf "EMPTY LIST\n"; ())
  | a :: []     -> (Printf.printf "%f\n" a; ())
  | a :: tail   -> ((Printf.printf "%f, " a); print_list tail)
;;

(* compute the average of a vector of floats *)
let vec_avg vec =
  match Array.length vec with
    0   -> failwith "Cannot compute the average of an empty array"
    | 1 -> vec.(0)
    | n -> (Array.fold_left ( +. ) 0.0 vec) /. float_of_int(n)


(** Haar transforms *)
(* calculate the Haar transform of a list *) 
let haar_transform lst =
  let rec haar_aux lst_aux = 
    match lst_aux with 
    | []             -> []   
    | a :: []        -> lst_aux
    | _              ->
      (
        (* ignore (print_list lst_aux); *)
        let rec haar_inner  = function
          | []              -> []
          | a :: b :: tail  -> (0.5 *. (b -. a), 0.5 *. (a +. b)) :: 
                               (haar_inner tail)
          | a :: []         -> failwith "Cannot compute the Haar transform of a \ 
                               list whose length is not a power of 2"
        in 
          let (diffs,sums)=List.split(haar_inner lst_aux) 
          in 
            diffs @ (haar_aux sums)
      )     
      
  in haar_aux lst ;;

(* recover the original list from its Haar transform *)
let inverse_haar lst = 
  if lst = [] then []
  else
    let rec  inv_haar_aux (coeff, data)  = 
      if coeff = [] then List.rev data
      else
        if data = [] then inv_haar_aux ((List.tl coeff), ((List.hd coeff) :: []))
        else
          let rec inv_haar_inner coeff_outer data_outer data_inner =
            match (coeff_outer,data_outer) with
               (_,[])                           -> (coeff_outer,(List.rev
               data_inner))
              |(a :: c_tail, b :: d_tail)       -> inv_haar_inner c_tail d_tail
                ((b -. a) :: (b +. a) :: data_inner)
              |([],_)                           -> failwith "invalid Haar\
                                                   transform" 
          in 
            inv_haar_aux (inv_haar_inner coeff data [])
      in inv_haar_aux ((List.rev lst), [])
;;  


(* compute the generalized Haar transform of a vector *)
let rec gen_haar vec =
  match Array.length vec with
    0 | 1 -> failwith "Invalid input, data vector is too small"
    | 2   -> Node (vec.(1) -. vec.(0), [])
    | n   -> let mid = n / 2 + (if (n mod 2) > 0 then 1 else 0) in
               let left_sub = (Array.sub vec 0 mid) and
                 right_sub = (Array.sub vec (n-mid) mid) 
               in
                 Node((vec_avg right_sub) -. (vec_avg left_sub),
                 [gen_haar left_sub; gen_haar right_sub])
                 
;; 

type haar_tree_node = {avg : float; diff : float};;

let make_tree_node x1 x2 = {avg=0.5 *. (x1 +. x2); diff = x2 -. x1};;

let rec make_mid_tree ltree rtree =
  match ltree, rtree with
    Node ({avg = a1; diff = d1},lst1), Node ({avg = a2; diff = d2},lst2) ->
      if ( lst1 = [] || lst2 = [] ) then
        Node (make_tree_node (a1 +. 0.5 *. d1) (a2 -. 0.5 *. d2),[])
      else 
        let left_tree = List.hd (List.rev lst1) and
          right_tree = List.hd lst2
        in
          Node (make_tree_node (a1 +. 0.5 *. d1) (a2 -. 0.5 *. d2),
                [left_tree; (make_mid_tree left_tree right_tree); right_tree])  
    | Empty_Node,_ |_, Empty_Node                   -> Empty_Node
;;

let gen_haar_recomb vec =
  let rec gen_haar_aux vec =
    match Array.length vec with
    0 | 1 -> failwith "Invalid input, data vector is too small"
    | 2   -> Node (make_tree_node vec.(0) vec.(1), [])
    | n   -> let mid = n / 2 + (if (n mod 2) > 0 then 1 else 0) in
               let left_sub = (Array.sub vec 0 mid) in
               let left_avg = (vec_avg left_sub) in
               let right_sub = (Array.sub vec (n-mid) mid) in 
               let right_avg = (vec_avg right_sub) 
               in
                 let left_tree = gen_haar_aux left_sub and
                   right_tree = gen_haar_aux right_sub  
                 in
                   Node (make_tree_node left_avg right_avg,
                   [left_tree; (make_mid_tree left_tree right_tree); 
                   right_tree])
  in gen_haar_aux vec 
;;

