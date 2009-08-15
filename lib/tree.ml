
(*****************************************************************************)
(*                   generic tree module // M Korotyaev                      *)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)


type 'a tree = Empty_Node | Node of 'a * 'a tree list ;;

(* allows for a simple recombination in a tree, where the last child of the 
 * previous node is the same as the first child of the next one*)
let list_rev_merge lst1 lst2 = 
  if (lst1 = [] || lst2 = []) then List.rev_append lst1 lst2
  else
    List.rev_append 
    (if (List.hd lst1 == List.hd lst2) 
      then (List.tl lst1)  else lst1) 
    lst2
;;     

let breadth_first_iter f e_f lvl_f tr =
  let process_node lst = function
    Node(value, children)       -> (f value; list_rev_merge children lst)
    |Empty_Node                 -> (e_f (); lst)
  in
    let rec process_node_lst lst =
      if lst = [] then []
      else
        let new_nodes = List.rev (List.fold_left process_node [] lst)
        in  (lvl_f ();process_node_lst new_nodes)
    in
    (ignore(process_node_lst [tr]); ())
;;

let breadth_first_map f e_value tr =
  let process_node res = function  
    Node(value, children)   -> ((f value) :: (fst res),
      list_rev_merge children (snd res))
    |Empty_Node             -> (e_value :: (fst res), snd res)
  in
    let rec process_node_lst lst =
      if lst = [] then []
      else
        let aux_pair = List.fold_left process_node ([],[]) lst 
        in (List.rev (fst aux_pair)) :: process_node_lst (List.rev (snd aux_pair))
    in
    process_node_lst [tr]
;;


let breadth_first_fold_left f a tr =
        ()
;;

let breadth_first_fold_left2 f a tr1 tr2 =
        ()
;;

let print_tree string_of_node tr =
  breadth_first_iter (fun x -> print_string ((string_of_node x)^" "))
    (fun () -> print_string "E ") (fun () -> print_newline ()) tr
;;
