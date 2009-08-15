(******************************************************************************)
(*                          Tree module unit tests                            *)
(*                                                                            *)
(******************************************************************************)

open Tree;;

let mytree = Node(1, 
  [ Node(2, [Node(3,[]); Empty_Node]); 
    Node(4, [Node(5,[]); Node(6,[])]); 
    Node(7, [])])
;;

breadth_first_map (fun x -> x) 0 mytree;;
let print_int_tree tr =
  breadth_first_iter (fun x -> print_string ((string_of_int x)^" "))
    (fun () -> print_string "Empty ") (fun () -> print_newline ()) tr
;;
print_int_tree mytree;;

let mybigtree = Node(-5,
  [ Node(-4,[ Node(-3,[]); mytree]);
    Node(-2,[ mytree; Node(-1,[])])
  ]);;

print_int_tree mybigtree;;
breadth_first_map (fun x -> x) 0 mybigtree;;

