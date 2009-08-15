(******************************************************************************)
(*                      tree module interface/ M.Korotyaev                    *)
(*                                                                            *) 
(*                                                                            *) 
(*                                                                            *) 
(******************************************************************************)

type 'a tree = Empty_Node | Node of 'a * 'a tree list ;;
(** generic planar tree type *)

val breadth_first_iter : ('a -> unit) -> (unit -> unit) -> (unit -> unit) -> 'a
  tree -> unit
(** iterates over a tree in the breadth-first order, call the second argument
 * for every Empty_Node node, and the third one after every level in the tree *)

val breadth_first_map : ('a -> 'b) -> 'b -> 'a tree -> 'b list list 
(** maps the first argument over a tree in the breadth-first order, subsitites
 * the second argument for every appearance of the Empty_Node, returns a list of
 * lists resulting from level-wise application of the first argument *)

val print_tree : ('a -> string) -> 'a tree -> unit
(** print breadth-first the nodes of a tree *)
