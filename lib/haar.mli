
(*****************************************************************************)
(*                   Haar wavelet module // M Korotyaev                      *)
(*   A note on terminology: Haar transform here refers to the vector of      *)
(*   Haar wavelet coefficients                                               *)
(*                                                                           *)
(*                                                                           *)
(*****************************************************************************)

(** Auxilliary functions *)

type haar_tree_node = {avg : float; diff : float};;
(** recombinant Haar tree node type *)

val print_list : float list -> unit
(** print the contents of a list of floats *)

val vec_avg : float array -> float
(** compute the average of an array of floats *)

(** Haar transforms *)

val haar_transform : float list -> float list
(** calculate the Haar transform of a list *) 

val inverse_haar : float list -> float list
(** recover the original list from its Haar transform *)

val gen_haar : float array -> float Tree.tree
(** compute the generalized Haar transform of a vector *)

val gen_haar_recomb : float array -> haar_tree_node Tree.tree
(** build the recombinant Haar tree of a float vector *)
