(******************************************************************************)
(*          Using Haar trees to compute cointegration coefficients            *)
(*                                                                            *)
(*                                                                            *)
(*                                                                            *)
(*                                                                            *)
(*                                                                            *)
(******************************************************************************)

open Str;;
open Haar;;
open Tree;;

(** Declarations *)
let read_input () =
  let sep = regexp "[ \t]*[,;]?[ \t]+" 
  and result = ref [] in
  try
    let first_line = read_line() in
      let tokens1 = Str.split sep first_line in
        let n = List.length tokens1 in
          result := List.map (fun x -> [float_of_string x]) tokens1;
          while true do
            let line = read_line() in
              let tokens = Str.split sep line in
                if (List.length tokens <> n) then
                  failwith "invalid input: inconsistent number of entries per
                  line"
                else
                  result := List.map2 (fun x y -> (float_of_string x) :: y) tokens
                  !result 
          done;
          !result
  with 
    End_of_file -> List.map List.rev !result 
    | e         -> raise e
;;


(* ugly little hack :) *)
let print_data_list data_list=
  try 
    let rec print_data lst =
      print_data (print_newline(); 
        List.map (function hd :: tl -> (Printf.printf "%f " hd;tl)) lst)
    in print_data data_list
  with 
    Match_failure(_)    -> ()
    | e                 -> raise e
;;
 
(* well, maybe using exceptions is not a bad way to terminate a side-effect
 * based recursion *) 
let transpose_list lst =
  let result = ref [] in
  try
    let rec transpose lists =
      let tmp_pair = List.split (List.map (function hd :: tl -> (hd,tl)) lists) 
      in
        (result := fst (tmp_pair) :: !result; transpose (snd tmp_pair))
    in transpose lst
  with
    Match_failure(_)    -> List.rev !result
    | e                 -> raise e       
;;

let rec mk_float_list n =
  if n = 0 then []
  else 0.0 :: mk_float_list (n - 1)
;;

let list_sum lst1 lst2 = List.map2 ( +. ) lst1 lst2 ;;

exception AverageOfAnEmptyList;;

(* average of a list of lists of floats *)
let list_avg lst = 
  let lst_len = List.length lst in
    if lst_len = 0 then raise AverageOfAnEmptyList
    else
      let res_tmp = List.fold_left list_sum 
        (mk_float_list (List.length (List.hd lst))) lst and
        f = 1.0 /. float_of_int(lst_len)
      in
        List.map (( *. ) f) res_tmp
;;

let recomb_node_to_str = function
  {avg=a; diff=d} -> Printf.sprintf "(%.4f;%.4f)" a d 
;;
    
let recomb_node_to_diff = function
  {diff=d} -> d
;;

let print_float_list lst =
  (Printf.printf "[";
   List.iter (fun x -> Printf.printf "%.4f " x) lst; 
   Printf.printf "]\n")
;;

let print_list_of_lists lst =
  List.iter (fun x -> (Printf.printf "["; List.iter print_float_list x; 
  Printf.printf "]\n")) lst;;

(* returns the first n elements of the list lst in reverse order. if n is
 * greater than the list length, returns the whole list *) 
let rev_list_head n lst =
  let rec rev_head n lst acc = 
    if n = 0 then acc
    else
      match lst with
        []              -> acc
        |hd :: tl       -> rev_head (n - 1) tl (hd :: acc)
  in rev_head n lst []
;;

let dot_prod lst1 lst2 =
  List.fold_left2 (fun a x y -> a +. x *. y) 0.0 lst1 lst2
;;

let normalize eps lst =
  let nrm = sqrt (dot_prod lst lst) in
    if (nrm < eps) then []
    else
      let f = 1.0 /. nrm in List.map (( *. ) f) lst
;;

let normalize_list lst = normalize (-.1.0) lst ;;

let neg_list lst =
  List.map (fun x -> -.x) lst
;;

let list_lin_comb a1 lst1 a2 lst2 =
  List.map2 (fun x y -> a1 *. x +. a2 *. y) lst1 lst2
;;

let flip_list dir lst =
  if (dot_prod dir lst) < 0.0 then neg_list lst else lst
;;

let epsilon = 0.0001;;
(* returns the estimate of the positive direction of the majority of vectors
 * (represented by float lists) contained in the lst, assuming they have unit
 * length *)
let list_dir lst =
  let rec update_dir dir n = function
    []          -> dir
    |hd :: tl -> 
      let v = flip_list dir hd in
      let newdir = normalize_list (list_lin_comb ((n -. 1.0) /. n) dir (1.0 /. n) v)
      in update_dir newdir (n +. 1.0) tl
  in update_dir (mk_float_list (List.length (List.hd lst))) 1.0 lst
;;

(* normalize each list in the list of lists of floats so that the individual
 * lists viewed as vectors all point in the same direction as dir and have unit
 * norm,  discarding the vectors that have almost zero norm *)
let normalize_avg lst =
  let auxlist = List.filter ( (<>) []) (List.map (normalize epsilon) lst) in
  let dir = list_dir auxlist in
    list_avg (List.map (flip_list dir) auxlist)
;;
(* "projective" normalize: scale the list so that the pos component is 1.0 *)
let proj_normalize pos lst =
  let f = 1.0 /. (List.nth lst pos) in List.map (( *. ) f) lst  

(* print the results corresponding to discarding different number of tree levels: 
 * from using just the root nodes to using the full trees*)
let print_coint_rel vect_lst =
  let print_coint_rel n =
    let coint_rel = normalize_avg (List.rev_map normalize_avg (rev_list_head n vect_lst))
    in Printf.printf "%d " n; print_float_list (proj_normalize 0 coint_rel) 
  in for i = (List.length vect_lst) downto 1 do print_coint_rel i done
;;

(** Main actions *)
(* read the data *) 
let data = read_input();;
let data_vecs = List.map Array.of_list data;;
(*check the reverse oprion *)
let rev = ref false;;

let opt_list = [("-r",Arg.Set rev,"reverse the list of Haar tree levels")] 
and usage_msg = "option -r results in reversing the list of Haar tree levels"
in Arg.parse opt_list (fun s-> ()) usage_msg;;

(* make generalized Haar trees for the data *)
let data_trees = List.map Haar.gen_haar_recomb data_vecs;;
(*List.map (Tree.print_tree recomb_node_to_str) data_trees;;*)

(* vectors is a list of lists of vectors (implemented as lists for now) of the
 * corresponding nodes of the Haar trees for each of the input series, arranged by
 * levels *) 
let diff_lists = List.map (Tree.breadth_first_map recomb_node_to_diff 0.0)
  data_trees;;
(* List.iter (List.iter print_float_list) diff_lists;; *)
let vectors = List.map transpose_list (transpose_list diff_lists);;
(*print_list_of_lists vectors;; *)

print_coint_rel (if (!rev) then List.rev vectors else vectors);;
print_newline();;
