
(*****************************************************************************)
(*            Haar wavelt module unit tests// M. Korotyaev                   *)
(*                                                                           *)
(*****************************************************************************)

open Haar;;
open Tree;;
let float_eq eps x y = ((x -. y) > -. eps && (x -. y) < eps);;  

assert(inverse_haar [0.5; 0.5; 1.0; 2.5] = [1.0; 2.0; 3.0; 4.0]);;

let haar_test lst =   
  let h_t = (haar_transform lst) in 
    Printf.printf "original list: "; ignore (print_list lst);
    Printf.printf "Haar transform: "; ignore (print_list h_t);

    let i_h_t = inverse_haar h_t in
      Printf.printf "inv Haar transform: "; ignore(print_list (i_h_t));
      assert(List.for_all2 (float_eq 1.0e-10) i_h_t lst);
      Printf.printf"***\n"
;;

let test_list=[
  []; [1.0; 2.0; 3.0; 4.0]; 
  [1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0];
  [8.0; 7.0; 6.0; 5.0; 4.0; 3.0; 2.0; 1.0];
  [0.1; 0.8; 0.3; 0.5; 0.7; 0.2; 0.4; 0.6]
  ]
in List.map haar_test test_list;;

assert (float_eq 1.0e-10 (vec_avg [| 1.0; 1.0; 1.0 |]) 1.0);;

let mydata = [| 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0; 8.0 |];;
let mytree = gen_haar mydata;;
print_tree string_of_float mytree;;

let myrecomb_tree = gen_haar_recomb mydata;;
let recomb_node_to_str = function
  {avg=a; diff=d} -> "("^string_of_float(a)^";"^string_of_float(d)^")"
;;
print_tree recomb_node_to_str myrecomb_tree;;

