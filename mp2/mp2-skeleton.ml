open Mp2common

(*****************************)
(***** PROBLEMS FROM ML2 *****)
(*****************************)
(* Problem 1 *)
let even_count_fr_base = max_int (* You want to change this *)
let even_count_fr_step x rec_val = raise(Failure "Function not implemented yet.")

(* Problem 2 *)
let pair_sums_map_arg p = raise(Failure "Function not implemented yet.")

(* Problem 3 *)
let even_count_tr_start = min_int
let even_count_tr_step acc_value x = raise(Failure "Function not implemented yet.")

(* Problem 4 *)
let count_element_start = max_int
let count_element_step m acc_value x = raise(Failure "Function not implemented yet.")

(* Problem 5 *)
let app_all_with fs b l = raise(Failure "Function not implemented yet.")

(* Problem 6 *)
let exists_between_start = (true || false) (* You want to change this *)
let exists_between_step m n acc_value x = raise(Failure "Function not implemented yet.")

(*****************************)
(****** PROBLEMS FOR MP2 *****)
(*****************************)
(***** Problem 7: Warmup (0 Points)  ******)
let consk (x, l) k = raise(Failure "Function not implemented yet.")
let concatk (s1, s2) k = raise(Failure "Function not implemented yet.")
let string_of_intk n k = raise(Failure "Function not implemented yet.")
let truncatek n k = raise(Failure "Function not implemented yet.")

(***** Problem 8: Basic CPS *****)
let diff_flipk p k = raise(Failure "Function not implemented yet.")

(***** Problem 9: Basic CPS *****)
let quadk (a, b, c) k = raise(Failure "Function not implemented yet.")

(***** Problem 10: Basic CPS *****)
let three_freezek (s, p) k = raise(Failure "Function not implemented yet.")

(***** Problem 11: Basic CPS *****)
let shiftk (s, q) k = raise(Failure "Function not implemented yet.")

(***** Problem 12a: Recursion & CPS ******)
let rec list_prod l = raise(Failure "Function not implemented yet.")

(***** Problem 12b: Recursion & CPS ******)
let rec list_prodk l k = raise(Failure "Function not implemented yet.")

(***** Problem 13a: Recursion & CPS *****)
let rec all_positive l  = raise(Failure "Function not implemented yet.")

(***** Problem 13b: Recursion & CPS *****)
let rec all_positivek l k = raise(Failure "Function not implemented yet.")

(***** Problem 14a: Recursion & CPS *****)
let rec even_count l = raise(Failure "Function not implemented yet.")

(***** Problem 14b: Recursion & CPS *****)
let rec even_countk l k = raise(Failure "Function not implemented yet.")


(********** EXTRA CREDIT **********)

(* Extra Credit, Problem 16a *)
let rec list_compose fs =  raise(Failure "Function not implemented yet.")

(* Extra Credit, Problem 16b *)
let rec list_composek fsk k = raise(Failure "Function not implemented yet.")
