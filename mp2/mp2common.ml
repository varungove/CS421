(* File: ml2common.ml *)

let output_str = ref "";;

let print_string str =
  (output_str := !output_str ^ str;
  Pervasives.print_string (str));;

let print_int n = print_string ((string_of_int n) );;
let print_float x = print_string ((string_of_float x));;
let print_newline () = print_string "\n";;
let print_endline s = print_string (s^"\n");;

let print_string_list l = print_string ("[" ^ String.concat "; " l ^ "] ");;
(*
  let int_list l = "[" ^ String.concat "; " (List.map string_of_int l) ^ "] ";;
*)
let print_int_list l = print_string ("[" ^ String.concat "; " (List.map string_of_int l) ^ "]");;
let print_float_list l = print_string ("[" ^ String.concat "; " (List.map string_of_float l) ^ "] ");;
let print_pair_int_list (x,y) =
  print_string ("(" ^
                "[" ^ String.concat "; " (List.map string_of_int x) ^ "]" ^
                ", " ^
                "[" ^ String.concat "; " (List.map string_of_int y) ^ "]" ^
                ")");;

let report_float x =
   print_string "Result: ";
   print_float x;
   print_newline();;

let report_int x =
   print_string "Result: ";
   print_int x;
   print_newline();;


let inck n k = k (n + 1);;
let deck n k = k (n - 1);;
let addk (a, b) k = k (a + b);;
let subk (a, b) k = k (a - b);;
let mulk (a, b) k = k (a * b);;
let modk (a, b) k = k (a mod b);;
let float_addk (a, b) k = k (a +. b);;
let float_subk (a, b) k = k (a -. b);;
let float_mulk (a, b) k = k (a *. b);;
let geqk (a, b) k = k (a >= b);;
let eqk (a, b) k = k (a = b);;
let notk b k = k (not b);;
