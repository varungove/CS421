(* CS421 - Fall 2016
 * ML1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)

open Mp1common


let random = 179  (* You want to change this *)

let title = "ML 1 -- Basic OCaml";; (* You want to change this *)

let greetings = "Hi there.";; (* You want to change this *)

let salute = "Greetings, my friend!";; (* You want to change this *)

let a = 17;; (* You want to change this *)

let pi = 3.14159 (* You want to change this *)

let e = 2.71828;; (* You want to change this *)

let quarter = 0.25;; (* You want to change this *)

let x = 32.7;; (* You want to change this *)

let s = "Hi there";; (* You want to change this *)

let myFirstFun n = (n+3)*4

let firstFun n = (n*2)+5

let square n = n*n

let times_13 n = n*13

let add_a n = n+a

let circumference r = 2.0*.pi*.r

let divide_e_by x = e/.x

let plus_quarter_times_3 y = (y+.quarter)*.3.0

let square_plus_x y = (y*.y)+.x ;;

let double_minus_one x = (x*2)-1

let diff_square_9 m = if (m>(-3) && m<3) then ((square m)-9) else (9-(square m))

let salutations name =  if name="Elsa" then (print_string "Halt! Who goes there!\n") else (print_string ("Hail, " ^ name ^ ". We warmly welcome you!\n")) 

let hail name = if name="Elsa" then (print_string "Wayell, hah theya, Ayelsa!") else (print_string ("Dear, " ^ name ^ ". I wish you the best in CS421.\n" )) 

let abs_diff x y =  if (x>y) then x-.y else y-.x

let greet name = if name = "Elsa" then (print_string "Hey Elsa, cool man!") else (print_string ("Hello, " ^ name ^ ". I hope you enjoy CS421.\n")) 

let make_bigger x = if x<0.0 then x*.(-1.0) else if (x>=0.0 && x<1.0) then x+.0.5 else x*.x

let dist_double s n = print_string ("" ^ s ^ ", I guess it's double or nothing!\n") ; n+n

let has_smallest_square m n = if ((square m)>(square n)) then n else if ((square n)>(square m)) then m else if m<n then m else n

let has_smallest_abs m n =  if (m>=0 && n>=0) then ( if m>n then n else m) else if (m=n || m+n=0) then n else if (m<0 && n>=0) then ( if (m*(-1))>n then n else m) else (if (n*(-1))>m then m else n)

let sign n = if n>0 then 1 else if n=0 then 0 else (-1)

