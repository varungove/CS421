open Mp2common

(*****************************)
(***** PROBLEMS FROM ML2 *****)
(*****************************)
(* Problem 1 *)
let even_count_fr_base = 0 
let even_count_fr_step x rec_val = if (x mod 2)=0 then (rec_val+1) else rec_val

(* Problem 2 *)
let pair_sums_map_arg p = 
    let one (a, b) = a in
    let two (a, b) = b in
    (one p + two p)

(* Problem 3 *)
let even_count_tr_start = 0
let even_count_tr_step acc_value x = if (x mod 2)=0 then (acc_value+1) else acc_value

(* Problem 4 *)
let count_element_start = 0
let count_element_step m acc_value x = if (x=m) then (acc_value+1) else (acc_value)

(* Problem 5 *)
let app_all_with fs b l = 
        let haha f =
        List.map (f b) l in
        List.map haha fs

(* Problem 6 *)
let exists_between_start = false
let exists_between_step m n acc_value x = if (x>=m && x<=n) then true else acc_value

(*****************************)
(****** PROBLEMS FOR MP2 *****)
(*****************************)
(***** Problem 7: Warmup (0 Points)  ******)
let consk (x, l) k = raise(Failure "Function not implemented yet.")
let concatk (s1, s2) k = k (s1^s2)
let string_of_intk n k = k (string_of_int n)
let truncatek n k = k (truncate n)

(***** Problem 8: Basic CPS *****)
let diff_flipk p k = subk (1,p) (fun j -> mulk(j,p) (fun i -> mulk(i,2) k))

(***** Problem 9: Basic CPS *****)
let quadk (a, b, c) k = 
            mulk (a, a) (fun r0 ->
            mulk (2, r0) (fun r1 ->
            mulk (4, b) (fun r2 ->
            addk (r1, r2) (fun r3 ->
            addk (r3, c) (fun r4 ->
            k r4
            )))))

(***** Problem 10: Basic CPS *****)
let three_freezek (s, p) k = 
            concatk (s, p) (fun r0 ->
            concatk (r0,r0) (fun r1 ->
            concatk (r1, r0) (fun r2 ->
            k r2
            )))

(***** Problem 11: Basic CPS *****)
let shiftk (s, q) k = 
            float_addk (q, 1.57) (fun r0 ->
            float_mulk (r0, r0)  (fun r1 ->
            truncatek  r1        (fun r2 ->
            string_of_intk r2    (fun r3 ->
            concatk (s, r3)      (fun r4 ->
            concatk (r4, s)      (fun r5 ->
            k r5
            ))))))
            

(***** Problem 12a: Recursion & CPS ******)
let rec list_prod l = 
    match l with
    | [] -> 1
    | hd :: tl -> hd * (list_prod tl)

(***** Problem 12b: Recursion & CPS ******)
let rec list_prodk l k = 
    match l with
    | [] -> k 1
    | hd :: tl -> list_prodk tl (fun r -> mulk(hd, r) k)

(***** Problem 13a: Recursion & CPS *****)
let rec all_positive l  = 
    match l with
    | [] -> true
    | hd :: tl -> if (hd>0) then all_positive tl else false

(***** Problem 13b: Recursion & CPS *****)
let rec all_positivek l k = 
    match l with
    | [] -> k true
    | hd :: tl -> if hd>0 then (all_positivek tl k) else k false

(***** Problem 14a: Recursion & CPS *****)
let rec even_count l = 
    match l with
    | [] -> 0
    | hd :: tl -> if (hd mod 2 = 0) then (1 + even_count tl) else (even_count tl)

(***** Problem 14b: Recursion & CPS *****)
let rec even_countk l k = 
    match l with
    | [] -> k 0
    | hd :: tl -> if (hd mod 2 = 0) then (even_countk tl (fun r -> addk(r, 1) k)) else ( even_countk tl k)


(********** EXTRA CREDIT **********)

(* Extra Credit, Problem 16a *)
let rec list_compose fs =  raise(Failure "Function not implemented yet.")

(* Extra Credit, Problem 16b *)
let rec list_composek fsk k = raise(Failure "Function not implemented yet.")
