(* CS421 - Fall 2016
 * MP1
 *
 * Please keep in mind that there may be more than one
 * way to solve a problem.  You will want to change how a number of these start.
 *)


(*Problem 1*)


let closer_to_origin p1 p2 = 
    let one (a, b) = a in 
    let two (a, b) = b in
    let dist point = sqrt (((one point)*.(one point)) +. ((two point)*.(two point))) in
    
    if dist p1 < dist p2 
        then -1 
        else
            if dist p2 < dist p1 
            then 1 
            else 0

(*Problem 2*)
let rec ackermann m n = 
    if m==0 then n+1
    else
    if (m>0 && n==0)
    then ackermann (m-1) 1
    else
    ackermann (m-1) (ackermann m (n-1)) 

(*Problem 3*)
let rec collatz n =
        if n==1 then 0 
        else
            if ((n mod 2)==0)
            then 1+ (collatz (n/2))
            else
            1 + (collatz ((n*3)+1))

(*Problem 4*)
let rec delannoy (m, n) =
        if (m==0 || n==0) then 1
        else
        (delannoy ((m-1),n)) + (delannoy (m,(n-1)) ) + (delannoy ((m-1),(n-1)))

(*Problem 5*)
let two_funs fns ins = 
                        let one (a, b) = a in 
                        let two (a, b) = b in
                        ((one fns) (one ins) , (two fns) (two ins) )

(*Problem 6*)
let rec product l = 
    match l with
    | [] -> 1.0
    | hd :: tl -> hd *. product tl

(*Problem 7*)
let rec double_all l = 
    match l with
    | [] -> []
    | hd :: tl -> (hd *. 2.0) :: double_all tl
    

(*Problem 8*)
let upto n = 
    let rec aux a b =
      if a > b then [] else a :: aux (a+1) b  in
     aux 0 n
    

(*Problem 9*)
let rec upuntil f = 
    let rec aux n =
        if (n==101 || f n)
        then n-1
        else
        aux (n+1) in
    let num = aux 0 in
    upto num
    


(*Problem 10*)
let rec pair_with_all x l = 
    match l with
    | [] -> []
    | h :: t -> (x, h) :: pair_with_all x t

(*Problem 11*)
let rec cross l1 l2 = 
    match l1 with
    | [] -> []
    | h :: t -> pair_with_all h l2 @ cross t l2 

(*Problem 12*)
let rec insert_by comp x l = 
    match l with
    | [] -> [x]
    | h :: t as l -> if  (comp h x == 1) then x :: l else h :: insert_by comp x t


(*Extra Credit - Problem 13*)
let rec collect_adjacent l = raise(Failure "Function not implemented yet.")