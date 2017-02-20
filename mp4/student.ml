(*
 * File: mp4-skeleton.ml
 *)

open Mp4common

(* Problem 1*)
let asMonoTy1 () = failwith "Not implemented"
let asMonoTy2 () = failwith "Not implemented"
let asMonoTy3 () = failwith "Not implemented"
let asMonoTy4 () = failwith "Not implemented"

(* Problem 2*)
let rec subst_fun subst m = 
    match subst with
    [] -> TyVar(m)
    |(x,y) :: tl -> if (x=m) then y else subst_fun tl m

(* Problem 3*)
let rec monoTy_lift_subst subst monoTy = 
    match monoTy with
    TyVar x -> subst_fun subst x
    |TyConst(n, t) -> TyConst(n, List.map(monoTy_lift_subst subst) t)

(* Problem 4*)
let rec occurs x ty = 
    match ty with
    TyVar v -> if (v=x) then true else false
    |TyConst(n ,t) -> List.exists (occurs x) t

(* Problem 5*)
let rec unify eqlst = 
    match eqlst with
    [] -> Some([])
    |(s,t) :: tl -> if (s=t) then unify tl
                    else(
                            match s,t with
                            |TyConst(x,y), TyVar(v) -> unify ((t,s)::tl)
                            |TyConst(x,l1), TyConst(y,l2) -> 
                                            if ((x=y) && (List.length l1 = List.length l2)) then unify (tl @ (List.combine l1 l2))
                                                                                           else None
                            |TyVar(v),t1 -> if (occurs v t1)
                                            then None
                                            else(
                                                    let rec newtl tail subst =
                                                                match tail with
                                                                [] -> []
                                                                |(i,j) :: xs -> (
                                                                                    monoTy_lift_subst subst i,
                                                                                    monoTy_lift_subst subst j
                                                                                ) :: newtl xs subst
                                                                in
                                                    match unify (newtl tl [(v, t1)]) with
                                                     None -> None
                                                    |Some(g) -> Some((v, (monoTy_lift_subst g t))::g)
                                                        
                                                )                                          
                                            
                        )
    
    
    
   

(* Extra Credit *)
let equiv_types ty1 ty2 = failwith "Not implemented"
