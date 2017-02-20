(* File: mp3.ml *)

open Mp3common

(* Problem 1 *)
let rec import_list lst = match lst with
    [] -> ConstExp NilConst
    | (x, y)::tl -> BinOpAppExp(ConsOp, BinOpAppExp(CommaOp, ConstExp(IntConst x) , ConstExp(IntConst y)), import_list tl )

(* Problem 2 *)
let pair_sums = LetRecInExp("pair_sums" , "lst", 

IfExp( BinOpAppExp(EqOp, VarExp "lst", ConstExp NilConst), ConstExp NilConst,
LetInExp("x" , MonOpAppExp (HdOp ,(VarExp "lst")) , 
BinOpAppExp(ConsOp,
    (BinOpAppExp(IntPlusOp, (MonOpAppExp (FstOp, (VarExp "x"))), (MonOpAppExp (SndOp, (VarExp "x"))) )), 
    (AppExp(VarExp "pair_sums", MonOpAppExp (TlOp ,(VarExp "lst")) ) ))

) 

)

, AppExp(VarExp "pair_sums" ,import_list [(7,1);(4,2);(6,3)] ))



(*   *)

(* Problem 3 *)
let rec count_const_in_exp exp =  match exp with
    |VarExp s -> 0
    |ConstExp a -> 1
    |BinOpAppExp(b, e1, e2) -> (count_const_in_exp e1) + (count_const_in_exp e2)
    |FunExp (x, e) -> count_const_in_exp e
    |AppExp (e1,e2) -> (count_const_in_exp e1) + (count_const_in_exp e2)
    |MonOpAppExp (m, e) -> (count_const_in_exp e)
    |IfExp (e1, e2, e3) -> (count_const_in_exp e1) + (count_const_in_exp e2) + (count_const_in_exp e3)
    |LetRecInExp (s, s1, e1, e2) ->  (count_const_in_exp e1) + (count_const_in_exp e2)
    |LetInExp (s, e1, e2) ->  (count_const_in_exp e1) + (count_const_in_exp e2)

(* Problem 4 *)
let rec freeVarsInExp exp = 
    match exp with
    |ConstExp _ -> []
    |VarExp  x -> [x]
    |IfExp (e1, e2, e3) -> freeVarsInExp e1 @ freeVarsInExp e2 @ freeVarsInExp e3
    |FunExp (x ,e) -> (List.filter (fun f -> if f=x then false else true) (freeVarsInExp e))
    |LetInExp (s, e1, e2) -> freeVarsInExp e1 @ ((List.filter (fun f -> if f=s then false else true) (freeVarsInExp e2)))
    |LetRecInExp (s1, s2, e1, e2) -> ((List.filter (fun f -> if (f=s1 || f=s2) then false else true)(freeVarsInExp e1))) @ ((List.filter (fun f1 -> if f1=s1 then false else true)(freeVarsInExp e2))) 
    

(* Problem 5 *)
let rec cps_exp e k = 
    match e with 
    |VarExp s -> VarCPS(k, s)
    |ConstExp e1 -> ConstCPS(k, e1)
    |IfExp (e1,e2,e3) -> 
        let v = (freshFor ((freeVarsInExp e1) @ (freeVarsInExp e2) @ (freeVarsInExp e3)) ) in
            (cps_exp e1 (FnContCPS(v, IfCPS(v, cps_exp e2 k, cps_exp e3 k))))
    |AppExp(e1, e2) ->
        let v2 = (freshFor ((freeVarsInExp e1) @ (freeVarsInExp e2))) in
            let v1 = (freshFor (v2 :: (freeVarsInExp e2))) in
                (cps_exp (e1) (FnContCPS( (v2) , (cps_exp (e2) (FnContCPS ( (v1), (AppCPS(k, v2,v1 )) )) ) ) ) )
    |LetInExp(s, e1, e2) ->
        cps_exp e1 (FnContCPS(s, cps_exp e2 k))
    |MonOpAppExp (m, e1) ->
        let v = freshFor(freeVarsInContCPS k) in 
        cps_exp e1 (FnContCPS(v, MonOpAppCPS(k, m, v )))
    |FunExp (s, e1) ->
        let v = freshFor(freeVarsInExp e1) in
            AppCPS( (FnContCPS(v, cps_exp e1 k)) , v, s )
            

