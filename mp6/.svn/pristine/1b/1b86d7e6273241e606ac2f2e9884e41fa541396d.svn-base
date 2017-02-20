open Mp6common;;

(* Refered Fall 2014 MP10 for understanding on certain parts I was stuck on *)

let const_to_val c = 
        match c with
        | BoolConst b  -> BoolVal b
        | IntConst i   -> IntVal i
        | StringConst s-> StringVal s
        | FloatConst f-> FloatVal f
        | NilConst     -> ListVal []
        | UnitConst   -> UnitVal

let monOpApply op v = 
    match op, v with
    | IntNegOp, IntVal i -> IntVal(-i)
    | HdOp, ListVal (hd::tl) -> hd
    | HdOp, ListVal [] -> raise (Failure "Not implemented yet")
    | TlOp, ListVal (hd::tl) -> ListVal tl
    | TlOp, ListVal [] -> raise (Failure "Not implemented yet")
    | FstOp, PairVal(f,s) -> f
    | SndOp, PairVal(f,s) -> s
    | PrintOp, StringVal str -> (print_string str; UnitVal)
    

let binOpApply binop (v1,v2) = 
    match binop, v1, v2 with
    | IntPlusOp, IntVal n, IntVal m -> IntVal(n+m)
    | IntMinusOp, IntVal n, IntVal m -> IntVal(n-m)
    | IntTimesOp, IntVal n, IntVal m -> IntVal(n*m)
    | IntDivOp, IntVal n, IntVal m -> 
                        if m=0 then Exn(0) else IntVal(n/m)
    | FloatPlusOp, FloatVal n, FloatVal m -> FloatVal(n+.m)
    | FloatMinusOp, FloatVal n, FloatVal m -> FloatVal(n-.m)
    | FloatTimesOp, FloatVal n, FloatVal m -> FloatVal(n*.m)
    | FloatDivOp, FloatVal n, FloatVal m -> 
                        if m=0.0 then Exn(0) else FloatVal(n/.m)
    | ConcatOp, StringVal s1, StringVal s2 -> StringVal(s1^s2)
    | ConsOp, _, ListVal l -> ListVal (v1::l)
    | CommaOp, _, _ -> PairVal (v1, v2)
    | EqOp, _, _ -> if v1=v2 then (BoolVal true) else (BoolVal false)
    | GreaterOp, _, _ -> if v1>v2 then (BoolVal true) else (BoolVal false)

let rec eval_exp (exp, m) = 
        match exp with
    | ConstExp c -> const_to_val c
    | VarExp ve ->
            (
            match lookup_mem m ve with
            | RecVarVal(f, x, e', m') -> Closure(x, e', ins_mem m' f (RecVarVal(f, x, e', m')))
            | var -> var
            )    
    | IfExp(e1,e2,e3) ->
            (
            match eval_exp (e1,m) with
            | (BoolVal true) -> eval_exp(e2, m)
            | (BoolVal false) -> eval_exp(e3, m)
            | Exn i -> Exn(i) 
            )
    | LetInExp(x,e1,e2) -> 
            (
            match eval_exp (e1,m) with
            | Exn i -> Exn(i)
            | v -> eval_exp(e2, ins_mem m x v)
            )
    | MonOpAppExp (mon_op, e) -> 
            (
            match eval_exp(e,m) with
            | Exn i -> Exn(i)
            | v -> monOpApply mon_op v
            )           
    | BinOpAppExp (binop, e1,e2) ->
            (match eval_exp (e1,m) with
            | Exn i ->Exn(i)
            | v1 -> (match eval_exp (e2,m) with
                | Exn k -> Exn(k)
                | v2 -> binOpApply binop (v1,v2)))                
    | FunExp(f,g) -> Closure(f,g,m)
    | AppExp(e1,e2) -> 
            (match eval_exp(e1, m) with
            | Exn i -> Exn(i)
            | Closure(x, e', m') ->
                    ( match eval_exp(e2, m) with
                    | Exn i -> Exn(i)
                    | v -> eval_exp(e', ins_mem m' x v)))
    | LetRecInExp(f,x,e1,e2) -> eval_exp(e2, ins_mem m f (RecVarVal(f, x, e1, m)))
   

let eval_dec (dec, m) = 
    match dec with
    | Anon ep -> ((None,eval_exp (ep, m)),m)
    | Let (x, exp) -> 
                (match eval_exp (exp, m) with
                | Exn r -> ((None, Exn r), m)
                | v -> ((Some x, v), ins_mem m x v))
    | LetRec (f, x, exp) -> 
        let temp = RecVarVal(f, x, exp, m) in
            ((Some f, temp), ins_mem m f temp)
    | _ -> raise (Failure "Not implemented yet")
 