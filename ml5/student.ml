(* File: ml5.ml *)

open Ml5common;;

let rec app_cont_to_value env k v = 
match k with 
    |ContVarCPS i ->
     (match lookup_cont env i with 
      | None -> Failed
      | Some (k',env') -> app_cont_to_value env' k' v)
    | FnContCPS (y, ec) -> Intermediate(ValueBinding(y,v)::env, ec)
    | ExnMatch i -> Failed
    | External -> Final v
    

let rec one_step_exp_cps_eval env exp_cps = 
match exp_cps with 
  |ConstCPS (k,c) ->
    app_cont_to_value env k (const_to_val c)
  | VarCPS(k, x) ->
     (match lookup_value env x with
        None -> Failed
      | Some v -> app_cont_to_value env k v)
  | MonOpAppCPS (k, monop, x, ke) ->
    (match lookup_value env x with
     |  None -> Failed
     | Some u ->
              (match monOpApply monop u with 
                Exn n ->Failed
               | Some v -> app_cont_to_value env k v))
  | BinOpAppCPS (k, binop, x, y, ke) ->
   (match (lookup_value env x, lookup_value env y) with 
   (Some w, Some u) ->
            (match binOpApply binop w u with 
             | Exn n -> Failed
             | Value v -> app_cont_to_value env k v)
    | _ -> Failed)
  | IfCPS (b, e1, e2) ->
    (match lookup_value env b with
       Some (BoolVal true) -> Intermediate(env, e1)
     | Some (BoolVal false) -> Intermediate(env, e2)
     | _ -> Failed)
  | FunCPS (k, x, i, j, ecps) ->
    app_cont_to_value env k
     (CPSClosureVal(x, i, j, ecps, env))
  | FixCPS (k, f, x, i, j, ecps) ->
    app_cont_to_value env k
     (CPSRecClosureVal(f, x, i, j, ecps, env))
  
