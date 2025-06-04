type term = Const of string | Var of string | Expr of expr

and expr = Pred of { name : term 
                 ; args : term list}
       | Bind of { binder : binder
                          ; var : term
                          ; expr : expr }
       | Op of op * expr list | Term of term
       | Null

and binder = Lambda | ForAll | Exists | Unique

and op = Not | And | Or | If

and statement = Decl of expr | Query of expr

let fmt_binder = function
  | Lambda -> "\\" | ForAll -> "@"
  | Exists -> "#"  | Unique -> "~"

let fmt_op (op : op) =
  match op with
  | Not -> "!" | And -> "&"
  | Or -> "|"  | If -> "->"

let rec fmt_terms terms = 
  let term_strs = List.map fmt_term terms in
  String.concat "," term_strs
and fmt_term = function
  | Var x -> x 
  | Const a -> a
  | Expr expr -> fmt_expr expr

and fmt_expr (expr : expr) = 
  match expr with
  | Term term -> fmt_term term
  | Pred{name; args} -> let s = 
    match name with Const s | Var s -> s | Expr _ -> failwith "Pred name should not be expr." in
    let terms_str = fmt_terms args in
    Printf.sprintf "%s(%s)" s terms_str
  | Bind{binder; var=Var x; expr} -> let b = fmt_binder binder in
    let s = fmt_expr expr in
    let str = match expr with
    | Bind _ | Pred _ -> Printf.sprintf "%s%s.%s" b x s
    | _ -> Printf.sprintf "%s%s[%s]" b x s in 
    str
  | Op(Not, [l]) -> (fmt_op Not) ^ (fmt_expr l)
  | Op(op, [l1; l2]) -> Printf.sprintf "%s %s %s" (fmt_expr l1) (fmt_op op) (fmt_expr l2)
  | Null -> ""
  | _ -> let err = Printf.sprintf "FORMAT ERROR: Cannot format lambda expressions of variant %s" (fmt_expr expr) in
    failwith err

(*
let print_expr expr =
  let expr_str = fmt_expr expr in
  print_endline expr_str
*)

let rec free_var' expr =
  match expr with
  | Bind{binder=Lambda; var; _} -> Some var
  | Bind bind -> free_var' bind.expr
  | Op(_, args) -> let args = List.map free_var' args in
    (match args with
     | [Some var; None] -> Some var
     | [None; None] -> None
     | [None; Some var] -> Some var
     | [] -> None
     | _ -> failwith "Incorrect amount of free vars in Op.")
  | _ -> None

let free_var expr =
  match expr with
  | Bind bind -> free_var' bind.expr
  | Op _ -> free_var' expr
  | _ -> None

let free_vars expr = 
  let rec free_vars' vars expr =
    match expr with
    | Bind {binder=Lambda; var; expr} -> let vars = vars@[var] in
      free_vars' vars expr
    | Bind {binder=_; var=_; expr} -> free_vars' vars expr
    | Op (_, args) -> 
      (match args with
      | [arg] -> free_vars' vars arg
      | [arg1; arg2] -> let vars1 = free_vars' [] arg1 in
        let vars2 = free_vars' [] arg2 in
        vars @ vars1 @ vars2
      | _ -> failwith "Incorrect amount of args in Op.")
    | _ -> vars
  in free_vars' [] expr

let rec add_e expr =
  match expr with
  | Bind bind -> Bind {bind with expr=add_e bind.expr}
  | Op (op, args) -> let args = List.map add_e args in Op (op, args)
  | Pred pred -> let args = pred.args@[Var "e"] in Pred {pred with args}
  | _ -> expr

let abstract_e expr =
  match expr with
  | Bind {binder=Lambda; var= Var "x"; expr} -> 
    let expr = Bind {binder=Lambda; var = Var "e"; expr} in
    Bind {binder=Lambda; var = Var "x"; expr}
  | _ -> failwith "Should be a bind."

let lambda_abstract var expr =
  Bind{binder=Lambda; var; expr}


let rec lift_bind expr = 
  match free_var expr with
  | Some var -> let expr = reduce_bind var expr in
    let binder = Lambda in
    Bind{binder; var; expr}
  | None -> expr

and reduce_bind var expr = 
  match expr with
  | Bind{binder=Lambda; var=var'; expr} ->
    if var = var' then expr 
    else let expr = reduce_bind var expr in 
      Bind{binder=Lambda; var=var'; expr}
  | Bind bind -> Bind{bind with expr=reduce_bind var bind.expr}
  | Op(op, args) -> let args = List.map (reduce_bind var) args in
    Op(op, args)
  | other -> other

let lift_binds expr = 
  let vars = free_vars expr in
  let rec lift_binds' expr vars =
  match vars with
  | [] -> expr
  | var::vars -> let expr = reduce_bind var expr in
    let expr = lift_binds' expr vars in
    lambda_abstract var expr
  in lift_binds' expr vars
(*
let rec lift_binds expr = 
  let var_opt = free_var expr in
  match var_opt with
  | Some _ -> let expr = lift_bind expr in
    lift_binds expr
  | None -> expr
*)
let subst_term v v' term =
  if v = term then v' else term

let rec subst_terms v w expr =
  match expr with
  | Bind{binder=Lambda; var; expr} ->
    if var = w then
      let expr = alpha_conv var (Var "v") expr in
      let expr = subst_terms v w expr in
      let expr = alpha_conv (Var "v") v expr in
      Bind{binder=Lambda; var=v; expr}
    else if var = v then
      Bind{binder=Lambda; var=w; expr=subst_terms v w expr}
    else
      Bind{binder=Lambda; var; expr=subst_terms v w expr}
  | Bind{binder; var; expr} -> 
    if var = v then Bind{binder; var; expr} 
    else if var = w then
      let expr = alpha_conv var (Var "v") expr in
      let expr = subst_terms v w expr in
      let expr = alpha_conv (Var "v") v expr in
      Bind{binder; var=v; expr}
    else 
      Bind{binder; var; expr=subst_terms v w expr}
  | Pred pred -> let args = List.map (subst_term v w) pred.args in
    Pred{pred with args}
  | Op(op, args) -> let args = List.map (subst_terms v w) args in
    Op(op, args)
  | other -> other

and subst_expr v w expr =
  match expr with
  | Bind bind -> let expr = subst_expr v w bind.expr in
    Bind{bind with expr}
  | Pred {name; args} -> 
    if v = name then
      match args with
      | [arg] -> apply w (Term arg)
      | [arg; arg'] -> apply (apply w (Term arg)) (Term arg')
      | _ -> failwith "Substitution error: Too many args in predicate."
    else
      expr
  | Op(op, args) -> let args = List.map (subst_expr v w) args in
    Op(op, args)
  | Term (Expr expr) -> Term (Expr (subst_expr v w expr))
  | Term var -> if v = var then w else Term var
  | other -> other

and alpha_conv v w expr = 
  match expr with
  | Bind{binder; var; expr} -> let var = subst_term v w var in
    Bind{binder; var; expr=alpha_conv v w expr}
  | Op(op, args) -> let args = List.map (alpha_conv v w) args in
    Op(op, args)
  | Pred pred -> let args = List.map (subst_term v w) pred.args in
    Pred{pred with args}
  | other -> other


and apply (e1 : expr) (e2 : expr) =
  match e1, e2 with
  | Bind{binder=Lambda; var=Var "P"; expr}, Bind {binder=Lambda; var=Var "x"; _}  
  | Bind{binder=Lambda; var=Var "P"; expr}, Bind {binder=Lambda; var=Var "y"; _}  
  | Bind{binder=Lambda; var=Var "P"; expr}, Bind {binder=Lambda; var=Var "z"; _}  
  | Bind{binder=Lambda; var=Var "P"; expr}, Bind {binder=Lambda; var=Var "e"; _}  
    -> let expr = subst_expr (Var "P") e2 expr in lift_binds expr
  | Bind{binder=Lambda; var=Var "Q"; expr}, Bind {binder=Lambda; var=Var "x"; _}  
  | Bind{binder=Lambda; var=Var "Q"; expr}, Bind {binder=Lambda; var=Var "y"; _}  
  | Bind{binder=Lambda; var=Var "Q"; expr}, Bind {binder=Lambda; var=Var "z"; _}  
    -> let expr = subst_expr (Var "Q") e2 expr in lift_binds expr
  | Bind {binder=Lambda; var=Var "x"; _}, Bind{binder=Lambda; var=Var "P"; expr} 
  | Bind {binder=Lambda; var=Var "y"; _}, Bind{binder=Lambda; var=Var "P"; expr} 
  | Bind {binder=Lambda; var=Var "z"; _}, Bind{binder=Lambda; var=Var "P"; expr} 
  | Bind{binder=Lambda; var=Var "e"; _}, Bind {binder=Lambda; var=Var "P"; expr}  
    -> let expr = subst_expr (Var "P") e1 expr in lift_binds expr
  | Bind {binder=Lambda; var=Var "x"; _}, Bind{binder=Lambda; var=Var "Q"; expr} 
  | Bind {binder=Lambda; var=Var "x1"; _}, Bind{binder=Lambda; var=Var "Q"; expr} 
  | Bind {binder=Lambda; var=Var "y"; _}, Bind{binder=Lambda; var=Var "Q"; expr} 
  | Bind {binder=Lambda; var=Var "z"; _}, Bind{binder=Lambda; var=Var "Q"; expr} 
  | Bind{binder=Lambda; var=Var "e"; _}, Bind {binder=Lambda; var=Var "Q"; expr}  
    -> let expr = subst_expr (Var "Q") e1 expr in lift_binds expr
  | Bind {binder=Lambda; var=Var "x"; _}, Bind {binder=Lambda; var=Var "x"; _} 
  | Bind {binder=Lambda; var=Var "x"; _}, Bind {binder=Lambda; var=Var "x1"; _} 
  | Bind {binder=Lambda; var=Var "x"; _}, Bind {binder=Lambda; var=Var "y"; _} 
    -> modifier e1 e2
  | Bind {binder=Lambda; var=Var "e"; _}, Bind {binder=Lambda; var=Var "e"; _} 
    -> event_modifier e1 e2
  | Bind{binder=Lambda; var; expr}, (Term term) -> subst_terms var term expr
  | (Term term), Bind{binder=Lambda; var; expr} -> subst_terms var term expr
  | Bind{binder=Lambda; var=Var "p"; expr}, Pred _ ->
    let expr = subst_expr (Var "p") e2 expr in lift_binds expr
  | Null, Null -> Null
  | Null, _ -> e2
  | _, Null -> e1
  | _, _ -> let str = Printf.sprintf "ApplicationError: Cannot apply %s to %s" (fmt_expr e1) (fmt_expr e2) in
    failwith str

and modifier lf1 lf2 =
  let lf = 
    Bind{binder=Lambda; var = Var "P"; expr=
    Bind{binder=Lambda; var = Var "Q"; expr =
      Op(And, [ Pred{name=Var"P"; args=[Var "x"]}
              ; Pred{name=Var"Q"; args=[Var "x"]}
              ])}} in
  let lf = apply lf1 lf in
  let lf = apply lf2 lf in
  Bind{binder=Lambda; var=Var "x"; expr=lf}
and event_modifier lf1 lf2 =
  let lf = 
    Bind{binder=Lambda; var = Var "P"; expr=
    Bind{binder=Lambda; var = Var "Q"; expr =
      Op(And, [ Pred{name=Var"P"; args=[Var "e"]}
              ; Pred{name=Var"Q"; args=[Var "e"]}
              ])}} in
  let lf = apply lf1 lf in
  let lf = apply lf2 lf in
  Bind{binder=Lambda; var=Var "e"; expr=lf}

let test = Bind{binder=Exists; 
                var=Var "y"; 
                expr=
             Bind{binder=Lambda; 
                  var=Var "x"; 
                  expr=
                Pred{name=Const "P"; 
                     args=[Var "x"; Var "y"]}}}

let unique = Bind{binder=Lambda; var = Var "P"; expr=
             Term (Expr (Bind{binder=Unique; var = Var "x"; expr=
             Pred{name=Var"P"; args=[Var "x"]}}))}
