open Logic
open Lambda

type decl = Model of m
          | Monadic of (e -> (m -> m))
          | Dyadic of (e -> (e -> (m -> m)))
          | Single of (m -> m)
          | Double of ((e -> (m -> m)) -> m)

let fmt_decl decl =
  match decl with
  | Model _ -> "m"
  | Monadic _ -> "monadic"
  | Dyadic _ -> "dyadic"
  | Single _ -> "single"
  | Double _ -> "double"

let add_entity m e = 
  if List.mem e m.entities then m
  else let entities = e::m.entities in
    {m with entities}

let rec add_entities m es =
  match es with
  | [] -> m
  | [e] -> add_entity m e
  | e::es -> let m = add_entity m e in
    add_entities m es

let add_pred m name arity =
  match arity with
  | 1 -> (match List.assoc_opt name m.unaries with
         | Some _ -> m
         | None -> let unaries = add_assoc name [] m.unaries in
           {m with unaries})
  | 2 -> (match List.assoc_opt name m.binaries with
         | Some _ -> m
         | None -> let binaries = add_assoc name [] m.binaries in
           {m with binaries})
  | _ -> failwith "Incorrect arity when adding pred to model."

let counter = ref 0

let new_entity () = counter := (!counter) + 1;
  let name = "x" ^ (string_of_int !counter) in
  Anon name

let decl_exists m =
  fun p -> let e = new_entity () in
  let m = add_entity m e in
  let m = p e m in
  m

let decl_forall m =
  fun p -> 
  let rec loop p m es =
    match es with
    | [] -> m
    | [e] -> p e m
    | e::es -> let m = p e m in 
      loop p m es in
  loop p m m.entities

let pred name args value =
  match args with
  | [Empty] -> let f = (fun x -> fun m -> 
    let i = assoc name m.unaries in
    let i = add_assoc x value i in
    let unaries = add_assoc name i m.unaries in
    {m with unaries}) in Monadic f
  | [Empty; Empty] ->
    let f = (fun y -> fun x -> fun m -> 
    let i = assoc name m.binaries in
    let i' = assoc x i in
    let i' = add_assoc y value i' in
    let i = add_assoc x i' i in
    let binaries = add_assoc name i m.binaries in
    {m with binaries}) in Dyadic f
  | [_; Undef] -> Model m
  | [Undef; _] -> Model m
  | [Empty; e] -> let f = (fun x -> fun m -> 
    let i = assoc name m.binaries in
    let i' = assoc x i in
    let m = add_entity m e in
    let i' = add_assoc e value i' in
    let i = add_assoc x i' i in
    let binaries = add_assoc name i m.binaries in
    {m with binaries}) in Monadic f
  | [e; Empty] -> let f = (fun x -> fun m -> 
    let i = assoc name m.binaries in
    let i' = assoc e i in
    let m = add_entity m e in
    let i' = add_assoc x value i' in
    let i = add_assoc e i' i in
    let binaries = add_assoc name i m.binaries in
    {m with binaries}) in Monadic f
  | [e1; e2] -> let f = (fun m -> let i = assoc name m.binaries in
    let m = add_entities m args in
    let i' = assoc e1 i in
    let i' = add_assoc e2 value i' in
    let i = add_assoc e1 i' i in
    let binaries = add_assoc name i m.binaries in
    {m with binaries}) in Single f
  | [e] -> let f = (fun m -> let i = assoc name m.unaries in
    let m = add_entity m e in
    let i = add_assoc e value i in
    let unaries = add_assoc name i m.unaries in
    {m with unaries}) in Single f
  | _ -> failwith "Incorrect args in pred decl."

let rec decl m (value : bool) expr : decl =
  match expr with
  | Bind{binder=Exists; var=_; expr} -> let declaration = decl m value expr in
    let quant = Double (decl_exists m) in
    app quant declaration
  | Bind{binder=ForAll; var=_; expr} -> let declaration = decl m value expr in
    let quant = Double (decl_forall m) in
    app quant declaration 
  | Pred{name=Const name; args} -> let m = add_pred m name (List.length args) in
    let eval_term' = eval_term m in
    let args = List.map eval_term' args in
    pred name args value 
  | Op(op, args) -> decl_op m value op args
      
  | _ -> failwith "DeclError."

and decl_op m value op args =
  match op, args with
  | Not, [expr] -> let value = not value in
    decl m value expr
  | And, args | Or, args -> 
    (match List.map (decl m value) args with
    | [Single f1; Single f2] -> let m = f1 m in
      Model (f2 m)
    | [Monadic f1; Monadic f2] -> 
      let f = fun x -> fun m ->
      let m = f1 x m in f2 x m in
      Monadic f
    | [other] -> other
    | _ -> failwith "Incorrect op args in decl.")
  | If, [expr1; expr2] -> cond m value expr1 expr2
  | _ -> failwith "Incorrect args in decl_op"

and cond m value expr1 expr2 =
  let p = eval m expr1 in
  let q = decl m value expr2 in
  match p, q with                          
  | Truth True, Monadic q -> Monadic q
  | Unary p, Monadic q -> let f = fun x -> fun m -> 
    if p x = True then q x m else m in Monadic f
  | _ -> failwith (Printf.sprintf "CondError. %s and %s" (fmt_tau p) (fmt_decl q))                                                                 

and app v1 v2 =
  match v1, v2 with                                                          
  | Double quant, Monadic p -> Model (quant p)
  | _ -> failwith "AppError."

