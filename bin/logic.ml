open Lambda

type e = Named of string | Anon of string | Undef
type t = True | False | Unk
type tau = Entity of e | Entities of e list | Truth of t 

type model = { entities : e list
             ; unaries : (string * (e * bool) list) list
             ; binaries : (string * (e * (e * bool) list) list) list}

type execution = Model of model | Tau of tau

type env = (term * e) list

let m = { entities =[]
        ; unaries  =[]
        ; binaries =[]
        }

let add_env var e env =
  (var, e)::env

let bool_to_truth b =
  match b with true -> True | false -> False

let fmt_entity e = match e with Named s -> s | Anon s -> s | Undef -> "?"
let rec fmt_entities es =
  let str = fmt_entities' es in
  Printf.sprintf "<%s>" str

and fmt_entities' es = 
  match es with
  | [] -> ""
  | [e] -> fmt_entity e
  | e::es -> let e_str = fmt_entity e in
    Printf.sprintf "%s, %s" e_str (fmt_entities' es)

let fmt_pair (p : e * bool) =
  let (entity, value) = p in
  let entity_str = fmt_entity entity in
  let value_str = string_of_bool value in
  Printf.sprintf "<%s, %s>" entity_str value_str

let rec fmt_pairs pairs =
  match pairs with
  | [] -> ""
  | [pair] -> fmt_pair pair
  | pair::pairs -> let pair_str = fmt_pair pair in
    Printf.sprintf "%s,%s" pair_str (fmt_pairs pairs)

let fmt_triple (t : e * (e * bool) list) =
  let entity, pairs = t in
  let e_str = fmt_entity entity in
  let pairs_str = fmt_pairs pairs in
  Printf.sprintf "<%s, %s>" e_str pairs_str

let rec fmt_triples ts =
  match ts with
  | [] -> ""
  | [t] -> fmt_triple t
  | t::ts -> let t_str = fmt_triple t in
    Printf.sprintf "%s,%s" t_str (fmt_triples ts)

let fmt_unary (unary : string * ('a list)) =
  let name, pairs = unary in
  let args_str = fmt_pairs pairs in
  Printf.sprintf "%s -> %s" name args_str

let fmt_binary (binary : string * ('a list)) =
  let name, triples = binary in
  let args_str = fmt_triples triples in
  Printf.sprintf "%s -> %s" name args_str

let rec fmt_unaries unaries =
  match unaries with
  | [] -> ""
  | [unary] -> fmt_unary unary
  | unary::unaries -> let unary_str = fmt_unary unary in
    Printf.sprintf "%s\n%s" unary_str (fmt_unaries unaries)

let rec fmt_binaries binaries =
  match binaries with
  | [] -> ""
  | [binary] -> fmt_binary binary
  | binary::binaries -> let binary_str = fmt_binary binary in
    Printf.sprintf "%s\n%s" binary_str (fmt_binaries binaries)

let fmt_model (m : model) =
  let entities_str = fmt_entities m.entities in
  let unaries_str = fmt_unaries m.unaries in
  let binaries_str = fmt_binaries m.binaries in
  Printf.sprintf "%s\n%s\n%s" entities_str unaries_str binaries_str

let conj = fun x -> fun y ->
  match x, y with
  | True, True -> True | True, False -> False
  | False, True -> False | False, False -> False
  | _ -> Unk

let disj = fun x -> fun y ->
  match x, y with
  | True, True -> True | True, False -> True
  | False, True -> True | False, False -> False
  | _ -> Unk

let impl = fun x -> fun y -> 
  match x, y with
  | True, True -> True | False, True -> True
  | True, False -> False | False, False -> True
  | _ -> Unk

let add_pred m (name : string) (args : e list) (value : bool) =
  match args with
  | [arg] -> let es = List.assoc_opt name m.unaries in
    (match es with
    | Some es -> let es = (arg, value)::es in
      let unary = (name, es) in
      let unaries = List.remove_assoc name m.unaries in
      let unaries = unary::unaries in
      {m with unaries}
    | None -> let unary = (name, [(arg, value)]) in
      let unaries = unary::m.unaries in
      {m with unaries})
  | [arg1; arg2] -> let es = List.assoc_opt name m.binaries in
    (match es with
    | Some es ->
      (match List.assoc_opt arg1 es with
      | Some es' -> let es' = (arg2,value)::es' in
        let es = List.remove_assoc arg1 es in
        let es = (arg1,es')::es in
        let binary = (name, es) in                             
        let binaries = List.remove_assoc name m.binaries in
        let binaries = binary::binaries in
        {m with binaries}
      | None -> let es = (arg1, [(arg2, value)])::es in
        let binary = (name, es) in
        let binaries = List.remove_assoc name m.binaries in
        let binaries = binary::binaries in
        {m with binaries})
    | None -> let binary = (name, [(arg1, [(arg2, value)])]) in
      let binaries = binary::m.binaries in
      {m with binaries})
  | _ -> failwith "too many or too little args"

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

let counter = ref 0

let make_entity term =
  match term with
  | Const name -> Named name
  | Var _ -> counter := (!counter) + 1;
    let name = "x" ^ (string_of_int !counter) in
    Anon name
  | Expr _ -> failwith "definite description not implemented"

let query_pred name args m =
  match args with
  | [arg] -> let pairs = List.assoc_opt name m.unaries in
    (match pairs with
     | Some pairs -> 
       (match List.assoc_opt arg pairs with
       | Some value -> Truth (bool_to_truth value)
       | None -> Truth Unk) 
     | None -> Truth Unk)
  | [arg1; arg2] -> let triples = List.assoc_opt name m.binaries in
    (match triples with 
     | Some triples -> 
       (match List.assoc_opt arg1 triples with
        | Some pairs -> 
          (match List.assoc_opt arg2 pairs with
           | Some value -> Truth (bool_to_truth value)
           | None -> Truth Unk)
        | None -> Truth Unk)
     | None -> Truth Unk)
  | _ -> failwith "Incorrect ARGS"

let rec decl m value env expr =
  match expr with
  | Bind{binder=Exists; var; expr} -> let e = make_entity var in
    let m = add_entity m e in
    let env = add_env var e env in
    decl m value env expr
  | Bind{binder=ForAll; var; expr} -> assign m.entities var m value env expr
  | Bind{binder=Unique; var; expr} -> let es = assign' m.entities var m env expr in
    (match es with
    | [] -> let e = make_entity var in
      let m = add_entity m e in
      let env = add_env var e env in
      decl m value env expr
    | [_] -> m
    | _::_ -> failwith "plural definite description not implemented.")
  | Pred{name; args} -> let m = decl_args m value env args in
    let get_env' = get_env m env in
    let args = List.map get_env' args in
    if List.mem Undef args then m
    else let m = add_entities m args in
      add_pred m (fmt_term name) args value
  | Op (op, args) -> 
    (match op, args with
     | Not, [arg] -> let value = false in
       decl m value env arg
     | Or, [arg1; arg2]
     | And, [arg1; arg2] -> let m = decl m value env arg1 in
       decl m value env arg2
     | If, [Pred{name=Const name; args}; arg2] -> 
       let get_env' = get_env m env in
       let args = List.map get_env' args in
       if (query_pred name args m) = Truth True then
         decl m value env arg2
       else m
     | _ -> failwith "Incorrect amount of args")
  | _ -> failwith "" 

and decl_arg m value env arg =
  match arg with
  | Expr expr -> decl m value env expr
  | _ -> m
and decl_args m value env args =
  match args with
  | arg::args -> let m = decl_arg m value env arg in
    decl_args m value env args
  | [] -> m

and assign entities var m value env expr =
  match entities with
  | [] -> m
  | e::entities -> let env = add_env var e env in
    let m = decl m value env expr in
    assign entities var m value env expr

and get_env m env term =
  match term with
  | Const c -> Named c
  | Var _ -> List.assoc term env
  | Expr expr ->  
    match expr with
    | Bind{binder=Unique; var; expr} -> let es = assign' m.entities var m env expr in
      (match es with
       | [e] -> e
       | [] -> Undef
       | _::_ -> Undef)
    | _ -> failwith "Arg must be iota statement."


and query m (env : env) expr =
  match expr with
  | Bind{binder=Lambda; var; expr} -> Entities (assign' m.entities var m env expr)
  | Bind{binder=ForAll; var; expr} -> Truth (assign_forall m.entities var m env expr)
  | Bind{binder=Exists; var; expr} -> Truth (assign_exists m.entities var m env expr)
  | Op (op, args) -> query_op op args m env
  | Pred{name=Const name; args} -> let get_env' = get_env m env in
    let args = List.map get_env' args in
    query_pred name args m
  | _ -> failwith ""
and query_op op args m env =
  let query' = query m env in
  let args = List.map query' args in
  match op, args with
  | And, [Truth arg1; Truth arg2] -> let value = conj arg1 arg2 in
    Truth value
  | Or, [Truth arg1; Truth arg2] -> let value = disj arg1 arg2 in
    Truth value
  | If, [Truth arg1; Truth arg2] -> let value = impl arg1 arg2 in
    Truth value
  | _ -> failwith "Incorrect arguments."

and assign' entities var m env expr =
  match entities with
  | [] -> []
  | [e] -> let env = add_env var e env in
    let value = query m env expr in
    if value = Truth True then [e] else []
  | e::entities -> let env = add_env var e env in
    let value = query m env expr in
    if value = Truth True then
      e::(assign' entities var m env expr)
    else
      assign' entities var m env expr

and assign_forall entities var m env expr =
  match entities with
  | [] -> Unk
  | [e] -> let env = add_env var e env in
    let value = query m env expr in
    (match value with
    | Truth value -> value
    | _ -> failwith "should be a truth value.")
  | e::es -> let env = add_env var e env in
    let value' = query m env expr in
    (match value' with
     | Truth value' -> conj value' (assign_forall es var m env expr)
     | _ -> failwith "should be truth value")

and assign_exists entities var m env expr =
  match entities with
  | [] -> Unk
  | [e] -> let env = add_env var e env in
    let value = query m env expr in
    (match value with
    | Truth value -> value
    | _ -> failwith "should be a truth value.")
  | e::es -> let env = add_env var e env in
    let value' = query m env expr in
    (match value' with
     | Truth value' -> disj value' (assign_exists es var m env expr)
     | _ -> failwith "should be truth value")

let eval m statement =
  match statement with
  | Decl expr -> let value = true in
    let env = [] in
    Model (decl m value env expr)
  | Query expr -> let env = [] in
    Tau (query m env expr)
