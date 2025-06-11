open Lambda

type e = Named of string | Anon of string | Undef | Empty
and t = True | False | Unk
and tau = Entity of e 
        | Entities of e list
        | Truth of t 
        | Unary of (e -> t)
        | Binary of (e -> (e -> t))
        | Trinary of (e -> (e -> (e -> t)))
        | Quant of ((e -> t) -> t)
        | Iota of ((e -> t) -> e)
        | UnOp of (t -> t)
        | BinOp of (t -> (t -> t))

type m = { entities : e list
             ; unaries : (string * (e * bool) list) list
             ; binaries : (string * (e * (e * bool) list) list) list
             ; theory : expr list}

type execution = Model of m | Tau of tau

let m = { entities =[]
        ; unaries  =[]
        ; binaries =[]
        ; theory = []
        }

let fmt_truth t =
  match t with
  | True -> "Yes"
  | False -> "No"
  | Unk -> "Unknown"

let bool_to_truth b =
  match b with true -> True | false -> False

let t_to_bool t = 
  match t with 
  | True -> true
  | Unk | False -> false

let fmt_entity e = match e with Named s -> String.capitalize_ascii s | Anon s -> s | Undef -> "?" | Empty -> ""

let rec fmt_entities es =
  let str = fmt_entities' es in
  Printf.sprintf "%s" str
and fmt_entities' es = 
  match es with
  | [] -> ""
  | [e] -> fmt_entity e
  | [e; e'] -> Printf.sprintf "%s and %s" (fmt_entity e) (fmt_entity e')
  | e::es -> let e_str = fmt_entity e in
    Printf.sprintf "%s, %s" e_str (fmt_entities' es)

let fmt_tau tau =
  match tau with
  | Entity e -> fmt_entity e
  | Entities es -> fmt_entities es
  | Truth t -> fmt_truth t
  | Unary _ -> "e -> t"
  | Binary _ -> "e -> (e -> t)"
  | Trinary _ -> "(e -> (e -> (e -> t)))"
  | Quant _ -> "((e -> t) -> t)"
  | Iota _ -> "((e -> t) -> e)"
  | UnOp _ -> "(t -> t)"
  | BinOp _ -> "(t -> (t -> t))"

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
  Printf.sprintf "%s : %s" name args_str

let fmt_binary (binary : string * ('a list)) =
  let name, triples = binary in
  let args_str = fmt_triples triples in
  Printf.sprintf "%s : %s" name args_str

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

let fmt_theory theory =
  let theory_str = List.map Lambda.fmt_expr theory in
  let str = String.concat ", " theory_str in
  Printf.sprintf "{%s}" str

let fmt_model (m : m) =
  let entities_str = Printf.sprintf "<%s>" (fmt_entities m.entities) in
  let unaries_str = fmt_unaries m.unaries in
  let binaries_str = fmt_binaries m.binaries in
  let theory_str = fmt_theory m.theory in
  Printf.sprintf "%s\n%s\n%s\n%s" entities_str unaries_str binaries_str theory_str

let assoc (k : 'a) (lst : ('a * 'b list) list) : 'b list =
  match List.assoc_opt k lst with
  | Some value -> value
  | None -> []

let add_assoc (k : 'a) (value : 'b) (l : ('a * 'b) list) : ('a * 'b) list = 
  let l = List.remove_assoc k l in (k, value)::l

let rec conj = fun x -> fun y ->
  match x, y with
  | True, True -> True | True, False -> False
  | False, True -> False | False, False -> False
  | _ -> Unk

and disj = fun x -> fun y ->
  match x, y with
  | True, True -> True | True, False -> True
  | False, True -> True | False, False -> False
  | _ -> Unk

and impl = fun x -> fun y -> 
  match x, y with
  | True, True -> True | False, True -> True
  | True, False -> False | False, False -> True
  | _ -> Unk

and neg = fun x ->
  match x with
  | True -> False
  | False -> True
  | Unk -> Unk

and exists m =
  let f p x = t_to_bool (p x) in
  fun p -> bool_to_truth (List.exists (f p) m.entities)
    
and forall m =
  let f p x = t_to_bool (p x) in
  fun p -> bool_to_truth (List.for_all (f p) m.entities)

and unique m =
  let f p x = t_to_bool (p x) in
  fun p -> 
  match List.filter (f p) m.entities with
  | [] -> Undef
  | [e] -> e
  | _ -> Undef

and pred m name args = 
  match args with
  | [Empty] -> let i = assoc name m.unaries in 
    let f = fun x -> 
    (match List.assoc_opt x i with
     | Some t -> bool_to_truth t
     | None -> Unk) in Unary f
  | [Empty; Empty] -> let i = assoc name m.binaries in
    let f = fun y -> fun x -> 
    let i' = assoc x i in
    (match List.assoc_opt y i' with
     | Some t -> bool_to_truth t
     | None -> Unk) in Binary f
  | [e] -> let i = assoc name m.unaries in 
    (match List.assoc_opt e i with
     | Some t -> Truth (bool_to_truth t)
     | None -> Truth Unk)
  | [e; Empty] -> let i = assoc name m.binaries in
    let f = fun x -> fun y -> 
    let i' = assoc x i in
    (match List.assoc_opt y i' with
     | Some t -> bool_to_truth t
     | None -> Unk) in Unary (f e)
  | [Empty; e] -> let i = assoc name m.binaries in
    let f = fun y -> fun x -> 
    let i' = assoc x i in
    (match List.assoc_opt y i' with
     | Some t -> bool_to_truth t
     | None -> Unk) in Unary (f e)
  | [e1; e2] -> let i = assoc name m.binaries in
    let f = fun y -> fun x -> 
    let i' = assoc x i in
    (match List.assoc_opt y i' with
     | Some t -> bool_to_truth t
     | None -> Unk) in Truth (f e1 e2)
  | _ -> failwith "Incorrect arity in predicate."

and binop_to_func op =
  match op with
  | And -> conj
  | Or -> disj
  | If -> impl
  | _ -> failwith "BinOp only."

and eval m expr =
  match expr with
  | Bind{binder=ForAll; var=_; expr} -> 
    let quant = Quant (forall m) in
    let value = eval m expr in
    app quant value 
  | Bind{binder=Exists; var=_; expr} ->
    let quant = Quant (exists m) in
    let value = eval m expr in
    app quant value
  | Bind{binder=Unique; var=_; expr} ->
    let iota = Iota (unique m) in
    let value = eval m expr in
    app iota value
  | Bind{binder=Lambda; var=_; expr} ->
    eval m expr
  | Pred{name=Const name; args} -> let args = List.map (eval_term m) args in pred m name args
  | Op(op, args) -> let args = List.map (eval m) args in
    let f = binop_to_func op in
    (match args with 
    | [Truth t1; Truth t2] -> Truth (f t1 t2)
    | [Unary p1; Unary p2] -> Unary (fun x -> f (p1 x)(p2 x))
    | [Truth t] -> if op = Not then Truth (neg t) 
                   else failwith "Not is the only UnOp."
    | _ -> failwith "Incorrect args for op.")
  | _ -> failwith ""

and eval_term m term =
  match term with
  | Const c -> Named c
  | Var _ -> Empty
  | Expr expr -> 
    match eval m expr with
    | Entity e -> e
    | _ -> failwith "Expression as argument of predicate must evaluate to a truth value."

and app arg1 arg2 =
  match arg1, arg2 with
  | Quant q, Unary p -> Truth (q p)
  | Iota q, Unary p -> Entity (q p)
  | Entity e, Unary p -> Truth (p e)
  | Entity e, Binary p -> Unary (p e)
  | _ -> failwith "Function application error."
