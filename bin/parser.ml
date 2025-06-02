open Lexer
open Lambda

type data = Trees of tree * tree
          | Text of { text : string
                    ; lemma : string
                    }
and tree = { data : data
           ; cat : cat
           ; sel : cat list
           ; arity : int ref 
           ; lf : Lambda.expr
           }

let rec print_tree ?(indent=0) tree =
  let spaces = String.make (2 * indent) ' ' in
  match tree.data with
  | Text { text; _ } ->
    (match text with
    | "" -> Printf.printf "%s[ %s : %s ]\n" spaces (fmt_cat tree.cat) (Lambda.fmt_expr tree.lf)
    | _ -> Printf.printf "%s[ %s : %s %s ]\n" spaces text (fmt_cat tree.cat) (Lambda.fmt_expr tree.lf))
  | Trees (left, right) -> Printf.printf "%s%s [ %s\n" spaces (fmt_cat tree.cat) (Lambda.fmt_expr tree.lf);
      print_tree ~indent:(indent + 1) left;
      print_tree ~indent:(indent + 1) right;
      print_endline (Printf.sprintf "%s]" spaces)

let rec fmt_tree tree =
  let {data; cat; sel; arity=_; lf=_} = tree in 
  match data with
  | Text {text; _} -> Printf.sprintf "(%s: %s %s)" text (fmt_cat cat) (fmt_sel sel) 
  | Trees(left, right) -> Printf.sprintf "(%s %s: %s%s)" (fmt_cat cat) (fmt_sel sel) 
    (fmt_tree left) (fmt_tree right)

and fmt_sel sel =
  match sel with
  | [] -> ""
  | [cat] -> Printf.sprintf "=%s" (fmt_cat cat)
  | sel -> let sel_strs = List.map fmt_cat sel in
    let str = String.concat "," sel_strs in
    Printf.sprintf "=[%s]" str

let selects t1 t2 =
  match t1.sel with
  | cat::_ -> cat = t2.cat
  | [] -> false


let adjoin_right t1 t2 =
  let data = Trees(t1, t2) in
  let cat = t1.cat in
  let sel = t1.sel in
  let arity = t1.arity in
  let lf = Lambda.Null in
  Some {data; cat; sel; arity; lf}

let adjoin_left t1 t2 =
  let data = Trees(t1, t2) in
  let cat = t2.cat in
  let sel = t2.sel in
  let arity = t2.arity in
  let lf = Lambda.Null in
  Some {data; cat; sel; arity; lf}

let event_lf = Lambda.Null 
  (*
  Bind { binder = Lambda
       ; var = Var "P"
       ; expr = Bind { binder = Exists
                     ; var = Var "e"
                     ; expr = Pred { name = Var "P"
                                   ; args = [Var "e"]
                                   }
                     }
       }*)


let rec set_lf (tree : tree) =
  match tree.cat, tree.data with

  | _, Trees(left, right) -> let left = set_lf left in
    let right = set_lf right in
    let data = Trees(left, right) in
    let lf = apply left.lf right.lf in
    let tree = {tree with data} in
    {tree with lf}

  | V, Text {text=_; lemma} -> 
    let arity = tree.arity in
    let lf = (match !arity with

      | 0 ->  Bind{binder = Lambda; var = Var "x"; expr=
              Pred{name=Const lemma; args=[Var "x"]}}

      | 1 ->  Bind{binder = Lambda; var = Var "y"; expr = 
              Bind{binder = Lambda; var = Var "x"; expr=
              Pred{name=Const lemma; args=[Var "x"; Var "y"]}}}

      | 2 ->  Bind{binder=Lambda; var=Var "z"; expr = 
              Bind{binder= Lambda; var = Var "y"; expr=
              Bind{ binder = Lambda; var = Var "x"; expr=
              Pred{ name=Const lemma; args=[Var "x"; Var "y"; Var "z"]}}}}

      | _ -> failwith "Arity of verb is > 3.") 
    in {tree with lf}

  | N, Text{text=_; lemma} -> 
    let lf = Bind{ binder=Lambda; var=Var "x"; expr=
             Pred{ name=Const lemma; args=[Var "x"]}} in
    {tree with lf}

  | D, Text{text=_; lemma} -> let lf =
    match lemma with
    | "a" -> make_bind Exists And
    | "an" -> make_bind Exists And
    | "the" -> make_bind Unique And
    | "every" -> make_bind ForAll If
    | "some" -> make_bind Exists And
    | _ -> let name = String.lowercase_ascii lemma in 
      Term (Const name)
    in
    {tree with lf}

  | J, Text{text=_; lemma} -> 
    let lf = Bind{ binder=Lambda
        ; var = Var "x"
        ; expr=Pred{ name=Const lemma
                   ; args=[Var "x"]}
        } in {tree with lf}

  | R, Text{text=_; lemma} ->
    let lf = Bind{ binder=Lambda
                 ; var = Var "e"
                 ; expr=Pred{name=Const lemma
                             ; args=[Var "e"]}
                 } in {tree with lf}

  | P, Text{text=_; lemma} ->
    let lf = Bind { binder = Lambda; var = Var "y"; expr=
             Bind { binder = Lambda; var = Var "x"; expr=
             Pred {name = Const lemma; args= [Var "x"; Var "y"]}}} in
    {tree with lf}
  | T, Text{text=_; lemma=_} -> {tree with lf=event_lf}
  | W, Text{text=_; lemma=_} -> {tree with lf=Term (Var "x1")}
  | _ -> tree

and make_bind binder op =
    Bind{binder=Lambda; var=Var "P"; expr=
    Bind{binder=Lambda; var = Var "Q"; expr =
    Bind{binder; var = Var "x"; expr =
      Op(op, [ Pred{name=Var "P"; args=[Var "x"]}
           ; Pred{name=Var "Q"; args=[Var "x"]}
           ]
        )}}}

let is_terminal t =
  match t.data with
  | Trees(_,_) -> false
  | Text _ -> true

let change_cat t cat cat' = 
    {t with cat=if t.cat=cat then cat' else t.cat}

let rec change_cats t cat cat' =
  match t.data with
  | Trees(t1,t2) -> 
    if t1.cat = cat && is_terminal t1 then
      let t1 = change_cat t1 cat cat' in
      let data = Trees(t1,t2) in
      change_cat {t with data} cat cat'
    else
      let t1 = change_cats t1 cat cat' in
      let t2 = change_cats t2 cat cat' in
      let data = Trees(t1,t2) in
      change_cat {t with data} cat cat'
  | Text _ -> change_cat t cat cat'

let rec set_v_lf t = 
  match t with
  | {data = Trees(t1, t2); _} -> let t1 = set_v_lf t1 in
    let t2 = set_v_lf t2 in
    let data = Trees(t1, t2) in
    {t with data}
  | {data = Text {text=_; lemma}; cat = V; _} -> if lemma = "be" then t else set_lf t
  | {data = Text _; _} -> t

let lex_to_tree lex =
  let {text; lemma; cat; sel} = lex in
  let data = Text {text; lemma} in
  let arity = ref 0 in
  let lf = Lambda.Null in
  match cat with
  | V -> {data; cat; sel; arity; lf}
  | _ -> set_lf {data; cat; sel; arity; lf}

let rec delete_lfs t = 
  match t.data with
  | Trees(t1,t2) -> let t1 = delete_lfs t1 in
    let t2 = delete_lfs t2 in
    let data = Trees(t1,t2) in
    let lf = Lambda.Null in
    {data; cat=t.cat; sel=t.sel; arity=t.arity; lf}
  | Text _ -> let lf = Lambda.Null in 
    {t with lf}

let make_trace d = 
  match d with
  | {data=Trees(_,_); cat = D; _} -> let lf = d.lf in 
    let data = Text{text=""; lemma=""} in
    {data; cat = D; sel=[]; arity=d.arity; lf},
    delete_lfs d
  | {data=Text _; cat = D; _} -> let data = Text{text=""; lemma=""} in
    {data; cat = D; sel=[]; arity=d.arity; lf=d.lf},
    delete_lfs d
  | _ -> failwith "Failed to make trace. Not a determiner."


let rec merge t1 t2 =
  let tl = function [] -> [] | _::t -> t in
  if selects t1 t2 then
    match t1.cat, t2.cat with
    | V, D -> 
      if t2.sel = [] then
        let c = if (t1.cat = V) && (t2.cat = D) then 1 else 0 in
        let arity = t1.arity in arity := !arity + c;
        let t = { data = Trees(t1, t2)
                ; cat = t1.cat
                ; sel = if t1.cat = V then t1.sel else tl t1.sel
                ; arity
                ; lf=Lambda.Null} in Some t
      else 
        None
    | _ -> Some { data=Trees(t1,t2)
                ; cat = t1.cat
                ; sel = tl t1.sel
                ; arity = t1.arity
                ; lf= Lambda.apply t1.lf t2.lf}
  else if selects t2 t1 then
    match t1.cat, t2.cat with
    | D, V -> let trace, t1 = make_trace t1 in
      let t2 = insert_trace' trace t2 in
      let t3 = { data = Text {text=""; lemma=""}
               ; cat = T
               ; sel = [V; D]
               ; arity=ref 0
               ; lf=event_lf} in
      let t2_opt = merge t3 t2 in
      (match t2_opt with
       | Some t2 -> merge t1 t2
       | None -> failwith "mergerror")
    | _ -> let t = { data = Trees(t1, t2)
            ; cat=t2.cat
            ; sel= tl t2.sel
            ; arity=t2.arity
            ; lf=Lambda.Null} in Some t
  else
    match t1.cat, t2.cat with

    | T, D -> (match t1.data with
      | Text{text=_; lemma} -> if lemma = "do" then
        let cat = V in
        let sel = [D] in
        let t1 = {t1 with cat; sel} in 
        merge t1 t2
        else None
      | Trees(_,_) -> None)

    | V, P -> 
        let sel = t1.sel in
        let arity = t1.arity in
        let lf = Lambda.Null in
        let cat = V in
        let expr = t2.lf in
        let var_opt = free_var' expr in
        (match var_opt with
        | Some var -> let expr = alpha_conv var (Var "e") expr in
          let t2 = {t2 with lf=expr} in
          let data = Trees(t1,t2) in
          Some {data; cat; sel; arity; lf}
        | None -> let data = Trees(t1,t2) in
          Some {data; cat; sel; arity; lf})

    | N, P | V, R -> adjoin_right t1 t2

    | R, V | R, T | R, B | R, H | J, N -> adjoin_left t1 t2

    | D, H
    | D, B -> let trace, t1 = make_trace t1 in
      let t2 = change_cats t2 t2.cat T in
      let t2 = {t2 with sel=[D]; lf=event_lf} in
      let t2 = insert_trace' trace t2 in
      merge t1 t2

    | D, T -> let trace, t1 = make_trace t1 in 
      let t2 = insert_trace' trace t2 in
      let t2 = {t2 with sel=[D]; lf=event_lf} in
      merge t1 t2

    | H, B
    | B, V -> let data = Trees(t1,t2) in
      let cat = t1.cat in let sel = [] in
      let arity = ref 0 in let lf = Lambda.Null in
      Some {data; cat; sel; arity; lf}

    | H, D -> let t1 = {t1 with cat=V; sel=[D]; lf=Lambda.Null} in merge t1 t2
    | B, D -> 
      (match t2.data with
      | Trees(t3,t4) -> let t1 = {t1 with cat=V; sel=[D]; lf=Lambda.Null} in 
        let t3 = {t3 with lf=Lambda.Null} in
        let t2 = {t2 with data=Trees(t3,t4)} in
        merge t1 t2
      | Text{text=_; lemma=_} -> 
        let t1 = {t1 with cat=V; sel=[D]; lf=Lambda.Null} in
        merge t1 t2)
    | B, J -> let t1 = {t1 with cat=V; sel=[J; D]; lf=Lambda.Null} in
      merge t1 t2

    | X, V -> let t1 = {t1 with cat=D} in 
      let arity = t2.arity in arity := !arity - 1;
      let t2 = {t2 with sel=[]; arity} in
      let t3 = { data = Text {text=""; lemma=""}
               ; cat = T
               ; sel = [V; D]
               ; arity=ref 0
               ; lf=event_lf} in
      let t_opt = merge t3 t2 in
      (match t_opt with
      | Some t2 -> let t = 
        {data = Trees(t1,t2)
        ; cat = t2.cat
        ; sel = []
        ; arity = t2.arity
        ; lf = Lambda.Null} in Some t
      | _ -> None)

    | W, V -> if t2.sel = [D] then
      let trace = {data=Text{text="";lemma=""}
                  ;cat = D
                  ;sel = []
                  ;arity = ref 0
                  ; lf = Term (Var "x1")} in
      let t2 = insert_trace' trace t2 in
      let t3 = { data = Text {text=""; lemma=""}
               ; cat = T
               ; sel = [V]
               ; arity=ref 0
               ; lf=event_lf} in
      let t2_opt = merge t3 t2 in
      (match t2_opt with
       | Some t2 -> 
         let t3 = {data= Text {text=""; lemma=""}
                  ;cat = C
                  ;sel =[T; W]
                  ;arity = ref 0
                  ;lf=Lambda.Null} in 
         (match merge t3 t2 with
         | Some t2 -> merge t1 t2
         | None -> None)
       | None -> failwith "mergerror")
    else None

    | W, C -> let trace = {data=Text{text=""; lemma=""}; cat=D;sel=[]; arity=ref 0; lf=Term (Var "x1")} in
      let t2 = insert_object trace t2 in
      Some {data=Trees(t1,t2); cat = C; sel= []; arity = ref 0; lf= Lambda.Null}

    | B, T
    | H, T
    | T, T -> let t1 = {t1 with cat=C; sel=[T]; lf=Lambda.Null} in
      merge t1 t2

    (*
    | C, S -> adjoin_left t1 t2
    | T, S -> let t = {data=Text{text=""; lemma=""}
                      ;cat = C
                      ;sel=[T]
                      ;arity=ref 0
                      ;lf=Lambda.Null} in
      let t1 = merge t t1 in
      (match t1 with Some t1 -> merge t1 t2 | None -> None)
*)
    | _ -> None

(*
and insert_trace d v =
  match v with
  | {data = Trees _; cat = V; _} -> 
    { data=Trees(d, v)
    ; cat = V
    ; sel = []
    ; arity = v.arity
    ; lf = Lambda.Null}
  | {data = Trees(t1, t2); _} -> let t1 = insert_trace d t1 in
    let t2 = insert_trace d t2 in
    {v with data = Trees(t1, t2)}
  | {data = Text _; cat = V; _} -> 
    { data=Trees(d, v)
    ; cat = V
    ; sel = []
    ; arity = v.arity
    ; lf = Lambda.Null}
  | {data = Text _; _} -> v
*)

and insert_object d t =
  match t.data with
  | Text _ -> if t.cat = V then
    let data = Trees(t,d) in
    let cat = V in
    let sel = [] in
    let arity = t.arity in
    arity := !arity + 1; 
    let lf = Lambda.Null in
    {data; cat; sel; arity; lf}
    else t
  | Trees(t1,t2) -> 
    match t1.cat, t2.cat with
    | D, V -> let t2 = {data=Trees(t2,d); cat=V;sel=[]; arity=t2.arity; lf = Lambda.Null} in
      let data = Trees(t1,t2) in
      let cat = V in
      let sel = [] in
      let arity=t2.arity in
      arity := !arity +1;
      let lf = Lambda.Null in
      {data; cat; sel; arity; lf}
    | _ -> let t1 = insert_object d t1 in
      let t2 = insert_object d t2 in
      let data = Trees(t1,t2) in
      let cat = t.cat in 
      let sel = [] in
      let arity = t2.arity in
      let lf = Lambda.Null in
      {data; cat; sel; arity; lf}

and insert_trace' d v =
  match v with
  | {data = Text _; cat = V; _} ->
    {data = Trees(d, v)
    ; cat = V
    ; sel = []
    ; arity = v.arity
    ; lf = Lambda.Null}
  | {data = Text _; _} -> v
  | {data = Trees (t1,t2); _} ->
    match t1, t2 with
    | {data = Text _; cat = V; _},
      {data = Trees _; cat = P; _} ->
      let t1 = {data = Trees(d,t1)
               ;cat = V
               ;sel = []
               ;arity=t1.arity
               ;lf = Lambda.Null} in
      {v with data=Trees(t1,t2)}
    | {data = Text _; cat = V; _},
      {data = Text _; cat = J; _}
    | {data = Text _; cat = V; _},
      {data = Trees _; cat = D; _}
    | {data = Text _; cat = V; _},
      {data = Text _; cat = D; _}
    | {data = Trees _; cat = V; _},
      {data = Text _; cat = D; _}
    | {data = Trees _; cat = V; _},
      {data = Trees _; cat = D; _} ->
      { data = Trees(d,v)
      ; cat = V
      ; sel = []
      ; arity = v.arity
      ; lf = Lambda.Null }
    | _ -> let t1 = insert_trace' d t1 in
      let t2 = insert_trace' d t2 in
      {v with data=Trees(t1,t2)}

let rec evaluate t =
  match t.data with
  | Trees(t1, t2) -> 
    let t1 = evaluate t1 in
    let t2 = evaluate t2 in
    (match t1.cat, t2.cat with
    (*
    | V, P -> let var_opt = free_var' t2.lf in
      (match var_opt with
      | Some var -> let lf = subst_terms var (Var "e") t2.lf in
        let t2 = {t2 with lf} in
        let lf = Lambda.apply t1.lf lf in
        {data=Trees(t1,t2); cat=t.cat; sel=t.sel; arity=t.arity; lf}
      | None -> let lf = Lambda.apply t1.lf t2.lf in
        {data=Trees(t1,t2); cat=t.cat; sel=t.sel; arity=t.arity; lf})*)
    | W, _ -> (match t1.lf with
               | Term (Var x) -> let var = Var x in 
                 let lf = Bind{binder=Lambda; var=var; expr=t2.lf} in
                 {data=Trees(t1,t2); cat=t.cat; sel=t.sel; arity=t.arity; lf}
               | _ -> failwith "Wh should be var.")
    | V, J -> (match t1.data with
              | Text{text=_; lemma="be"} ->
                (*let lf = Lambda.add_e t2.lf in
                let lf = Lambda.abstract_e lf in*)
                {data=Trees(t1,t2); cat=t.cat; sel=t.sel; arity=t.arity; lf=t2.lf}
              | _ -> let lf = Lambda.apply t1.lf t2.lf in
                let data = Trees(t1,t2) in 
                {t with data; lf})
    | _ -> let lf = Lambda.apply t1.lf t2.lf in
      {data=Trees(t1,t2); cat=t.cat; sel=t.sel; arity=t.arity; lf})
  | Text _ -> t

let rec parse_trees ts = 
  match ts with
  | []    -> failwith "Cannot parse empty tree."
  | [t]   -> t, []
  | t::ts -> let t', ts' = parse_trees ts in
    match merge t t' with
    | Some t -> parse' t ts'
    | None -> t, t'::ts'
and parse' t ts =
  match ts with
  | [] -> t, []
  | [t'] -> (match merge t t' with 
            | Some t'' -> t'', []
            | None -> t, [t'])
  | t'::ts' -> (match merge t t' with
                | Some t'' -> parse' t'' ts'
                | None -> t, ts) 
let parse lexes =
  let ts = List.map lex_to_tree lexes in
  let tree, stack = parse_trees ts in
  let tree = set_v_lf tree in
  let tree = evaluate tree in
  match stack with
  | [] -> tree, stack
  | _ -> tree, stack (* failwith "Syntax error." *)
