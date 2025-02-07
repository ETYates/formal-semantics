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
      Printf.printf "%sText { text = \"%s\"; lf = \"%s\" }\n" spaces text (fmt_expr tree.lf)
  | Trees (left, right) ->
      Printf.printf "%sTrees lf = %s\n" spaces (fmt_expr tree.lf);
      print_tree ~indent:(indent + 1) left;
      print_tree ~indent:(indent + 1) right

let rec fmt_tree tree =
  let {data; cat; sel; arity=_; lf} = tree in 
  let l_str = fmt_expr lf in
  match data with
  | Text {text; _} -> Printf.sprintf "(%s: %s(%s)%s)" text (fmt_cat cat) l_str (fmt_sel sel) 
  | Trees(left, right) -> Printf.sprintf "(%s(%s)%s: %s%s)" (fmt_cat cat) l_str (fmt_sel sel) 
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

let merge t1 t2 =
  (* print_endline ((fmt_tree t1) ^ (fmt_tree t2)); *)
  let tl = function [] -> [] | _::t -> t in
  if selects t1 t2 then
    let cat = t1.cat in
    let sel = if cat = V then t1.sel else tl t1.sel in
    let c = if (t1.cat = V) && (t2.cat = D) then 1 else 0 in
    let arity = t1.arity in arity := !arity + c;
    let lf = Lambda.Null in
    let t = { data = Trees(t1, t2); cat; sel; arity; lf} in
    Some t
  else if selects t2 t1 then
    let cat = t2.cat in
    let sel = tl t2.sel in
    let c = if (t1.cat = D) && (t2.cat = V) then 1 else 0 in
    let arity = t2.arity in arity := !arity + c;
    let lf = Lambda.Null in
    let t = { data = Trees(t1, t2); cat; sel; arity; lf} in
    Some t
  else None

let lex_to_tree lex =
  let {text; lemma; cat; sel} = lex in
  let data = Text {text; lemma} in
  let arity = ref 0 in
  let lf = Lambda.Null in
  {data; cat; sel; arity; lf}

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
      | 1 ->  Bind{binder=Lambda; var=Var "x"; expr=Pred{name=Const lemma; args=[Var "x"]}}
      | 2 ->  Bind{ binder=Lambda; 
                    var=Var "y"; 
                    expr = Bind { binder= Lambda;
                               var = Var "x";
                               expr=Pred{ name=Const lemma; 
                                       args=[Var "x"; Var "y"]
                                     }
                             }
                  }
      | 3 ->  Bind{ binder=Lambda; 
                    var=Var "z"; 
                    expr = Bind { binder= Lambda;
                               var = Var "y";
                               expr=Bind{ binder = Lambda; 
                                       var = Var "x";
                                       expr=Pred{ name=Const lemma; 
                                               args=[Var "x"; Var "y"; Var "z"]
                                             }
                                     }
                             }
                  }
      | _ -> failwith "Arity of verb is > 3.") 
    in {tree with lf}
  | N, Text{text=_; lemma} -> let lf = Bind{ binder=Lambda; 
                                             var=Var "x"; 
                                             expr=Pred{ name=Const lemma; 
                                                     args=[Var "x"]
                                                   }
                                           } in
    {tree with lf}
  | D, Text{text=_; lemma} -> let lf =
    match lemma with
    | "a" -> make_bind Exists And
    | "the" -> make_bind Unique And
    | "every" -> make_bind ForAll If
    | "some" -> make_bind Exists And
    | _ -> Term (Const lemma)
    in
    {tree with lf}
  | _ -> tree

and make_bind binder op =
    Bind{binder=Lambda; var=Var "P"; expr=
    Bind{binder=Lambda; var = Var "Q"; expr =
    Bind{binder; var = Var "x"; expr =
      Op(op, [ Pred{name=Var "P"; args=[Var "x"]}
           ; Pred{name=Var "Q"; args=[Var "x"]}
           ]
        )}}}

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
  match stack with
  | [] -> let tree = set_lf tree in
    tree, stack
  | _ -> let tree = set_lf tree
    in tree, stack (* failwith "Syntax error." *)
