type cat = N | V | D | B | J | R | M | P | X | Conj
and lex = { text : string
          ; lemma : string
          ; cat : cat
          ; sel : cat list
          }
(*
and num = Singular
        | Plural
and cat = Common
        | Proper
and person = First
           | Third
           | Plain
           | Gerund
and tense = Pres
          | Past

*)
let fmt_cat cat =
  match cat with
  | N -> "n" | V -> "v" | D -> "d"
  | B -> "b" | J -> "j" | R -> "r"
  | M -> "m" | P -> "p" | X -> "x"
  | Conj -> "conj"

let fmt_lex (lex : lex) = 
  let {text; cat; _} = lex in
  let cat_str = fmt_cat cat in
  Printf.sprintf "%s: %s" text cat_str

let rec lexify tokens = 
  match tokens with
  | [] -> []
  | (text, lemma, tag)::tokens ->
    match tag with
    | "JJ"   -> {text; lemma; cat = N; sel = [N]}::lexify tokens
    | "NN"   -> {text; lemma;  cat = N; sel = []}::lexify tokens
    | "NNS"  -> {text; lemma; cat = N; sel = []}::lexify tokens
    | "NNP"  -> {text; lemma; cat = D; sel = []}::lexify tokens
    | "PRP"  -> {text; lemma; cat = D; sel = []}::lexify tokens
    | "NNPS" -> {text; lemma; cat = D; sel = []}::lexify tokens
    | "DT"   -> {text; lemma; cat = D; sel = [N]}::lexify tokens
    | "VBZ"  -> {text; lemma; cat = V; sel = [D]}::lexify tokens
    | "VBD"  -> {text; lemma; cat = V; sel = [D]}::lexify tokens
    | "VBP"  -> {text; lemma; cat = V; sel = [D]}::lexify tokens
    | "."    -> {text; lemma; cat = X; sel = []}::lexify tokens
    | other  -> let message = Printf.sprintf "Invalid POS tag: %s" other in
      failwith message

