type cat = N | V | D | J | R | M | P | S | T | B | H | X | W | C | Conj
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
  | J -> "j" | R -> "r" | H -> "h"
  | M -> "m" | P -> "p" | S -> "s"
  | T -> "t" | B -> "b" | Conj -> "conj"
  | X -> "x" | W -> "w" | C -> "c" 

let fmt_lex (lex : lex) = 
  let {text; cat; _} = lex in
  let cat_str = fmt_cat cat in
  Printf.sprintf "%s: %s" text cat_str

let rec lexify tokens = 
  match tokens with
  | [] -> []
  | (text, lemma, tag)::tokens ->
    match tag with
    | "JJ" | "JJR" | "JJS" -> {text; lemma; cat = J; sel = []}::lexify tokens
    | "NN"   -> {text; lemma; cat = N; sel = []}::lexify tokens
    | "NNS"  -> {text; lemma; cat = N; sel = []}::lexify tokens
    | "NNP"  -> {text; lemma; cat = D; sel = []}::lexify tokens
    | "PRP"  -> {text; lemma; cat = D; sel = []}::lexify tokens
    | "NNPS" -> {text; lemma; cat = D; sel = []}::lexify tokens
    | "DT"   -> {text; lemma; cat = D; sel = [N]}::lexify tokens
    | "VBZ" | "VBD" | "VBP" | "VBN" | "VBG" | "VB" -> 
      (match lemma with
       | "do" -> let cat = T in
         let sel = [V] in
         {text; lemma; cat; sel}::lexify tokens
       | "have" -> let cat = H in
         let sel = [V] in
         {text; lemma; cat; sel}::lexify tokens
       | "be" -> let cat = B in
         let sel = [V] in
         {text; lemma; cat; sel}::lexify tokens
       | _ -> let sel = [D] in
         let cat = V in
         {text; lemma; cat; sel}::lexify tokens)
    | "."    -> {text; lemma; cat = S; sel = []}::lexify tokens
    | "RB" | "RBR" | "RBS" -> {text; lemma; cat = R; sel = []}::lexify tokens
    | "IN"	-> {text; lemma; cat = P; sel = [D]}::lexify tokens (* Preposition or subordinating conjunction *)
    | "MD" -> {text; lemma; cat = T; sel = [V]}::lexify tokens	(* Modal *)
    | "EX"	-> {text; lemma; cat=X; sel =[]}::lexify tokens(* Existential there *)
    | "WP"	-> {text; lemma; cat=W; sel =[]}::lexify tokens (* Wh-pronoun *)
    | "CC"	(* Coordinating conjunction *)
    | "POS"	(* Possessive ending *)
    | "TO"	(* to *)
    | "WDT"	(* Wh-determiner *)
    | "WP$"	(* Possessive wh-pronoun *)
    | "WRB"	(* wh-adverb *) 
    | "SYM"	(* Symbol *)
    | "UH"	(* Interjection *)
    | "FW"	(* Foreign word *)
    | "PDT"	(* Predeterminer *)
    | "LS"	(* List item marker *)
    | "CD"	(* Cardinal number *)
    | "RP" -> let message = Printf.sprintf "POS tag: %s is unimplemented." tag in
      failwith message
    | other	-> let message = Printf.sprintf "Invalid POS tag: %s" other in
      failwith message

