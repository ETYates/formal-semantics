(** Type representing lexical categories. *)
type cat = N | V | D | J | R | M | P | S | T | B | H | X | W | C | Conj

(** Type representing lexical entries. *)
type lex = { 
  text : string;
  lemma : string;
  cat : cat;
  sel : cat list;
}

(** Formats a lexical category as a string. *)
val fmt_cat : cat -> string

(** Formats a lexical entry as a string. *)
val fmt_lex : lex -> string

(** Converts a list of tokens into lexical entries. *)
val lexify : (string * string * string) list -> lex list

