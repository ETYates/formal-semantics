open Lexer

(** Type representing data in a parse tree. *)
type data = 
  | Trees of tree * tree
  | Text of { text : string; lemma : string }

(** Type representing a parse tree node. *)
and tree = { 
  data : data;
  cat : cat;
  sel : cat list;
  arity : int ref;
  lf : Lambda.expr 
}

(** Prints a tree with indentation for structure visualization. *)
val print_tree : ?indent:int -> tree -> unit

(** Formats a tree as a string. *)
val fmt_tree : tree -> string

(** Determines if the first tree selects the second. *)
val selects : tree -> tree -> bool

(** Merges two trees if selection criteria are met. *)
val merge : tree -> tree -> tree option

(** Converts a lexical entry into a tree. *)
val lex_to_tree : lex -> tree

(** Recursively sets the logical form of a tree. *)
val set_lf : tree -> tree

(** Parses a list of trees into a structured parse tree and stack. *)
val parse_trees : tree list -> tree * tree list

(** Parses a list of lexical entries into a parse tree. *)
val parse : lex list -> tree * tree list

