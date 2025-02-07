(** Type representing a token with its text, lemma, and tag. *)
type token = string * string * string

(** Constructs a token from a string representation. *)
val token : token -> token

(** Formats a token as "text/tag". *)
val fmt_token : token -> string

(** Loads a spaCy model given its name. *)
val load_model : string -> Py.Object.t

(** The loaded spaCy model instance. *)
val model : Py.Object.t

(** Converts a Python object representing a token into an OCaml tuple. *)
val py_to_tuple : Py.Object.t -> token

(** Tokenizes a raw input string into a list of tokens. *)
val tokenize : string -> token list
