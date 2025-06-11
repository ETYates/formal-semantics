(** Type representing an entity. *)
type e = Named of string | Anon of string | Undef

(** Type representing truth values including an undefined state. *)
type t = True | False | Unk
type tau = Entity of e 
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
         ; theory : Lambda.expr list}

type execution = Model of m | Tau of tau

val t_to_bool : t -> bool

val m : m

val fmt_truth : t -> string

val fmt_entity : e -> string

val fmt_entities : e list -> string

val fmt_model : m -> string

val fmt_tau : tau -> string

val assoc : 'a -> ('a * 'b list) list -> 'b list 

val add_assoc : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

val eval : m -> Lambda.expr -> tau

val eval_term : m -> Lambda.term -> e

(** Conjunction operation on truth values. *)
val conj : t -> t -> t

(** Disjunction operation on truth values. *)
val disj : t -> t -> t

(** Implication operation on truth values. *)
val impl : t -> t -> t

