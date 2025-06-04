(** Type representing an entity. *)
type e = Named of string | Anon of string | Undef

(** Type representing truth values including an undefined state. *)
type t = True | False | Unk
type tau = Entity of e | Entities of e list | Truth of t

type model = {entities : e list
             ; unaries : (string * (e * bool) list) list
             ; binaries : (string * (e * (e * bool) list) list) list
             ; theory : Lambda.expr list}

type execution = Model of model | Tau of tau

type env = (Lambda.term * e) list

val m : model

val fmt_entity : e -> string

val fmt_entities : e list -> string

val fmt_model : model -> string

val fmt_tau : tau -> string

val eval : model -> Lambda.statement -> execution

(** Conjunction operation on truth values. *)
val conj : t -> t -> t

(** Disjunction operation on truth values. *)
val disj : t -> t -> t

(** Implication operation on truth values. *)
val impl : t -> t -> t

val query : model -> env -> Lambda.expr -> tau
