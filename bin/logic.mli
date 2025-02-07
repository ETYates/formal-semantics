(** Type representing an entity. *)
type e = string

(** Type representing truth values including an undefined state. *)
type t = True | False | Undef

(** Type representing different semantic categories. *)
type sem = 
  | Entity of e 
  | Truth of t
  | Unary of (e -> t) 
  | Binary of (e -> (e -> t))
  | Monadic of ((e -> t) -> t)
  | Dyadic of ((e -> t) -> ((e -> t) -> t))
  | Iotic of ((e -> t) -> e)

(** Conjunction operation on truth values. *)
val conj : t -> t -> t

(** Disjunction operation on truth values. *)
val disj : t -> t -> t

(** Implication operation on truth values. *)
val impl : t -> t -> t

