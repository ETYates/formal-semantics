(** Type representing a term, which can be a constant or a variable. *)
type term = Const of string | Var of string | Expr of expr

(** Type representing an expression in the formal system. *)
and expr =
  | Pred of { name : term; args : term list }
  | Bind of { binder : binder; var : term; expr : expr }
  | Op of op * expr list
  | Term of term
  | Null

(** Type representing binders such as lambda, universal, existential, and unique quantifiers. *)
and binder = Lambda | ForAll | Exists | Unique

(** Type representing logical operators. *)
and op = Not | And | Or | If

and statement = Decl of expr | Query of expr
(** Formats a binder as a string. *)
val fmt_binder : binder -> string

val add_e : expr -> expr
val abstract_e : expr -> expr

(** Formats a list of terms as a string. *)
val fmt_terms : term list -> string

(** Formats a single term as a string. *)
val fmt_term : term -> string

(** Formats a logical operator as a string. *)
val fmt_op : op -> string

(** Formats an expression as a string. *)
val fmt_expr : expr -> string

(** Finds a free variable in an expression, if it exists. *)
val free_var : expr -> term option
val free_var' : expr -> term option

(** Lifts a lambda binder in an expression. *)
val lift_bind : expr -> expr

(** Substitutes a term within an expression. *)
val subst_term : term -> term -> term -> term

(** Substitutes a term within an expression recursively. *)
val subst_terms : term -> term -> expr -> expr

(** Substitutes an expression for a variable within another expression. *)
val subst_expr : term -> expr -> expr -> expr

(** Performs alpha conversion on an expression. *)
val alpha_conv : term -> term -> expr -> expr

(** Applies one expression to another. *)
val apply : expr -> expr -> expr

(** Test expression example. *)
val test : expr

val unique : expr
