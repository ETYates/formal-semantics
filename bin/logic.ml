type e = string
type t = True | False | Undef

type sem = Entity of e | Truth of t
         | Unary of (e -> t) | Binary of (e -> (e -> t))
         | Monadic of ((e -> t) -> t)
         | Dyadic of ((e -> t) -> ((e -> t) -> t))
         | Iotic of ((e -> t) -> e)

let conj = fun x -> fun y ->
  match x, y with
  | True, True -> True | True, False -> False
  | False, True -> False | False, False -> False
  | _ -> Undef

let disj = fun x -> fun y ->
  match x, y with
  | True, True -> True | True, False -> True
  | False, True -> True | False, False -> False
  | _ -> Undef

let impl = fun x -> fun y -> 
  match x, y with
  | True, True -> True | False, True -> True
  | True, False -> False | False, False -> True
  | _ -> Undef
