open Lambda

type e = string
type v = string
type t = True | False | Unk

type model = { entities : e list
             ; events : v list
             ; single : (string * (e list)) list
             ; double : (string * (e * e list) list) list
             ; unary : (string * (e * v list) list) list
             ; binary : (string * (e * (e * v list) list) list) list }

let conj = fun x -> fun y ->
  match x, y with
  | True, True -> True | True, False -> False
  | False, True -> False | False, False -> False
  | _ -> Unk

let disj = fun x -> fun y ->
  match x, y with
  | True, True -> True | True, False -> True
  | False, True -> True | False, False -> False
  | _ -> Unk

let impl = fun x -> fun y -> 
  match x, y with
  | True, True -> True | False, True -> True
  | True, False -> False | False, False -> True
  | _ -> Unk
