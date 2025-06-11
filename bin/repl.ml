
let prompt = "|- "

let proc_flag raw_input =
  let strs = String.split_on_char '-' raw_input in
  match strs with
  | [] -> "", ""
  | [str] -> (String.trim str), ""
  | str::strs -> let sentence = String.trim str in
    let flag = "-" ^ (String.concat "-" strs |> String.trim) in 
    sentence, flag

let exec_flag flag (tree : Parser.tree) model =
  match flag with
  | "" -> ()
  | "--show-lf" -> print_endline (Lambda.fmt_expr tree.lf)
  | "--show-model" -> print_endline (Logic.fmt_model model)
  | "--show-tree" -> Parser.print_tree tree
  | _ -> let message = Printf.sprintf "Invalid cli-flag: %s" flag in
    failwith message

let rec run model = 
  print_string prompt;
  let raw_input = read_line () in
  let raw_input, flag = proc_flag raw_input in
  match raw_input with
    | "Quit!" -> ()
    | "Show model!" -> print_endline (Logic.fmt_model model); run model
    | raw_input -> let tokens = Token.tokenize raw_input in
      let lexes = Lexer.lexify tokens in
      let statement, tree = Parser.parse lexes in
      match statement with
      | Query expr -> let tau = Logic.eval model expr in
        exec_flag flag tree model; 
        (match tau with
        | Unary p -> let f p x = Logic.t_to_bool (p x) in
          let es = List.filter (f p) model.entities in
          print_endline (Logic.fmt_entities es); run model
        | Logic.Truth _ -> print_endline (Logic.fmt_tau tau); run model
        | _ -> failwith "Queries should only return truth values and unary predicates.")
      | Decl expr ->                    
        match Decl.decl model true expr with
        | Model model -> exec_flag flag tree model; run model
        | Single q -> let model = q model in exec_flag flag tree model; run model
        | other -> print_endline (Decl.fmt_decl other); failwith "Failed declaration. Final value is not a model."
let _ = run Logic.m 
