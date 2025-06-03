
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
      let execution = Logic.eval model statement in
      match execution with
      | Logic.Model model -> exec_flag flag tree model; run model
      | Logic.Tau tau -> exec_flag flag tree model; 
        print_endline (Logic.fmt_tau tau); run model

let _ = run Logic.m 
