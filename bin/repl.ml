
let prompt = "|- "

let rec run model = 
  print_string prompt;
  let raw_input = read_line () in
  match raw_input with
    | "Quit!" -> ()
    | raw_input -> let tokens = Token.tokenize raw_input in
      let lexes = Lexer.lexify tokens in
      let statement, _ = Parser.parse lexes in
      (* Parser.print_tree tree; *) 
      let execution = Logic.eval model statement in
      match execution with
      | Logic.Model m -> run m
      | Logic.Tau tau -> 
        match tau with
        | Truth True -> print_endline "Yes."; run model
        | Truth False -> print_endline "No."; run model
        | Truth Unk -> print_endline "Unknown."; run model
        | Entity e -> print_endline (Logic.fmt_entity e); run model
        | Entities es -> print_endline (Logic.fmt_entities es); run model


let _ = run Logic.m 
