
let prompt = "|- "

let rec run () = 
  print_string prompt;
  let raw_input = read_line () in
  match raw_input with
    | "Quit!" -> ()
    | raw_input -> let tokens = Token.tokenize raw_input in
      let lexes = Lexer.lexify tokens in
      let tree, _ = Parser.parse lexes in
      let lf_str = Lambda.fmt_expr tree.lf in
      print_endline lf_str;
      run ()


let _ = run ()
