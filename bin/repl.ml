
let prompt = "|- "

let rec run () = 
  print_string prompt;
  let raw_input = read_line () in
  match raw_input with
    | "Quit!" -> ()
    | raw_input -> let tokens = Token.tokenize raw_input in
      let lexes = Lexer.lexify tokens in
      let tree, _ = Parser.parse lexes in
      let lf = tree.lf in
      print_endline (Lambda.fmt_expr lf);
      run ()


let _ = run ()
