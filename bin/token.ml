type token = string * string * string

let token str : token = str

let () = Py.initialize ()

let fmt_token token = 
  let (text, _, tag) = token in
  Printf.sprintf "%s/%s" text tag

let load_model name = 
  let spacy = Py.import "spacy" in
  let load = Py.Module.get_function spacy "load" in
  let name = Py.String.of_string name in
  load [|name|]

let model = load_model "en_core_web_sm"

let py_to_tuple pyobject =
  let text_attr = Py.String.of_string "text" in
  let text = Py.Object.find_attr pyobject text_attr in
  let tag_attr = Py.String.of_string "tag_" in
  let tag = Py.Object.find_attr pyobject tag_attr in
  let lemma_attr = Py.String.of_string "lemma_" in
  let lemma = Py.Object.find_attr pyobject lemma_attr in
  let lemma = Py.Object.to_string lemma in
  let text = Py.Object.to_string text in
  let tag = Py.Object.to_string tag in
  (text,lemma, tag)

let tokenize raw_input = 
  let call = Py.Module.get_function model "__call__" in
  let doc = call [|Py.String.of_string raw_input|] in
  let pyobjects = Py.List.to_list doc in
  let tuples = List.map py_to_tuple pyobjects in
  let tokens = List.map token tuples in
  tokens
