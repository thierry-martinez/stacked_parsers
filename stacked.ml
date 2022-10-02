let parse str =
  let lexbuf = Lexing.from_string str in
  let stack = Stack.create () in
  Stack.push (Stacked_lexers.lex_s stack) stack;
  let stacked_lexer lexbuf =
    Stack.top stack lexbuf in
  Stacked_parsers.s stacked_lexer lexbuf

let var_parse str =
  let lexbuf = Lexing.from_string str in
  Stacked_parsers.var_s Stacked_lexers.lex_var_s lexbuf

let test parse =
  assert (parse "1 2 {ab (0123) cde} (4 {abc} 8)" = [1; 2; 2; 3; 3; 4; 3; 8])

let () =
  test parse;
  test var_parse
