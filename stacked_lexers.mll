(* First variant: with explicit stack of lexers *)

rule lex_s stack = parse
  | [ ' ' '\n' '\t' ] { lex_s stack lexbuf }
  | ['0' - '9']+ as int { Stacked_parsers.INT (int_of_string int) }
  | "(" {
      Stack.push (lex_s stack) stack;
      PAREN_LEFT
    }
  | ")" | eof {
      let _ : _ -> _ = Stack.pop stack in
      PAREN_RIGHT
    }
  | "{" {
      Stack.push (lex_s2 stack) stack;
      CURLY_LEFT
    }

and lex_s2 stack = parse
  | [ ' ' '\n' '\t' ] { lex_s2 stack lexbuf }
  | [ 'A' - 'Z' 'a' - 'z' ]+ as str { Stacked_parsers.STR str }
  | "(" {
      Stack.push (lex_s stack) stack;
      PAREN_LEFT
    }
  | "{" {
      Stack.push (lex_s2 stack) stack;
      CURLY_LEFT
     }
  | "}" | eof {
      let _ : _ -> _ = Stack.pop stack in
      CURLY_RIGHT
    }

(* Second variant: with nested calls to parsers *)

and lex_var_s = parse
  | [ ' ' '\n' '\t' ] { lex_var_s lexbuf }
  | ['0' - '9']+ as int { Stacked_parsers.INT (int_of_string int) }
  | "(" { PAREN_LEFT }
  | ")" | eof { PAREN_RIGHT }
  | "{" {
      let strs = Stacked_parsers.var_s2 lex_var_s2 lexbuf in
      STRS strs
    }

and lex_var_s2 = parse
  | [ ' ' '\n' '\t' ] { lex_var_s2 lexbuf }
  | [ 'A' - 'Z' 'a' - 'z' ]+ as str { Stacked_parsers.STR str }
  | "(" {
      let ints = Stacked_parsers.var_s lex_var_s lexbuf in
      INTS ints
    }
  | "{" { CURLY_LEFT }
  | "}" | eof { CURLY_RIGHT }
