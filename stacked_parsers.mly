%token PAREN_LEFT "("
%token PAREN_RIGHT ")"
%token CURLY_LEFT "{"
%token CURLY_RIGHT "}"

(* First variant: with explicit stack of lexers *)
%token <int> INT
%token <string> STR
%start <int list> s
%start <string list> s2

(* Second variant: with nested calls to parsers *)
%token <int list> INTS
%token <string list> STRS
%start <int list> var_s
%start <string list> var_s2
%%

(* First variant *)

let s := items = items*; PAREN_RIGHT; { List.flatten items }

let items :=
  | PAREN_LEFT; ~ = s; <>
  | CURLY_LEFT; strs = s2; { List.map String.length strs }
  | i = INT; { [i] }

let s2 := items = items2*; CURLY_RIGHT; { List.flatten items }

let items2 :=
  | CURLY_LEFT; ~ = s2; <>
  | PAREN_LEFT; ints = s; { List.map string_of_int ints }
  | str = STR; { [str] }

(* Second variant *)

let var_s := items = var_items*; PAREN_RIGHT; { List.flatten items }

let var_items :=
  | PAREN_LEFT; ~ = var_s; <>
  | strs = STRS; { List.map String.length strs }
  | i = INT; { [i] }

let var_s2 := items = var_items2*; CURLY_RIGHT; { List.flatten items }

let var_items2 :=
  | CURLY_LEFT; ~ = var_s2; <>
  | ints = INTS; { List.map string_of_int ints }
  | str = STR; { [str] }
