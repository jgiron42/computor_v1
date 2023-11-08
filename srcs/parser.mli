type token =
  | VAL of (float Tree.value)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | EXP
  | LPAREN
  | RPAREN
  | EOL
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> float Tree.node
