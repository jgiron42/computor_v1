(*stolen from https://v2.ocaml.org/manual/lexyacc.html*)
{
	open Parser
	exception Eof
	exception InvalidChar of char
}
rule token = parse
	[' ' '\t']     { token lexbuf }
  | eof			   { EOF }
  | ['0'-'9']+("."['0'-'9']+)? as lxm { VAL(Const(float_of_string lxm)) }
  | ['A'-'Z']+ as lxm { VAL(Variable(lxm)) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '^'            { EXP }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            { raise Eof }
  | _ as char      { raise (InvalidChar char) }