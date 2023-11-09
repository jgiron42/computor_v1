/*stolen from https://v2.ocaml.org/manual/lexyacc.html*/
%{
%}
%token <float Tree.value> VAL
%token PLUS MINUS TIMES DIV EXP
%token LPAREN RPAREN
%token EOL
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%right EXP         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */
%token EOF
%start main             /* the entry point */
%type <float Tree.node> main expr
%%
main:
	expr EOF                { $1 }
;
expr:
	VAL                     { Tree.Leaf($1) }
  | LPAREN expr RPAREN      { $2 }
  | expr PLUS expr          { Tree.BinaryNode($1, Add, $3) }
//  | expr MINUS expr         { Tree.BinaryNode($1, Add, Tree.UnaryNode(Opp, $3)) }
  | expr MINUS expr         { Tree.BinaryNode($1, Sub, $3) }
  | expr TIMES expr         { Tree.BinaryNode($1, Multi, $3) }
//  | expr DIV expr         { Tree.BinaryNode($1, Multi, Tree.UnaryNode(Invert, $3)) }
  | expr DIV expr           { Tree.BinaryNode($1, Div, $3) }
  | expr EXP expr         { Tree.BinaryNode($1, Exp, $3) }
  | MINUS expr %prec UMINUS { Tree.UnaryNode(Opp, $2) }
  | PLUS expr %prec UMINUS { $2 }
;