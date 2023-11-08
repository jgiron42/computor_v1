open Tree
type base_type = float

type 'a operation_class =
| Unary of ('a token * operation * ('a token * 'a token) option)
| Binary of ('a token * operation * ('a token * 'a token) option)


let print_oper = function
| Add -> print_string "+"
| Sub -> print_string "-"
| Multi -> print_string "*"
| Div -> print_string "/"
| Invert -> print_string "i"
| Exp -> print_string "^"
| Opp -> print_string "-"

let rec print_lex = function
| t :: r ->
  (match t with
  | BracketOpen -> print_string "BracketOpen: "; print_char '('; print_newline ()
  | BracketClose -> print_string "BracketClose: "; print_char ')'; print_newline ()
  | Value(Const(v)) -> print_string "Value: "; print_float v; print_newline ()
  | Value(Variable(v)) -> print_string "Variable: "; print_string v; print_newline ()
  | Oper(o) -> print_string "Oper: "; print_oper o; print_newline ()
  | End -> print_endline "End"
  ); print_lex r
| [] -> ();;
let rec print_stack s = if ((Stack.length s) == 0) then () else (print_lex [Stack.pop s]; print_stack s);;

let rec print_spaces n = if (n > 0) then (print_char ' '; print_spaces (n - 1));;
let rec print_tree ?(n=0) = function
| Leaf(Const(v)) -> print_spaces n; print_float v; print_newline ();
| Leaf(Variable(v)) -> print_spaces n; print_string v; print_newline ();
| BinaryNode(l, o, r) -> (print_tree ~n:(n + 1) l); print_spaces n;print_char 'b'; print_oper o; print_newline (); (print_tree ~n:(n + 1) r); 
| UnaryNode(o, r) -> print_spaces n; print_char 'u'; print_oper o; (print_tree ~n:(n + 1) r);
;;


let rec print_expr = function
| Leaf(Const(v)) -> print_float v; 
| Leaf(Variable(v)) ->  print_string v;
| BinaryNode(l, o, r) -> print_char '(';(print_expr l); print_oper o; (print_expr r); print_string ")"; 
| UnaryNode(o, r) ->  print_oper o; (print_expr r)

let rec tree_map_a f tree = match (f tree) with
| UnaryNode(o, n) -> (UnaryNode(o, tree_map_a f n))
| BinaryNode(l, o, r) -> (BinaryNode(tree_map_a f l, o, tree_map_a f r))
| other -> other

let parse s = try
				let lexbuf = Lexing.from_string s in
				  try Parser.main Lexer.token lexbuf
				  with Stdlib.Parsing.Parse_error -> (
					Printf.fprintf stderr "Invalid syntax, unexpected token: '%s'\n" (Lexing.lexeme lexbuf);
					exit 0
				  )
			  with
			  | Lexer.Eof -> (
				Printf.fprintf stderr "Unexpected end of file\n";
				exit 0
			  )
			  | Lexer.InvalidChar(c) -> (
				Printf.fprintf stderr "Unexpected char: '%c'\n" c;
				exit 0
			  )

let rec tree_map_d f tree = f (match tree with
| UnaryNode(o, n) -> (UnaryNode(o, tree_map_d f n))
| BinaryNode(l, o, r) -> (BinaryNode(tree_map_d f l, o, tree_map_d f r))
| other -> other)
;;

let rec eval_node_environ environ = function
| Leaf(Const(v)) -> v
| Leaf(Variable(v)) -> Hashtbl.find environ v
| BinaryNode(l, Add, r) -> Float.add (eval_node_environ environ l) (eval_node_environ environ r)
| BinaryNode(l, Sub, r) -> Float.sub (eval_node_environ environ l) (eval_node_environ environ r)
| BinaryNode(l, Multi, r) -> Float.mul (eval_node_environ environ l) (eval_node_environ environ r)
| BinaryNode(l, Div, r) -> Float.div (eval_node_environ environ l) (eval_node_environ environ r)
| BinaryNode(l, Exp, r) -> Float.pow (eval_node_environ environ l) (eval_node_environ environ r)
| UnaryNode(Opp, l) -> Float.neg (eval_node_environ environ l)
| UnaryNode(Sub, l) -> Float.neg (eval_node_environ environ l)
| UnaryNode(Invert, l) -> Float.div 1. (eval_node_environ environ l)
| _ -> Float.nan (* Unreached *);;

let eval_node = eval_node_environ (Hashtbl.create 10)

let remove_unary = tree_map_a (function
| UnaryNode(Opp, n) -> BinaryNode(n, Multi, Leaf(Const(-1.)))
| UnaryNode(Invert, n) -> BinaryNode(n, Exp, Leaf(Const(-1.)))
| other -> other);;

let put_unary = tree_map_a (function
| BinaryNode(n, Multi, Leaf(Const(-1.))) -> UnaryNode(Opp, n)
| BinaryNode(n, Exp, Leaf(Const(-1.))) -> UnaryNode(Invert, n)
| other -> other);;

let put_minus = tree_map_a (function
| BinaryNode(a, Add, UnaryNode(Opp, b)) -> BinaryNode(a, Sub, b)
| BinaryNode(UnaryNode(Opp, a), Add, b) -> BinaryNode(b, Sub, a)
| Leaf(Const(1.)) -> Leaf(Const(1.)) (*for type inference TODO: remove*)
| other -> other);;

let put_div = tree_map_a (function
| BinaryNode(a, Multi, UnaryNode(Invert, b)) -> BinaryNode(a, Div, b)
| BinaryNode(UnaryNode(Invert, a), Multi, b) -> BinaryNode(b, Div, a)
| UnaryNode(Invert, n) -> BinaryNode(Leaf(Const(1.)), Div, n)
| other -> other);;

let expand node = tree_map_a (function
| BinaryNode(l, Multi, BinaryNode(r1, Add, r2)) -> (BinaryNode(BinaryNode(l, Multi, r1), Add, BinaryNode(l, Multi, r2)))
| BinaryNode(BinaryNode(r1, Add, r2), Multi, l) -> (BinaryNode(BinaryNode(l, Multi, r1), Add, BinaryNode(l, Multi, r2)))
| other -> other) node;;

let rec prio ?(list=[Add; Multi; Exp]) x = match list with
| [] -> 1;
| e :: r when e = x -> 0;
| _ :: r -> 1 + prio ~list:r x;;

let rec expr_compare l r =
  match l with
  | BinaryNode(_,opl,_) -> (match r with
    | BinaryNode(_,opr,_) -> (prio opl) > (prio opr)
    | _ -> false
  )
  | Leaf(Const(_)) -> (
    match r with
    | BinaryNode _ | Leaf(Variable _) -> true
    | _ -> false
  )
  | Leaf(Variable(lv)) -> (
    match r with
    | BinaryNode _ -> true
    | Leaf(Variable(rv)) -> (compare lv rv) < 0
    | _ -> false
  )
  | _ -> false;;

let  normalize = (let rec f = function
  | BinaryNode(l, ((Multi | Add) as op), r)
    when (expr_compare l r)
    -> tree_map_d f (BinaryNode(r, op, l))
  | BinaryNode(BinaryNode(ll, opl, lr), ((Multi | Add) as op), r)
    when ((opl = op) && (expr_compare lr r))
    -> tree_map_d f (BinaryNode(BinaryNode(ll, opl, r), op, lr))
  | BinaryNode(BinaryNode(_, ((Multi | Add) as opl), _) as l, op, BinaryNode(rl, opr, rr))
    when ((opl = opr) && (opr = op))
    -> tree_map_d f (BinaryNode(BinaryNode(l, op, rl), op, rr))
  | other -> other in tree_map_d f);;

let factorize_out = tree_map_d (function
| BinaryNode(UnaryNode(Opp, l), Multi, UnaryNode(Opp, r)) -> BinaryNode(l, Multi, r)
| BinaryNode(UnaryNode(Opp, l), Add, UnaryNode(Opp, r)) -> UnaryNode(Opp, BinaryNode(l, Add, r))
| BinaryNode(UnaryNode(Opp, l), Multi, r)
| BinaryNode(l, Multi, UnaryNode(Opp, r)) -> UnaryNode(Opp, BinaryNode(l, Multi, r))
| BinaryNode(l, Multi, (UnaryNode(Invert, _) as r)) -> BinaryNode(r, Multi, l)
| BinaryNode(BinaryNode(UnaryNode(Invert, _) as ll, Multi, (_ as lr)), Multi, r) -> BinaryNode(ll, Multi, BinaryNode(lr, Multi, r))
| BinaryNode(l, Multi, BinaryNode(UnaryNode(Invert, _) as rl, Multi, (_ as rr))) -> BinaryNode(rl, Multi, BinaryNode(l, Multi, rr))
| BinaryNode(l, Add, BinaryNode(rl, Multi, rr)) when l = rr -> BinaryNode(BinaryNode(rl, Add, Leaf(Const(1.))), Multi, l)
| BinaryNode(l, Add, BinaryNode(rl, Multi, rr)) when l = rl -> BinaryNode(BinaryNode(rr, Add, Leaf(Const(1.))), Multi, l)
| BinaryNode(BinaryNode(ll, Multi, lr), Add, r) when r = ll -> BinaryNode(BinaryNode(lr, Add, Leaf(Const(1.))), Multi, r)
| BinaryNode(BinaryNode(ll, Multi, lr), Add, r) when r = lr -> BinaryNode(BinaryNode(ll, Add, Leaf(Const(1.))), Multi, r)
| other -> other
)

let rec reduce = tree_map_d (function
| BinaryNode(Leaf(Const(l)), Add, Leaf(Const(r))) -> Leaf(Const(Float.add l r))
| BinaryNode(Leaf(Const(l)), Multi, Leaf(Const(r))) -> Leaf(Const(Float.mul l r))
| BinaryNode(BinaryNode(_ as ll, Add, Leaf(Const(lr))), Add, Leaf(Const(r))) -> BinaryNode(ll, Add, Leaf(Const(Float.add lr r)))
| BinaryNode(BinaryNode(_ as ll, Multi, Leaf(Const(lr))), Multi, Leaf(Const(r))) -> BinaryNode(ll, Multi, Leaf(Const(Float.mul lr r)))
| BinaryNode(Leaf(Const(l)), Sub, Leaf(Const(r))) -> Leaf(Const(Float.sub l r))
| BinaryNode(Leaf(Const(l)), Div, Leaf(Const(r)))
  when (Float.compare (Float.rem l r) 0.) == 0
  -> Leaf(Const(Float.div l r))
| BinaryNode(Leaf(Const(l)), Exp, Leaf(Const(r)))
  when ((Float.compare (Float.rem 1. r) 0.) == 0 && (Float.compare (Float.pow (Float.pow l r) (1. /. r)) l) == 0)
  -> Leaf(Const(Float.pow l r))
| UnaryNode(Opp, Leaf(Const(v))) -> Leaf(Const(Float.neg v))
| other -> other)
;;

let rec simplify = tree_map_d (function
| BinaryNode(_, Exp, Leaf(Const(0.))) -> Leaf(Const(1.))
| BinaryNode(r, Exp, Leaf(Const(1.))) -> r
| BinaryNode(Leaf(Const(0.)), Exp, _) -> Leaf(Const(0.))
| BinaryNode(Leaf(Const(1.)), Exp, _) -> Leaf(Const(1.))
| BinaryNode(_, Multi, Leaf(Const(0.))) -> Leaf(Const(0.))
| BinaryNode(Leaf(Const(0.)), Multi, _) -> Leaf(Const(0.))
| BinaryNode(r, Multi, Leaf(Const(1.))) -> r
| BinaryNode(Leaf(Const(1.)), Multi, r) -> r
| other -> other)
;;

let factorize = tree_map_d (function
| BinaryNode(l, Add, r)
  when l = r
  -> normalize (BinaryNode(Leaf(Const(2.)), Multi, l))
| BinaryNode(ll, Add, BinaryNode(Leaf(Const(n)), Multi, r))
  when ll = r
  -> normalize (BinaryNode(r, Multi, Leaf(Const(n+.1.))))
| BinaryNode(BinaryNode(ll, Multi, Leaf(Const(n))), Add, r)
  when ll = r
  -> normalize (BinaryNode(r, Multi, Leaf(Const(n+.1.))))
| other -> other
);;

let group_exp = tree_map_d (function
| BinaryNode(l, Multi, r)
  when l = r
  -> normalize (BinaryNode(l, Exp, Leaf(Const(2.))))
| BinaryNode(BinaryNode(ll, Exp, Leaf(Const(n))), Multi, r)
  when ll = r
  -> normalize (BinaryNode(ll, Exp, Leaf(Const(n+.1.))))
| other -> other
);;

let rec has_variable = function
| Leaf(Variable _) -> true
| BinaryNode(l, _, r) -> (has_variable l) ||  (has_variable r)
| UnaryNode(_, n) -> (has_variable n)
| _ -> false;;
