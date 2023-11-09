open Tree
type base_type = float

type 'a operation_class =
| Unary of ('a token * operation * ('a token * 'a token) option)
| Binary of ('a token * operation * ('a token * 'a token) option)


let string_of_oper = function
| Plus -> "+"
| Minus -> "-"
| Times -> "*"
| Div -> "/"
| Invert -> "i"
| Exp -> "^"
| Opp -> "-"

let print_oper o = print_string (string_of_oper o)

let rec print_spaces n = if (n > 0) then (print_char ' '; print_spaces (n - 1))

let rec print_tree ?(n=0) = function
| Leaf(Const(v)) -> print_spaces n; print_float v; print_newline ();
| Leaf(Variable(v)) -> print_spaces n; print_string v; print_newline ();
| BinaryNode(l, o, r) -> (print_tree ~n:(n + 1) l); print_spaces n;print_char 'b'; print_oper o; print_newline (); (print_tree ~n:(n + 1) r); 
| UnaryNode(o, r) -> print_spaces n; print_char 'u'; print_oper o; print_newline (); (print_tree ~n:(n + 1) r)

let rec string_of_expr  ?(op='+') = function
| Leaf(Const(v)) -> Printf.sprintf "%g" v;
| Leaf(Variable(v)) ->  v;
| BinaryNode(l, o, r) -> ((match o with
  | Plus | Minus -> (string_of_expr l)
  | Times | Div -> (match l with
    | BinaryNode(_, (Plus | Minus | Div), _) -> "(" ^ string_of_expr l ^ ")"
    | _ -> string_of_expr l)
  | Exp -> (match l with
    | BinaryNode(_, (Plus | Minus | Div | Times | Exp), _) -> "(" ^ string_of_expr l ^ ")"
    | _ -> string_of_expr l)
  | _ -> string_of_expr l)
   ^ (string_of_oper o) ^
  (match o with
    | Plus -> (string_of_expr r)
    | Minus ->  "(" ^ string_of_expr r ^ ")"
    | Times | Div -> (match r with
      | BinaryNode(_, (Plus | Minus | Div | Times), _) -> "(" ^ string_of_expr r ^ ")"
      | _ -> string_of_expr r)
    | Exp -> (match r with
      | BinaryNode(_, (Plus | Minus | Div | Times), _) -> "(" ^ string_of_expr r ^ ")"
      | _ -> string_of_expr r)
    | _ -> string_of_expr r)
    )
| UnaryNode(o, (BinaryNode(_, _, _) as n)) ->  (string_of_oper o) ^ "(" ^ (string_of_expr n) ^ ")"
| UnaryNode(o, n) ->  (string_of_oper o) ^ (string_of_expr n)

let print_expr e = print_string (string_of_expr e)

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

let rec map_until f tree = let tree2 = f tree in
	if tree2 = tree
	then tree
	else map_until f tree2

let rec eval_node_environ environ = function
| Leaf(Const(v)) -> v
| Leaf(Variable(v)) -> Hashtbl.find environ v
| BinaryNode(l, Plus, r) -> Float.add (eval_node_environ environ l) (eval_node_environ environ r)
| BinaryNode(l, Minus, r) -> Float.sub (eval_node_environ environ l) (eval_node_environ environ r)
| BinaryNode(l, Times, r) -> Float.mul (eval_node_environ environ l) (eval_node_environ environ r)
| BinaryNode(l, Div, r) -> Float.div (eval_node_environ environ l) (eval_node_environ environ r)
| BinaryNode(l, Exp, r) -> Float.pow (eval_node_environ environ l) (eval_node_environ environ r)
| UnaryNode(Opp, l) -> Float.neg (eval_node_environ environ l)
| UnaryNode(Minus, l) -> Float.neg (eval_node_environ environ l)
| UnaryNode(Invert, l) -> Float.div 1. (eval_node_environ environ l)
| _ -> Float.nan (* Unreached *)

let eval_node = eval_node_environ (Hashtbl.create 10)

and remove_unary = tree_map_a (function
| UnaryNode(Opp, n) -> BinaryNode(n, Times, Leaf(Const(-1.)))
| UnaryNode(Invert, n) -> BinaryNode(n, Exp, Leaf(Const(-1.)))
| other -> other)

let put_unary = tree_map_a (function
| BinaryNode(n, Times, Leaf(Const(-1.))) -> UnaryNode(Opp, n)
| BinaryNode(n, Exp, Leaf(Const(-1.))) -> UnaryNode(Invert, n)
| other -> other)

let remove_minus : (float node -> float node) = tree_map_a (function
| BinaryNode(a, Minus, b) -> BinaryNode(a, Plus, UnaryNode(Opp, b))
| other -> other)

let remove_div : (float node -> float node) = tree_map_a (function
| BinaryNode(a, Div, b) -> BinaryNode(a, Times, UnaryNode(Invert, b))
| other -> other)

let put_minus : (float node -> float node) = tree_map_a (function
| BinaryNode(a, Plus, UnaryNode(Opp, b)) -> BinaryNode(a, Minus, b)
| BinaryNode(UnaryNode(Opp, a), Plus, b) -> BinaryNode(b, Minus, a)
| other -> other)

let put_div = tree_map_a (function
| BinaryNode(a, Times, UnaryNode(Invert, b)) -> BinaryNode(a, Div, b)
| BinaryNode(UnaryNode(Invert, a), Times, b) -> BinaryNode(b, Div, a)
| UnaryNode(Invert, n) -> BinaryNode(Leaf(Const(1.)), Div, n)
| other -> other)

let expand node = tree_map_a (function
| BinaryNode(l, Times, BinaryNode(r1, Plus, r2)) -> (BinaryNode(BinaryNode(l, Times, r1), Plus, BinaryNode(l, Times, r2)))
| BinaryNode(BinaryNode(r1, Plus, r2), Times, l) -> (BinaryNode(BinaryNode(l, Times, r1), Plus, BinaryNode(l, Times, r2)))
| BinaryNode(BinaryNode(l1, Times, l2), Exp, r) -> (BinaryNode(BinaryNode(l1, Exp, r), Times, BinaryNode(l2, Exp, r)))
| other -> other) node

let rec prio ?(list=[Plus; Times; Exp]) x = match list with
| [] -> 1;
| e :: r when e = x -> 0;
| _ :: r -> 1 + prio ~list:r x

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
  | _ -> false

let  normalize = (let rec f = function
  | BinaryNode(l, ((Times | Plus) as op), r)
    when (expr_compare l r)
    -> tree_map_d f (BinaryNode(r, op, l))
  | BinaryNode(BinaryNode(ll, opl, lr), ((Times | Plus) as op), r)
    when ((opl = op) && (expr_compare lr r))
    -> tree_map_d f (BinaryNode(BinaryNode(ll, opl, r), op, lr))
  | BinaryNode(BinaryNode(_, ((Times | Plus) as opl), _) as l, op, BinaryNode(rl, opr, rr))
    when ((opl = opr) && (opr = op))
    -> tree_map_d f (BinaryNode(BinaryNode(l, op, rl), op, rr))
  | other -> other in tree_map_d f)

let factorize_out = tree_map_d (function
| BinaryNode(UnaryNode(Opp, l), Times, UnaryNode(Opp, r)) -> BinaryNode(l, Times, r)
| BinaryNode(UnaryNode(Opp, l), Plus, UnaryNode(Opp, r)) -> UnaryNode(Opp, BinaryNode(l, Plus, r))
| BinaryNode(UnaryNode(Opp, l), Times, r)
| BinaryNode(l, Times, UnaryNode(Opp, r)) -> UnaryNode(Opp, BinaryNode(l, Times, r))
| BinaryNode(l, Times, (UnaryNode(Invert, _) as r)) -> BinaryNode(r, Times, l)
| BinaryNode(BinaryNode(UnaryNode(Invert, _) as ll, Times, (_ as lr)), Times, r) -> BinaryNode(ll, Times, BinaryNode(lr, Times, r))
| BinaryNode(l, Times, BinaryNode(UnaryNode(Invert, _) as rl, Times, (_ as rr))) -> BinaryNode(rl, Times, BinaryNode(l, Times, rr))
| BinaryNode(l, Plus, BinaryNode(rl, Times, rr)) when l = rr -> BinaryNode(BinaryNode(rl, Plus, Leaf(Const(1.))), Times, l)
| BinaryNode(l, Plus, BinaryNode(rl, Times, rr)) when l = rl -> BinaryNode(BinaryNode(rr, Plus, Leaf(Const(1.))), Times, l)
| BinaryNode(BinaryNode(ll, Times, lr), Plus, r) when r = ll -> BinaryNode(BinaryNode(lr, Plus, Leaf(Const(1.))), Times, r)
| BinaryNode(BinaryNode(ll, Times, lr), Plus, r) when r = lr -> BinaryNode(BinaryNode(ll, Plus, Leaf(Const(1.))), Times, r)
| other -> other
)

let factorize_div : (float node -> float node) = tree_map_d (function
| BinaryNode(BinaryNode(ll, Div, lr), Times, BinaryNode(rl, Div, rr)) -> BinaryNode(BinaryNode(ll, Times, rl), Div, BinaryNode(lr, Times, rr))
| BinaryNode(BinaryNode(ll, Div, lr), Times, r) -> BinaryNode(BinaryNode(ll, Times, r), Div, lr)
| BinaryNode(l, Times, BinaryNode(rl, Div, rr)) -> BinaryNode(BinaryNode(l, Times, rl), Div, rr)
| other -> other
)

external feclearexcept : int -> unit = "feclearexcept"
external fetestexcept : int -> int = "fetestexcept"

let test_precision_lost f a b = feclearexcept 61; f a b; (Int.to_string (fetestexcept 61)) = "0"
let test_fraction f a b = (Float.equal (Float.of_int (Float.to_int (f a b))) (f a b))

let rec reduce_one = function
| BinaryNode(BinaryNode(ll, Div, lr), Times, BinaryNode(rl, Div, rr)) -> BinaryNode(BinaryNode(ll, Times, rl), Div, BinaryNode(lr, Times, rr))
| BinaryNode(BinaryNode(ll, Div, lr), Times, r) -> BinaryNode(BinaryNode(ll, Times, r), Div, lr)
| BinaryNode(l, Times, BinaryNode(rl, Div, rr)) -> BinaryNode(BinaryNode(l, Times, rl), Div, rr)
| BinaryNode(BinaryNode(ll, Div, lr), Plus, BinaryNode(rl, Div, rr)) when (lr = rr) -> BinaryNode(BinaryNode(ll, Plus, rl), Div, lr)
| BinaryNode(BinaryNode(ll, Div, lr), Minus, BinaryNode(rl, Div, rr)) when (lr = rr) -> BinaryNode(BinaryNode(ll, Minus, rl), Div, lr)

| BinaryNode(Leaf(Const(l)), Plus, Leaf(Const(r))) -> Leaf(Const(Float.add l r))
| BinaryNode(Leaf(Const(l)), Times, Leaf(Const(r))) -> Leaf(Const(Float.mul l r))
| BinaryNode(BinaryNode(_ as ll, Plus, Leaf(Const(lr))), Plus, Leaf(Const(r))) -> BinaryNode(ll, Plus, Leaf(Const(Float.add lr r)))
| BinaryNode(BinaryNode(_ as ll, Times, Leaf(Const(lr))), Times, Leaf(Const(r))) -> BinaryNode(ll, Times, Leaf(Const(Float.mul lr r)))
| BinaryNode(Leaf(Const(l)), Minus, Leaf(Const(r))) -> Leaf(Const(Float.sub l r))
| BinaryNode(Leaf(Const(l)), Div, Leaf(Const(r)))
(*  when (test_precision_lost Float.div l r) *)
  when (test_fraction Float.div l r)
  -> Leaf(Const(Float.div l r))
| BinaryNode(BinaryNode(ll, Div, (Leaf(Const(_)) as lr)), Div, (Leaf(Const(_)) as r)) -> (BinaryNode(ll, Div, BinaryNode(lr, Times, r)))
| BinaryNode(Leaf(Const(l)), Exp, Leaf(Const(r)))
(*  when (test_precision_lost Float.pow l r) *)
  when (test_fraction Float.pow l r)
  -> Leaf(Const(Float.pow l r))
| BinaryNode(Leaf(Const(l)), Exp, BinaryNode(Leaf(Const(rl)), Div, Leaf(Const(rr))))
(*  when (test_precision_lost Float.pow l r) *)
  when (test_fraction Float.pow l (rl /. rr))
  -> Leaf(Const(Float.pow l (rl /. rr)))

| UnaryNode(Opp, Leaf(Const(v))) -> Leaf(Const(Float.neg v))
| BinaryNode(l, Minus, Leaf(Const(r))) when ((Float.compare r 0.) < 0) -> BinaryNode(l, Plus, Leaf(Const(Float.neg r)))

| BinaryNode(UnaryNode(Opp, rl), Plus, rr) -> BinaryNode(rr, Minus, rl)
| UnaryNode(Opp, BinaryNode(UnaryNode(Opp, l), Minus, r)) -> BinaryNode(r, Plus, l)
| UnaryNode(Opp, BinaryNode(l, Div, r)) -> BinaryNode(UnaryNode(Opp, l), Div, r)
| UnaryNode(Opp, BinaryNode(l, Plus, r)) -> BinaryNode(UnaryNode(Opp, l), Minus, r)
| UnaryNode(Opp, BinaryNode(l, Minus, r)) -> BinaryNode(r, Minus, l)
| other -> other

let rec reduce_d = tree_map_d reduce_one

let rec reduce_a = tree_map_a reduce_one

let rec simplify = tree_map_d (function
| BinaryNode(_, Exp, Leaf(Const(0.))) -> Leaf(Const(1.))
| BinaryNode(r, Exp, Leaf(Const(1.))) -> r
| BinaryNode(Leaf(Const(0.)), Exp, Leaf(Const(c))) when ((Float.compare 0. c) > 0) ->  (Printf.fprintf stderr "Error: Division by zero.\n" ;exit 1)
| BinaryNode(Leaf(Const(0.)), Exp, _) -> Leaf(Const(0.))
| BinaryNode(Leaf(Const(1.)), Exp, _) -> Leaf(Const(1.))
| BinaryNode(_, Times, Leaf(Const(0.))) -> Leaf(Const(0.))
| BinaryNode(Leaf(Const(0.)), Times, _) -> Leaf(Const(0.))
| BinaryNode(r, Times, Leaf(Const(1.))) -> r
| BinaryNode(Leaf(Const(1.)), Times, r) -> r
| BinaryNode(l, Plus, Leaf(Const(0.))) -> l
| other -> other)


let factorize = tree_map_d (function
| BinaryNode(l, Plus, r)
  when l = r
  -> normalize (BinaryNode(Leaf(Const(2.)), Times, l))
| BinaryNode(ll, Plus, BinaryNode(Leaf(Const(n)), Times, r))
  when ll = r
  -> normalize (BinaryNode(r, Times, Leaf(Const(n+.1.))))
| BinaryNode(BinaryNode(ll, Times, Leaf(Const(n))), Plus, r)
  when ll = r
  -> normalize (BinaryNode(r, Times, Leaf(Const(n+.1.))))
| other -> other
)

let group_exp = tree_map_d (function
| BinaryNode(l, Times, r)
  when l = r
  -> normalize (BinaryNode(l, Exp, Leaf(Const(2.))))
| BinaryNode(BinaryNode(ll, Exp, nl), Times, r)
  when ll = r
  -> normalize (BinaryNode(ll, Exp, BinaryNode(nl, Plus, Leaf(Const(1.)))))
| BinaryNode(BinaryNode(ll, Exp, lr), Times, BinaryNode(rl, Exp, rr))
  when ll = rl
  -> normalize (BinaryNode(ll, Exp, BinaryNode(lr, Plus, rr)))
| other -> other
)

let rec has_variable = function
| Leaf(Variable _) -> true
| BinaryNode(l, _, r) -> (has_variable l) ||  (has_variable r)
| UnaryNode(_, n) -> (has_variable n)
| _ -> false
