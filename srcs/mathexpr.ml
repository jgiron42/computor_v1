type base_type = float;;

type operation =
| Add
| Sub
| Multi
| Div
| Exp
| Opp
| Invert
;;

type 'a value =
| Const of 'a
| Variable of string

type 'a token = 
| Value of 'a value
| Oper of operation
| BracketOpen
| BracketClose
| End
;;

type 'a node = 
| Leaf of 'a value
| BinaryNode of ('a node * operation * 'a node)
| UnaryNode of (operation * 'a node)
;;

type 'a operation_class =
| Unary of ('a token * operation * ('a token * 'a token) option)
| Binary of ('a token * operation * ('a token * 'a token) option)
;;

let print_oper = function
| Add -> print_string "+"
| Sub -> print_string "-"
| Multi -> print_string "*"
| Div -> print_string "/"
| Invert -> print_string "i"
| Exp -> print_string "^"
| Opp -> print_string "-";;
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

let rec print_expr = (function
| Leaf(Const(v)) -> print_float v; 
| Leaf(Variable(v)) ->  print_string v;
| BinaryNode(l, o, r) -> print_char '(';(print_expr l); print_oper o; (print_expr r); print_string ")"; 
| UnaryNode(o, r) ->  print_oper o; (print_expr r););
;;


exception InvalidCharacter of char;;
exception UnexpectedToken of (base_type token);;
exception UnresolvedVariable of (string);;

let parse_float s n =
  let reg = Str.regexp {|[0-9]*\(\.[0-9]*\)?|} in
  (
    ignore (Str.string_match reg s n);
    let v = float_of_string (Str.matched_string s) in
    (v, Str.match_end ())
  ) 
let parse_var s n =
  let reg = Str.regexp {|[a-zA-Z]*|} in
  (
    ignore (Str.string_match reg s n);
    let v = (Str.matched_string s) in
    (v, Str.match_end ())
  ) 

let rec lex ?(n=0) s = 
if ((n) >= (String.length s)) then [End] else
match s.[n] with
| '(' -> BracketOpen :: lex s ~n:(n+1)
| ')' -> BracketClose :: lex s ~n:(n+1)
| '+' -> Oper (Add) :: lex s ~n:(n+1)
| '-' -> Oper (Sub) :: lex s ~n:(n+1)
| '*' -> Oper (Multi) :: lex s ~n:(n+1)
| '^' -> Oper (Exp) :: lex s ~n:(n+1)
| '/' -> Oper (Div) :: lex s ~n:(n+1)
| '0' .. '9' -> let (v, r) = parse_float s n in Value(Const(v)) :: lex s ~n:r
| 'a' .. 'z' | 'A' .. 'Z' -> let (v, r) = parse_var s n in Value(Variable(v)) :: lex s ~n:r
| ' ' | '\n' | '\t' -> lex s ~n:(n+1)
| c -> raise (InvalidCharacter c);;

let rec stack_from_list = function
| [] -> Stack.create ()
| e :: r -> let s = (stack_from_list r) in Stack.push e s; s;;

let rec push_list_to_stack s= function
| [] ->  ()
| e :: r -> push_list_to_stack s r; Stack.push e s;;

let tok_match s tok (reciprocal: (('a token * 'a token) option)) =
  if (Stack.top s) = tok then ((ignore (Stack.pop s); true))
  else 
    (
      match reciprocal with 
      | Some(t, r) -> if ((Stack.top s) = t) then (ignore (Stack.pop s);ignore (Stack.push r s); true) else false
      | None -> false
    );;

let rec parse_val s = 
  match Stack.pop s with
  | Value(v) -> Leaf(v)
  | BracketOpen -> (
    let ret = parse s in
    if (tok_match s BracketClose None) then ret else raise(UnexpectedToken (Stack.top s))
    )
  | t -> raise(UnexpectedToken t);

and parse_gen op_list s = match op_list with
| [] -> parse_val s
| Binary(tok, f, reciprocal) :: r -> (
  let left = (parse_gen r s) in
  if Stack.is_empty s then left else
    (if (tok_match s tok reciprocal)
      then BinaryNode(left, f, parse_gen op_list s)
      else left)
)
| Unary(tok, f, reciprocal) :: r -> (
    if (tok_match s tok reciprocal)
    then (UnaryNode(f, parse_gen op_list s))
    else (parse_gen r s)
);

and parse s = parse_gen
  [
    Binary( Oper(Add), Add, Some(Oper(Sub), Oper(Opp)));
    Unary( Oper(Opp), Opp, None); (*hack*)
    Binary( Oper(Multi), Multi, Some(Oper(Div), Oper(Invert)));
    Unary( Oper(Invert), Invert, None); (*also hack*)
    Binary( Oper(Exp), Exp, None);
    Unary( Oper(Sub), Opp, None);
  ] s;;

let s_to_tree s = lex s |> stack_from_list |> parse;;

let rec tree_map_a f tree = match (f tree) with
| UnaryNode(o, n) -> (UnaryNode(o, tree_map_a f n))
| BinaryNode(l, o, r) -> (BinaryNode(tree_map_a f l, o, tree_map_a f r))
| other -> other
;;
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

let rec reduce = tree_map_d (function
| BinaryNode(Leaf(Const(l)), Add, Leaf(Const(r))) -> Leaf(Const(Float.add l r))
| BinaryNode(Leaf(Const(l)), Multi, Leaf(Const(r))) -> Leaf(Const(Float.mul l r))
| BinaryNode(BinaryNode(_ as ll, Add, Leaf(Const(lr))), Add, Leaf(Const(r))) -> BinaryNode(ll, Add, Leaf(Const(Float.add lr r)))
| BinaryNode(BinaryNode(_ as ll, Multi, Leaf(Const(lr))), Multi, Leaf(Const(r))) -> BinaryNode(ll, Multi, Leaf(Const(Float.mul lr r)))
| BinaryNode(Leaf(Const(l)), Sub, Leaf(Const(r))) -> Leaf(Const(Float.sub l r))
| BinaryNode(Leaf(Const(l)), Div, Leaf(Const(r)))
  when (Float.compare (Float.rem l r) 0.) == 0
  -> Leaf(Const(Float.mul l r))
| UnaryNode(Opp, Leaf(Const(v))) -> Leaf(Const(Float.neg v))
| other -> other)
;;

let rec simplify = tree_map_d (function
| BinaryNode(_, Exp, Leaf(Const(0.))) -> Leaf(Const(1.))
| BinaryNode(r, Exp, Leaf(Const(1.))) -> r
| BinaryNode(Leaf(Const(0.)), Exp, _) -> Leaf(Const(0.))
| BinaryNode(Leaf(Const(1.)), Exp, _) -> Leaf(Const(1.2))
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
| BinaryNode(ll, Multi, BinaryNode(Leaf(Const(n)), Add, r))
  when ll = r
  -> normalize (BinaryNode(ll, Multi, Leaf(Const(n+.1.))))
| BinaryNode(BinaryNode(Leaf(Const(n)), Multi, lr), Add, r)
  when lr = r
  -> normalize (BinaryNode(Leaf(Const(n+.1.)), Multi, lr))
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
