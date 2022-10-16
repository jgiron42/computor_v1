open Printf;;
open Mathexpr;;

if ((Array.length Sys.argv) != 2) then (Printf.fprintf stderr "Invalid number of arguments\n" ;exit 1);;

let from_equation s = match (String.split_on_char '=' s) with
  | a :: [b] -> ("("^a^")-("^b^")")
  | _ -> (Printf.fprintf stderr "No equal\n" ;exit 1)
;;

let rec get_sum = function
| BinaryNode(l, Add, r) -> List.concat [(get_sum l); (get_sum r)]
| other -> [other]

let rec factors_of f = function
| [] -> Leaf(Const(0.))
| any :: next when (Leaf(Const(1.)) = f && not (has_variable any)) -> BinaryNode(any, Add, (factors_of f next))
| any :: next when any = f -> BinaryNode(Leaf(Const(1.)), Add, (factors_of f next))
| (BinaryNode(l, Multi, r)) :: next when (l = f && not (has_variable r)) -> BinaryNode(r, Add, (factors_of f next))
| (BinaryNode(l, Multi, r)) :: next when (r = f && not (has_variable l)) -> BinaryNode(l, Add, (factors_of f next))
| _ :: next -> factors_of f next;;

let rec tree_degree = function
| [] -> 0.
| BinaryNode(BinaryNode(Leaf(Variable("x")), Exp, Leaf(Const(degree))), Multi, _) :: next -> Float.max degree (tree_degree next)
| BinaryNode(Leaf(Variable("x")), Exp, Leaf(Const(degree))) :: next -> Float.max degree (tree_degree next)
| _ :: next -> tree_degree next
;;

let solve_poly1 (a, b) = 
  if (compare a 0.) = 0 then []
  else [s_to_tree (sprintf "(-%f)/%f" a b)]

let solve_poly2 (a, b, c) = 
  if (compare a 0.) = 0 then solve_poly1 (b, c)
  else let delta = (b *. b -. 4. *. a *. c) in match delta with
    | n when (Float.compare n  0.) > 0 -> print_string "Discriminant is strictly positive, ";
    [(s_to_tree (sprintf "(-%f-(%f^0.5))/(2 * %f)" b delta a)); (s_to_tree(sprintf "(-%f+%f^0.5)/(2 * %f)" b delta a))]
    | n when (Float.compare n  0.) == 0 -> print_string "Discriminant is null, ";[(s_to_tree(sprintf "(-%f)/(2 * %f)" b a)) ]
    | _ -> []

let tree = Sys.argv.(1)
  |> from_equation
  |> lex
  |> stack_from_list
  |> parse
  |> remove_unary
  |> expand
  |> normalize
  |> factorize
  |> group_exp
  |> reduce;;

print_expr tree; print_endline " = 0.";;

let sum = get_sum tree 

let degree = tree_degree sum;;

(print_string "Polynomial degree: "; print_float degree; print_newline ());;

if degree > 2. then (Printf.fprintf stderr "The polynomial degree is strictly greater than 2, I can't solve.\n" ;exit 1);;

let res = solve_poly2 (
  factors_of (BinaryNode(Leaf(Variable("x")), Exp, Leaf(Const(2.)))) sum |> eval_node ,
  factors_of (Leaf(Variable("x"))) sum |> eval_node ,
  factors_of (Leaf(Const(1.))) sum |> eval_node 
  ) in
  match res with
  | [a; b] -> print_endline "the two solutions are:"; print_float (eval_node  a); print_newline ();print_float (eval_node  b); print_newline ();
  | [a] -> print_endline "the solution is:";print_float (eval_node  a); print_newline ();
  | [] -> print_endline "their is no solution";
  | _ -> () (* Unreached *)