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
| BinaryNode(BinaryNode(Leaf(Variable("X")), Exp, Leaf(Const(degree))), Multi, _) :: next -> Float.max degree (tree_degree next)
| BinaryNode(Leaf(Variable("X")), Multi, _) :: next -> Float.max 1. (tree_degree next)
| BinaryNode(Leaf(Variable("X")), Exp, Leaf(Const(degree))) :: next -> Float.max degree (tree_degree next)
| _ :: next -> tree_degree next
;;

let rec has_extra_variables = function
| Leaf(Variable(name)) when ((String.compare name "X") != 0) -> true
| BinaryNode(l, _, r) -> ((has_extra_variables l) || (has_extra_variables r))
| UnaryNode(_, n) -> (has_extra_variables n)
| _ -> false
;;


let rec poly_degree ?(n=0.) = function
| [] -> 0.
| 0. :: r -> poly_degree ~n:(n +. 1.) r
| _ :: r -> Float.max n (poly_degree ~n:(n +. 1.) r)
;;

let solve_poly1 (a, b) = 
  if (compare a 0.) = 0 then []
  else [s_to_tree (sprintf "(-%f)/%f" b a)]

let solve_poly2 = function
  | (0., b, c) -> solve_poly1 (b, c)
  | (a, b, c) -> ( let delta = (b *. b -. 4. *. a *. c) in match delta with
    | n when (Float.compare n  0.) > 0 -> print_string "Discriminant is strictly positive, ";
    [(s_to_tree (sprintf "(-%f-(%f^0.5))/(2 * %f)" b delta a)); (s_to_tree(sprintf "(-%f+%f^0.5)/(2 * %f)" b delta a))]
    | n when (Float.compare n  0.) == 0 -> print_string "Discriminant is null, ";[(s_to_tree(sprintf "(-%f)/(2 * %f)" b a)) ]
    | n when (Float.compare n  0.) < 0 -> print_string "Discriminant is strictly negative, ";
    [(s_to_tree (sprintf "(-%f-i*((-%f)^0.5))/(2 * %f)" b delta a)); (s_to_tree(sprintf "(-%f+i*(-%f)^0.5)/(2 * %f)" b delta a))]
    | _ -> [])

let rec pretty_print_poly = function
| (a, b) :: [] -> printf "%f * X^%d = 0\n" a b;
| (a, b) :: r -> printf "%f * X^%d + " a b ; pretty_print_poly r;
| [] -> printf "0 = 0\n";;

let do_magic a = remove_unary a
  |> expand
  |> normalize
  |> simplify
  |> factorize
  |> group_exp
  |> simplify
  |> reduce;;

let pretty_print_expr e = do_magic e
  |> put_unary
  |> put_minus
  |> factorize_out
  |> put_div
  |> print_expr;;

let tree = Sys.argv.(1)
  |> from_equation
  |> lex
  |> stack_from_list
  |> parse
  |> do_magic;;

let sum = get_sum tree;;

if (has_extra_variables tree) then (Printf.fprintf stderr "The expression have other variables than X.\n" ;exit 1);;

if (tree_degree sum) > 2. then (Printf.fprintf stderr "The polynomial degree is strictly greater than 2, I can't solve.\n" ;exit 1);;

let poly2 = [|
            factors_of (Leaf(Const(1.))) sum |> eval_node;
            factors_of (Leaf(Variable("X"))) sum |> eval_node;
            factors_of (BinaryNode(Leaf(Variable("X")), Exp, Leaf(Const(2.)))) sum |> eval_node
            |];;

let degree = poly_degree (Array.to_list poly2);;

(printf "Reduced form: "; pretty_print_poly (List.filter (function | (0. , _) -> false | _ -> true) [(poly2.(0), 0); (poly2.(1), 1); (poly2.(2), 2)]));
(print_string "Polynomial degree: "; print_float degree; print_newline ());

match tree with Leaf(Const(0.)) -> printf "All real numbers are solutions\n" | _ ->

(*print_float poly2.(2); print_newline ();*)
(*print_float poly2.(1); print_newline ();*)
(*print_float poly2.(0); print_newline ();*)

solve_poly2 (poly2.(2), poly2.(1), poly2.(0)) |> function
  | [a; b] -> print_endline "the two solutions are:"; pretty_print_expr a; print_newline ();pretty_print_expr b; print_newline ();
  | [a] -> print_endline "the solution is:";pretty_print_expr a; print_newline ();
  | [] -> print_endline "their is no solution";
  | _ -> () (* Unreached *)