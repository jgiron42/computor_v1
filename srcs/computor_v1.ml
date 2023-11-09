open Printf
open Parser
open Lexer
open Mathexpr
open Tree

external feclearexcept : int -> unit = "feclearexcept"
external fetestexcept : int -> int = "fetestexcept"

let from_equation s = match (String.split_on_char '=' s) with
  | a :: [b] -> ("("^a^")-("^b^")")
  | _ -> (Printf.fprintf stderr "No equal\n" ;exit 1)

let rec get_sum = function
| BinaryNode(l, Add, r) -> List.concat [(get_sum l); (get_sum r)]
| other -> [other]

let rec factors_of f = function
| [] -> Leaf(Const(0.))
| any :: next when (Leaf(Const(1.)) = f && not (has_variable any)) -> BinaryNode(any, Add, (factors_of f next))
| any :: next when any = f -> BinaryNode(Leaf(Const(1.)), Add, (factors_of f next))
| (BinaryNode(l, Multi, r)) :: next when (l = f && not (has_variable r)) -> BinaryNode(r, Add, (factors_of f next))
| (BinaryNode(l, Multi, r)) :: next when (r = f && not (has_variable l)) -> BinaryNode(l, Add, (factors_of f next))
| _ :: next -> factors_of f next

let rec tree_degree = function
| [] -> 0.
| BinaryNode(BinaryNode(Leaf(Variable("X")), Exp, Leaf(Const(degree))), Multi, _) :: next -> Float.max degree (tree_degree next)
| BinaryNode(Leaf(Variable("X")), Multi, _) :: next -> Float.max 1. (tree_degree next)
| BinaryNode(Leaf(Variable("X")), Exp, Leaf(Const(degree))) :: next -> Float.max degree (tree_degree next)
| _ :: next -> tree_degree next


let rec has_extra_variables = function
| Leaf(Variable(name)) when ((String.compare name "X") != 0) -> true
| BinaryNode(l, _, r) -> ((has_extra_variables l) || (has_extra_variables r))
| UnaryNode(_, n) -> (has_extra_variables n)
| _ -> false



let rec poly_degree ?(n=0.) = function
| [] -> 0.
| 0. :: r -> poly_degree ~n:(n +. 1.) r
| _ :: r -> Float.max n (poly_degree ~n:(n +. 1.) r)


let solve_poly1 (a, b) = 
  if (compare a 0.) = 0 then []
  else [parse (sprintf "(-%f)/%f" b a)]

let solve_poly2 = function
  | (0., b, c) -> solve_poly1 (b, c)
  | (a, b, c) -> ( let delta = (b *. b -. 4. *. a *. c) in match delta with
    | n when (Float.compare n  0.) > 0 -> print_string "Discriminant is strictly positive, ";
    [(parse (sprintf "(-%f-(%f^(1/2)))/(2 * %f)" b delta a)); (parse(sprintf "(-%f+%f^(1/2))/(2 * %f)" b delta a))]
    | n when (Float.compare n  0.) == 0 -> print_string "Discriminant is null, ";[(parse(sprintf "(-%f)/(2 * %f)" b a)) ]
    | n when (Float.compare n  0.) < 0 -> print_string "Discriminant is strictly negative, ";
    [(parse (sprintf "(-%f-i*((-%f)^(1/2)))/(2 * %f)" b delta a)); (parse(sprintf "(-%f+i*(-%f)^(1/2))/(2 * %f)" b delta a))]
    | _ -> [])

let rec pretty_print_poly = function
| (a, b) :: [] -> printf "%g * X^%d = 0\n" a b;
| (a, b) :: r -> printf "%g * X^%d + " a b ; pretty_print_poly r;
| [] -> printf "0 = 0\n"

let do_magic a = a
  |> remove_minus
  |> remove_div
  |> remove_unary
  |> expand
  |> normalize
  |> simplify
  |> factorize
  |> group_exp
  |> simplify
  |> map_until reduce_a
  |> simplify

let zbeub t = print_tree t; t

let pretty_string_of_expr e = do_magic e
  |> put_unary
  |> factorize_out
  |> put_minus
  |> put_div
  |> map_until reduce_a
  |> simplify
  |> factorize_div
  |> string_of_expr

let pretty_print_expr e = print_string (pretty_string_of_expr e)

let print_result e = feclearexcept 61;
	let reduced = pretty_string_of_expr e and solved = (Printf.sprintf "%g" (eval_node e)) in
		if (reduced = solved)
			then (print_string reduced; print_newline ())
			else (print_string reduced; print_string " ≈ "; print_string solved; print_newline ())


let rec print_sum = function
| n :: r -> print_tree n; print_sum r
| [] -> print_newline ()


let () = if ((Array.length Sys.argv) != 2) then (Printf.fprintf stderr "Invalid number of arguments\n" ;exit 1)
		 else let tree = (Sys.argv.(1) |> from_equation |> parse |> do_magic) in
			let sum = get_sum tree in
				if (has_extra_variables tree) then (Printf.fprintf stderr "The expression have other variables than X.\n" ;exit 1)
				else if (tree_degree sum) > 2. then (Printf.fprintf stderr "The polynomial degree is strictly greater than 2, I can't solve.\n" ;exit 1)
				else let poly2 = [|
					factors_of (Leaf(Const(1.))) sum |> eval_node;
					factors_of (Leaf(Variable("X"))) sum |> eval_node;
					factors_of (BinaryNode(Leaf(Variable("X")), Exp, Leaf(Const(2.)))) sum |> eval_node
					|] in
					let degree = poly_degree (Array.to_list poly2) in
						(printf "Reduced form: "; pretty_print_poly (List.filter (function | (0. , _) -> false | _ -> true) [(poly2.(0), 0); (poly2.(1), 1); (poly2.(2), 2)]));
						(print_string "Polynomial degree: "; print_int (Float.to_int degree); print_newline ());
						match tree with Leaf(Const(0.)) -> printf "All real numbers are solutions\n" | _ ->
							solve_poly2 (poly2.(2), poly2.(1), poly2.(0)) |> function
							  | [a; b] -> print_endline "the two solutions are:"; print_result a; print_result b
							  | [a] -> print_endline "the solution is:"; print_result a
							  | [] -> print_endline "their is no solution";
							  | _ -> () (* Unreached *)