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
| BinaryNode(l, Plus, r) -> List.concat [(get_sum l); (get_sum r)]
| other -> [other]

let rec factors_of f = function
| [] -> Leaf(Const(0.))
| any :: next when (Leaf(Const(1.)) = f && not (has_variable any)) -> BinaryNode(any, Plus, (factors_of f next))
| any :: next when any = f -> BinaryNode(Leaf(Const(1.)), Plus, (factors_of f next))
| (BinaryNode(l, Times, r)) :: next when (l = f && not (has_variable r)) -> BinaryNode(r, Plus, (factors_of f next))
| (BinaryNode(l, Times, r)) :: next when (r = f && not (has_variable l)) -> BinaryNode(l, Plus, (factors_of f next))
| _ :: next -> factors_of f next

let rec tree_degree = function
| [] -> 0.
| BinaryNode(BinaryNode(Leaf(Variable("X")), Exp, Leaf(Const(degree))), Times, _) :: next -> Float.max degree (tree_degree next)
| BinaryNode(Leaf(Variable("X")), Times, _) :: next -> Float.max 1. (tree_degree next)
| BinaryNode(Leaf(Variable("X")), Exp, Leaf(Const(degree))) :: next -> Float.max degree (tree_degree next)
| _ :: next -> tree_degree next


let rec has_extra_variables = function
| Leaf(Variable(name)) when ((String.compare name "X") != 0) -> true
| BinaryNode(l, _, r) -> ((has_extra_variables l) || (has_extra_variables r))
| UnaryNode(_, n) -> (has_extra_variables n)
| _ -> false



let rec poly_degree ?(n=0.) = function
| [] -> 0.
| Leaf(Const(0.)) :: r -> poly_degree ~n:(n +. 1.) r
| _ :: r -> Float.max n (poly_degree ~n:(n +. 1.) r)


let solve_poly1 (a, b) = 
  if (a = Leaf(Const(0.))) then []
  else [BinaryNode(UnaryNode(Opp, b), Div, a)]

let solve_poly2 = function
  | (Leaf(Const(0.)), b, c) -> solve_poly1 (b, c)
  (*| (a, b, c) -> ( let delta = (b *. b -. 4. *. a *. c) in match delta with*)
  | (a, b, c) -> ( let delta = (BinaryNode(BinaryNode(b, Times, b), Minus, BinaryNode(Leaf(Const(4.)), Times, BinaryNode(a, Times, c)))) in match (eval_node delta) with
    | n when (Float.compare n  0.) > 0 -> print_string "Discriminant is strictly positive, ";
    [BinaryNode(BinaryNode(UnaryNode(Opp, b), Minus, BinaryNode(delta, Exp, BinaryNode(Leaf(Const(1.)), Div, Leaf(Const(2.))))), Div, BinaryNode(Leaf(Const(2.)), Times, a)); 
     BinaryNode(BinaryNode(UnaryNode(Opp, b), Plus, BinaryNode(delta, Exp, BinaryNode(Leaf(Const(1.)), Div, Leaf(Const(2.))))), Div, BinaryNode(Leaf(Const(2.)), Times, a))]
    | n when (Float.compare n  0.) == 0 -> print_string "Discriminant is null, ";
    [BinaryNode(UnaryNode(Opp, b), Div, BinaryNode(Leaf(Const(2.)), Times, a))] 
    | n when (Float.compare n  0.) < 0 -> print_string "Discriminant is strictly negative, ";
    [BinaryNode(BinaryNode(UnaryNode(Opp, b), Minus, BinaryNode(Leaf(Variable("i")), Times, BinaryNode(UnaryNode(Opp, delta), Exp, BinaryNode(Leaf(Const(1.)), Div, Leaf(Const(2.)))))), Div, BinaryNode(Leaf(Const(2.)), Times, a)); 
    BinaryNode(BinaryNode(UnaryNode(Opp, b), Plus, BinaryNode(Leaf(Variable("i")), Times, BinaryNode(UnaryNode(Opp, delta), Exp, BinaryNode(Leaf(Const(1.)), Div, Leaf(Const(2.)))))), Div, BinaryNode(Leaf(Const(2.)), Times, a))]
    | _ -> [])

let print_tree_pipe t = print_tree t; t

let do_magic a = a
  |> remove_minus
  |> remove_div
  |> remove_unary
  |> map_until expand
  |> normalize
  |> simplify
  |> factorize
  |> group_exp
  |> simplify
  |> map_until reduce_a
  |> simplify


let beautify e = do_magic e
  |> put_unary
  |> factorize_out
  |> put_minus
  |> put_div
  |> map_until reduce_a
  |> simplify
  |> factorize_div

let pretty_string_of_expr e = beautify e
  |> string_of_expr

let pretty_print_expr e = print_string (pretty_string_of_expr e)

let print_result e =
        if (has_extra_variables  e)
                then (pretty_print_expr ((map_until reduce_complex_d) e); print_newline ())
                else let reduced = pretty_string_of_expr e and solved = (Printf.sprintf "%g" (eval_node e)) in
        		if (reduced = solved)
        			then (print_string reduced; print_newline ())
        			else let dec = pretty_string_of_expr ((map_until (tree_map_a (reduce_one test_precision_loss))) (beautify e)) in
                                if (dec = solved)
                                then (print_string reduced; print_string "   =   "; print_string solved; print_newline ())
                                else if (reduced = dec)
                                    then (print_string reduced; print_string "   ≈   "; print_string solved; print_newline ())
                                    else (print_string reduced; print_string "   =   "; print_string dec; print_string "   ≈   "; print_string solved; print_newline ())



let rec pretty_print_poly = function
| (a, b) :: [] -> printf "%s * X^%d = 0\n" (pretty_string_of_expr a) b;
| (a, b) :: r -> printf "%s * X^%d + " (pretty_string_of_expr a) b ; pretty_print_poly r;
| [] -> printf "0 = 0\n"



let rec print_sum = function
| n :: r -> print_tree n; print_sum r
| [] -> print_newline ()


let () = if ((Array.length Sys.argv) != 2) then (Printf.fprintf stderr "Invalid number of arguments\n" ;exit 1)
		 else let tree = (Sys.argv.(1) |> from_equation |> parse |> do_magic) in
			let sum = get_sum tree in
				if (has_extra_variables tree) then (Printf.fprintf stderr "The expression have other variables than X.\n" ;exit 1)
				else if (tree_degree sum) > 2. then (Printf.fprintf stderr "The polynomial degree is strictly greater than 2, I can't solve.\n" ;exit 1)
				else let poly2 = [|
					factors_of (Leaf(Const(1.))) sum |> (map_until reduce_a);
					factors_of (Leaf(Variable("X"))) sum |> (map_until reduce_a);
					factors_of (BinaryNode(Leaf(Variable("X")), Exp, Leaf(Const(2.)))) sum |> (map_until reduce_a)
					|] in
					let degree = poly_degree (Array.to_list poly2) in
						(printf "Reduced form: "; pretty_print_poly (List.filter (function | (Leaf(Const(0.)) , _) -> false | _ -> true) [(poly2.(0), 0); (poly2.(1), 1); (poly2.(2), 2)]));
						(print_string "Polynomial degree: "; print_int (Float.to_int degree); print_newline ());
						match tree with Leaf(Const(0.)) -> printf "All real numbers are solutions\n" | _ ->
							solve_poly2 (poly2.(2), poly2.(1), poly2.(0)) |> function
							  | [a; b] -> print_endline "the two solutions are:"; print_result a; print_result b
							  | [a] -> print_endline "the solution is:"; print_result a
							  | [] -> print_endline "their is no solution";
							  | _ -> () (* Unreached *)
