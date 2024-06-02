Computorv1 is a small 42 project whose goal is simply to solve an equation of the 2nd degree.

For the subject, we can pick any language. I decided to use OCaml and **chose to use only tree rewriting to solve the equations**.

The program uses ocamllex and ocamlyacc to parse the equation (in any form) and prints the results in fractional form, in decimal form (without float precision loss), and if needed, in decimal form with float precision loss.
