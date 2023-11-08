
type operation =
| Add
| Sub
| Multi
| Div
| Exp
| Opp
| Invert

type 'a value =
| Const of 'a
| Variable of string

type 'a node =
| Leaf of 'a value
| BinaryNode of ('a node * operation * 'a node)
| UnaryNode of (operation * 'a node)

type 'a token =
| Value of 'a value
| Oper of operation
| BracketOpen
| BracketClose
| End
| Node of 'a node

