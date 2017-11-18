open Compiler_theory

type expression =
  | Plus of expression * expression
  | Or of expression * expression
  | Integer of int
  | Bool of bool;;

let rec compiler_ast_of_expression expression =
  match expression with
    | Plus (e1, e2) -> Compiler.Plus (compiler_ast_of_expression e1, compiler_ast_of_expression e2)
    | Or (e1, e2) -> Compiler.Or (compiler_ast_of_expression e1, compiler_ast_of_expression e2)
    | Integer n -> Compiler.Value (
        Compiler.Integer (Arith.Int_of_integer (Big_int.big_int_of_int n))
      )
    | Bool b -> Compiler.Value (Compiler.Bool b);;

let rec stringify_ast expression =
  match expression with
    | Compiler.Plus (e1, e2) -> "(" ^ stringify_ast e1 ^ "+" ^ stringify_ast e2 ^ ")"
    | Compiler.Or (e1, e2) -> "(" ^ stringify_ast e1 ^ "|" ^ stringify_ast e2 ^ ")"
    | Compiler.Value (Compiler.Integer n) -> Big_int.string_of_big_int (Arith.integer_of_int n)
    | Compiler.Value (Compiler.Bool b) -> string_of_bool b;;
