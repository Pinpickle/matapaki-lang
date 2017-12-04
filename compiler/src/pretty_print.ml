open Compiler_theory

let pretty_print_binary_operator operator =
  match operator with
    | Ast.And -> "&&"
    | Ast.Or -> "||"
    | Ast.Plus -> "+"
    | Ast.Minus -> "-";;

let pretty_print_value value =
  match value with
    | Ast.Integer n -> Big_int.string_of_big_int (Arith.integer_of_int n)
    | Ast.Bool b -> string_of_bool b;;

let rec pretty_print_ast_expression expression =
  match expression with
    | Ast.BinaryOperator (operator, (e1, e2)) -> "(" ^ pretty_print_ast_expression e1 ^ " " ^ pretty_print_binary_operator operator ^ " " ^ pretty_print_ast_expression e2 ^ ")"
    | Ast.Value v -> pretty_print_value v
    | Ast.Variable name -> name
    | Ast.LetBinding ((name, assignment), inner_expression) ->
      "let " ^ name ^ " = " ^ pretty_print_ast_expression assignment ^ ";\n" ^ pretty_print_ast_expression inner_expression;;
