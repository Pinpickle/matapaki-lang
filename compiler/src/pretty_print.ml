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
      "let " ^ name ^ " = " ^ pretty_print_ast_expression assignment ^ ";\n" ^ pretty_print_ast_expression inner_expression
    | Ast.FunctionApplication (name, argument) -> name ^ " (" ^ (pretty_print_ast_expression argument) ^ ")"
    | Ast.UnitLiteral -> "(| |)";;

let rec pretty_print_ast_type ast_type =
  match ast_type with
    | Ast.TInt -> "Int"
    | Ast.TBool -> "Bool"
    | Ast.TUnit -> "Unit"
    | Ast.Function (input, output) -> pretty_print_ast_type input ^ " -> " ^ pretty_print_ast_type output;;

let pretty_print_ast_function function_block =
  (if Ast.r_exported function_block then "export " else "") ^ "fun " ^ Ast.r_function_name function_block ^ ": " ^
  pretty_print_ast_type (Ast.r_argument_type function_block) ^ " -> " ^
  pretty_print_ast_type (Ast.r_return_type function_block) ^ " \n  " ^
  Ast.r_argument_name function_block ^ " = " ^ pretty_print_ast_expression (Ast.r_body function_block);;

let pretty_print_ast_program program =
  (String.concat "\n\n" (List.map pretty_print_ast_function (Ast.r_defined_functions program)))