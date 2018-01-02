theory Ast
  imports Main List
begin

datatype astValue = Integer "int" | Bool "bool"

datatype astBinaryOperator = Plus | Minus | Or | And

datatype astType = TInt | TBool | Function "astType *  astType"

datatype astExpression
  = BinaryOperator "astBinaryOperator * astExpression * astExpression"
  | Value "astValue"
  | LetBinding "(String.literal * astExpression) * astExpression"
  | Variable "String.literal"
  | FunctionApplication "String.literal * astExpression"

record ast_function_definition =
  r_function_name :: String.literal
  r_argument_name :: String.literal
  r_argument_type :: astType
  r_return_type :: astType
  r_body :: astExpression

record ast_program =
  r_defined_functions :: "ast_function_definition list"
  r_main_expression :: astExpression

definition "make_ast_function = ast_function_definition.make"
definition "make_ast_program = ast_program.make"

datatype ('a, 'b) either = Right 'a | Left 'b

end
