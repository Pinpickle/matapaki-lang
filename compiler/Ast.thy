theory Ast
  imports Main List
begin

datatype astValue = Integer "int" | Bool "bool"

datatype astBinaryOperator = Plus | Minus | Or | And

datatype astType = TInt | TBool | Function "astType * astType" | TUnit

datatype astExpression
  = BinaryOperator "astBinaryOperator * astExpression * astExpression"
  | Value "astValue"
  | LetBinding "(String.literal * astExpression) * astExpression"
  | Variable "String.literal"
  | FunctionApplication "String.literal * astExpression"
  | UnitLiteral

record ast_function_definition =
  r_function_name :: String.literal
  r_argument_name :: String.literal
  r_argument_type :: astType
  r_return_type :: astType
  r_body :: astExpression
  r_exported :: bool

record ast_program =
  r_defined_functions :: "ast_function_definition list"

definition "make_ast_function = ast_function_definition.make"
definition "make_ast_program = ast_program.make"

datatype ('a, 'b) either = Right 'a | Left 'b

fun option_of_either :: "('a, 'b) either \<Rightarrow> 'a option" where
  "option_of_either (Right val) = Some val" |
  "option_of_either (Left _) = None"

end
