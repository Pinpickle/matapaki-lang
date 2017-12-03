theory Ast
  imports Main List
begin

datatype astValue = Integer "int" | Bool "bool"

datatype astBinaryOperator = Plus | Minus | Or | And

datatype astExpression = BinaryOperator "astBinaryOperator * astExpression * astExpression" | Value "astValue"

datatype ('a, 'b) either = Right 'a | Left 'b

end
