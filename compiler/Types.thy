theory Types
  imports Main List "./Ast"
begin

datatype astType = TInt | TBool

fun either_type_of_binary_operator :: "astBinaryOperator * astType * astType \<Rightarrow> (astType, unit) either" where
  "either_type_of_binary_operator (Plus, TInt, TInt) = Right TInt" |
  "either_type_of_binary_operator (Minus, TInt, TInt) = Right TInt" |
  "either_type_of_binary_operator (Or, TBool, TBool) = Right TBool" |
  "either_type_of_binary_operator (And, TBool, TBool) = Right TBool" |
  "either_type_of_binary_operator (_, _, _) = Left ()"

fun either_type_of_either_binary_operator :: "astBinaryOperator * (astType, unit) either * (astType, unit) either \<Rightarrow> (astType, unit) either" where
  "either_type_of_either_binary_operator (operator, Right t1, Right t2) = either_type_of_binary_operator (operator, t1, t2)" |
  "either_type_of_either_binary_operator (_, _, _) = Left ()"

fun type_of_value :: "astValue \<Rightarrow> astType" where
  "type_of_value (Integer _) = TInt" |
  "type_of_value (Bool _) = TBool"

fun either_type_of_expression :: "astExpression => (astType, unit) either" where
  "either_type_of_expression (BinaryOperator (operator, expression1, expression2)) =
    either_type_of_either_binary_operator (operator, either_type_of_expression expression1, either_type_of_expression expression2)" |
  "either_type_of_expression (Value v) =
    Right (type_of_value v)"

end
