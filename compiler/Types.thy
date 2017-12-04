theory Types
  imports Main List String "./Ast"
begin

datatype astType = TInt | TBool
type_synonym type_context = "(String.literal * astType) list"

fun type_for_name_in_context :: "type_context \<Rightarrow> String.literal \<Rightarrow> astType option" where
  "type_for_name_in_context [] _ = None" |
  "type_for_name_in_context (Cons (name, type) rest) required_name = (
    if (name = required_name) then Some type
    else type_for_name_in_context rest required_name
  )"

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

fun either_type_of_expression :: "type_context \<Rightarrow> astExpression => (astType, unit) either" where
  "either_type_of_expression context (BinaryOperator (operator, expression1, expression2)) =
    either_type_of_either_binary_operator (operator, either_type_of_expression context expression1, either_type_of_expression context expression2)" |
  "either_type_of_expression context (Value v) =
    Right (type_of_value v)" |
  "either_type_of_expression context (LetBinding ((name, assignment), inner_expression)) = (
    case (either_type_of_expression context assignment) of
      Right let_binding_type \<Rightarrow> (case (type_for_name_in_context context name) of
        None \<Rightarrow> (either_type_of_expression (Cons (name, let_binding_type) context) inner_expression) |
        Some _ \<Rightarrow> Left ()
      ) |
      Left _ \<Rightarrow> Left ()
  )" |
  "either_type_of_expression context (Variable name) = (
    case (type_for_name_in_context context name) of
      Some type \<Rightarrow> Right type |
      None \<Rightarrow> Left ()
  )"

end
