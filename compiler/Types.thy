theory Types
  imports Main List String "./Ast"
begin

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
  )" |
  "either_type_of_expression context (FunctionApplication (name, argument)) = (
    case (type_for_name_in_context context name) of
      Some (Function (input_type, output_type)) \<Rightarrow> (
        if (Right input_type = either_type_of_expression context argument) then Right output_type
        else Left ()
      ) |
      None \<Rightarrow> Left ()
    )
  "

fun either_type_of_function :: "type_context \<Rightarrow> ast_function_definition \<Rightarrow> (astType, unit) either" where
  "either_type_of_function context definition = (
    case (either_type_of_expression ((r_argument_name definition, r_argument_type definition) # context) (r_body definition)) of
      Right body_type \<Rightarrow> (
        if (body_type = (r_return_type definition)) then Right (r_return_type definition)
        else Left ()
      ) |
      Left _ \<Rightarrow> Left ()
    )
  "

fun context_of_functions :: "ast_function_definition list \<Rightarrow> type_context" where
  "context_of_functions function_definitions = map (\<lambda>def. (r_function_name def, Function (r_argument_type def, r_return_type def))) function_definitions"

fun either_type_of_program :: "ast_program \<Rightarrow> (astType, unit) either" where
  "either_type_of_program program = (
    let context = context_of_functions (r_defined_functions program) in (
      if (find (\<lambda>fn. either_type_of_function context fn = Left ()) (r_defined_functions program)) = None then
        either_type_of_expression context (r_main_expression program)
      else Left ()
    )
  )
  "


end
