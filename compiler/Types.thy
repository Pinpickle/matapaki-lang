theory Types
  imports Main List String "./Ast"
begin

type_synonym type_context = "(String.literal * astType) list"
type_synonym typed_ast = "astType * astExpression"

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

fun either_type_of_either_binary_operator :: "astBinaryOperator * (typed_ast, unit) either * (typed_ast, unit) either \<Rightarrow> (typed_ast, unit) either" where
  "either_type_of_either_binary_operator (operator, Right (t1, ast1), Right (t2, ast2)) = (
    case either_type_of_binary_operator (operator, t1, t2) of
      Right operator_type \<Rightarrow> Right (operator_type, BinaryOperator (operator, ast1, ast2)) |
      Left _ \<Rightarrow> Left ()
  )" |
  "either_type_of_either_binary_operator (_, _, _) = Left ()"

fun type_of_value :: "astValue \<Rightarrow> astType" where
  "type_of_value (Integer _) = TInt" |
  "type_of_value (Bool _) = TBool"

fun either_type_of_expression :: "type_context \<Rightarrow> astExpression => (typed_ast, unit) either" where
  "either_type_of_expression context (BinaryOperator (operator, expression1, expression2)) =
    either_type_of_either_binary_operator (operator, either_type_of_expression context expression1, either_type_of_expression context expression2)" |
  "either_type_of_expression context (Value v) =
    Right (type_of_value v, Value v)" |
  "either_type_of_expression context (LetBinding ((name, assignment), inner_expression)) = (
    case (either_type_of_expression context assignment) of
      Right (let_binding_type, let_value_ast)  \<Rightarrow> (case (type_for_name_in_context context name) of
        None \<Rightarrow> (
          case (either_type_of_expression (Cons (name, let_binding_type) context) inner_expression) of
            Right (let_in_type, let_in_ast) \<Rightarrow> Right (let_in_type, LetBinding ((name, let_value_ast), let_in_ast)) |
            Left _ \<Rightarrow> Left ()
          ) |
        Some _ \<Rightarrow> Left ()
      ) |
      Left _ \<Rightarrow> Left ()
  )" |
  "either_type_of_expression context (Variable name) = (
    case (type_for_name_in_context context name) of
      Some type \<Rightarrow> Right (type, Variable name) |
      None \<Rightarrow> Left ()
  )" |
  "either_type_of_expression context (FunctionApplication (name, argument)) = (
    case (type_for_name_in_context context name) of
      Some (Function (input_type, output_type)) \<Rightarrow> (
        case (either_type_of_expression context argument) of
          Right (argument_type, argument_ast) \<Rightarrow> if (argument_type = input_type) then Right (output_type, FunctionApplication (name, argument_ast))
            else Left () |
          Left _ \<Rightarrow> Left ()
      ) |
      None \<Rightarrow> Left ()
    )
  " |
  "either_type_of_expression context (RecordLiteral values) = (
    let typed_values = List.map_filter (\<lambda>x. x) (List.map (\<lambda>(i, (name, expression)). (
      case (either_type_of_expression context expression) of
      Right (value_type, value_ast) \<Rightarrow> Some (i, name, value_type, value_ast) |
      Left _ \<Rightarrow> None
    )) values) in (
      if size typed_values = size values then
        Right (TRecord (map (\<lambda>(i, name, value_type, _). (i, (name, value_type))) typed_values), RecordLiteral (map (\<lambda>(i, name, _, value_ast). (i, (name, value_ast))) typed_values))
      else
        Left ()
      )
    )" |
  "either_type_of_expression context (RecordAccess (record_expression, name, [])) = (
    case (either_type_of_expression context record_expression) of
      Right (TRecord record_values, record_expression) \<Rightarrow> (
        case List.find (\<lambda>(i, (value_name, value_type)). name = value_name) record_values of
          Some (_, (_, value_type)) \<Rightarrow> Right (value_type, RecordAccess (record_expression, name, map (\<lambda>(i, (value_name, _)). (i, value_name)) record_values)) |
          None \<Rightarrow> Left ()
      ) |
      Left _ \<Rightarrow> Left ()
  )"



(* Whether a type can be an input or output *)
fun is_type_simple :: "astType \<Rightarrow> bool" where
  "is_type_simple TInt = True" |
  "is_type_simple TBool = True" |
  "is_type_simple (TRecord values) = (
    List.list_all (\<lambda>(_, (_, record_type)). record_type = TInt \<or> record_type = TBool) values)" |
  "is_type_simple _ = False"

fun either_type_of_function :: "type_context \<Rightarrow> ast_function_definition \<Rightarrow> (ast_function_definition, unit) either" where
  "either_type_of_function context definition = (
    if ((\<not>(r_exported definition)) \<or> ((is_type_simple (r_argument_type definition)) \<and> (is_type_simple (r_return_type definition)))) then (
      case (either_type_of_expression ((r_argument_name definition, r_argument_type definition) # context) (r_body definition)) of
        Right (body_type, typed_body) \<Rightarrow> (
          if (body_type = (r_return_type definition)) then Right (definition\<lparr>r_body := typed_body\<rparr>)
          else Left ()
        ) |
        Left _ \<Rightarrow> Left ()
      )
    else Left ()
  )
  "

fun context_of_functions :: "ast_function_definition list \<Rightarrow> type_context" where
  "context_of_functions function_definitions = map (\<lambda>def. (r_function_name def, Function (r_argument_type def, r_return_type def))) function_definitions"

fun either_type_of_program :: "ast_program \<Rightarrow> (typed_program, unit) either" where
  "either_type_of_program program = (
    let context = context_of_functions (r_defined_functions program);
        types = map (either_type_of_function context) (r_defined_functions program);
        correct_types = List.map_filter (option_of_either) types in (
      if (size types = size correct_types) then
        Right \<lparr>r_defined_functions = correct_types, r_exported_functions = (filter r_exported  correct_types)\<rparr>
      else Left ()
    )
  )
  "


end
