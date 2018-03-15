theory Types
  imports Main List String "./Ast"
begin

record type_context =
  r_variable_environment :: "(String.literal * astType) list"
  r_is_effect_unwrap :: "bool"
  r_state_type :: astType

type_synonym ast_effect_and_type = "astType * ast_effect set"

type_synonym typed_ast = "ast_effect_and_type * astExpression"

fun type_for_name_in_environment :: "(String.literal * astType) list \<Rightarrow> String.literal \<Rightarrow> astType option" where
  "type_for_name_in_environment [] _ = None" |
  "type_for_name_in_environment (Cons (name, type) rest) required_name = (
    if (name = required_name) then Some type
    else type_for_name_in_environment rest required_name
  )"

fun add_type_to_context :: "type_context \<Rightarrow> String.literal \<Rightarrow> astType \<Rightarrow> type_context" where
  "add_type_to_context context name type = context\<lparr> r_variable_environment := (name, type) # (r_variable_environment context) \<rparr>"

fun type_for_name_in_context :: "type_context \<Rightarrow> String.literal \<Rightarrow> astType option" where
  "type_for_name_in_context context name = type_for_name_in_environment (r_variable_environment context) name"

fun either_type_of_binary_operator :: "astBinaryOperator * astType * astType \<Rightarrow> (astType, unit) either" where
  "either_type_of_binary_operator (Plus, TInt, TInt) = Right TInt" |
  "either_type_of_binary_operator (Minus, TInt, TInt) = Right TInt" |
  "either_type_of_binary_operator (Or, TBool, TBool) = Right TBool" |
  "either_type_of_binary_operator (And, TBool, TBool) = Right TBool" |
  "either_type_of_binary_operator (_, _, _) = Left ()"

fun either_type_of_either_binary_operator :: "astBinaryOperator * (typed_ast, unit) either * (typed_ast, unit) either \<Rightarrow> (typed_ast, unit) either" where
  "either_type_of_either_binary_operator (operator, Right ((t1, ef1), ast1), Right ((t2, ef2), ast2)) = (
    case either_type_of_binary_operator (operator, t1, t2) of
      Right operator_type \<Rightarrow> Right ((operator_type, ef1 \<union> ef2), BinaryOperator (operator, ast1, ast2)) |
      Left _ \<Rightarrow> Left ()
  )" |
  "either_type_of_either_binary_operator (_, _, _) = Left ()"

fun type_of_value :: "astValue \<Rightarrow> astType" where
  "type_of_value (Integer _) = TInt" |
  "type_of_value (Bool _) = TBool" |
  "type_of_value (AddressLiteral _) = TAddress"

fun is_effectful_function_unwrapped :: "type_context \<Rightarrow> astType \<Rightarrow> bool" where
  "is_effectful_function_unwrapped context (TEffect _) = r_is_effect_unwrap context" |
  "is_effectful_function_unwrapped _ _ = True"

fun is_type_key_mapping :: "astType \<Rightarrow> bool" where
  "is_type_key_mapping TInt = True" |
  "is_type_key_mapping TBool = True" |
  "is_type_key_mapping TAddress = True" |
  "is_type_key_mapping _ = False"

fun either_type_of_expression :: "type_context \<Rightarrow> astExpression => (typed_ast, unit) either" where
  "either_type_of_expression context (BinaryOperator (operator, expression1, expression2)) =
    either_type_of_either_binary_operator (operator, either_type_of_expression context expression1, either_type_of_expression context expression2)" |
  "either_type_of_expression context (Value v) =
    Right ((type_of_value v, {}), Value v)" |
  "either_type_of_expression context (LetBinding ((name, assignment), inner_expression)) = (
    case (either_type_of_expression context assignment) of
      Right ((let_binding_type, let_value_effects), let_value_ast)  \<Rightarrow> (case (type_for_name_in_context context name) of
        None \<Rightarrow> (
          case (either_type_of_expression (add_type_to_context context name let_binding_type) inner_expression) of
            Right ((let_in_type, let_in_effects), let_in_ast) \<Rightarrow> Right ((let_in_type, let_value_effects \<union> let_in_effects), LetBinding ((name, let_value_ast), let_in_ast)) |
            Left _ \<Rightarrow> Left ()
          ) |
        Some _ \<Rightarrow> Left ()
      ) |
      Left _ \<Rightarrow> Left ()
  )" |
  "either_type_of_expression context (Variable name) = (
    case (type_for_name_in_context context name) of
      Some type \<Rightarrow> Right ((type, {}), Variable name) |
      None \<Rightarrow> Left ()
  )" |
  "either_type_of_expression context (FunctionApplication (name, argument)) = (
    case (type_for_name_in_context context name) of
      Some (Function (input_type, output_type)) \<Rightarrow> (
        if (is_effectful_function_unwrapped context output_type) then (
          case (either_type_of_expression (context\<lparr> r_is_effect_unwrap := False \<rparr>) argument) of
            Right ((argument_type, argument_effects), argument_ast) \<Rightarrow> if (argument_type = input_type) then Right ((output_type, argument_effects), FunctionApplication (name, argument_ast))
              else Left () |
            Left _ \<Rightarrow> Left ()
        ) else Left ()
      ) |
      Some _ \<Rightarrow> Left () |
      None \<Rightarrow> Left ()
    )
  " |
  "either_type_of_expression context (RecordLiteral values) = (
    let typed_values = List.map_filter (\<lambda>x. x) (List.map (\<lambda>(i, (name, expression)). (
      case (either_type_of_expression context expression) of
      Right ((value_type, value_effects), value_ast) \<Rightarrow> Some (i, name, (value_type, value_effects), value_ast) |
      Left _ \<Rightarrow> None
    )) values) in (
      if size typed_values = size values then
        Right (
          (
            TRecord (map (\<lambda>(i, name, (value_type, _), _). (i, (name, value_type))) typed_values),
            (fold (\<lambda>(_, _, (_, value_effects), _). (\<lambda>all_effects. all_effects \<union> value_effects)) typed_values {})
          ),
          RecordLiteral (map (\<lambda>(i, name, _, value_ast). (i, (name, value_ast))) typed_values)
        )
      else
        Left ()
      )
    )" |
  "either_type_of_expression context (RecordAccess (record_expression, name, _)) = (
    case (either_type_of_expression context record_expression) of
      Right ((TRecord record_values, effects), record_expression) \<Rightarrow> (
        case List.find (\<lambda>(i, (value_name, value_type)). name = value_name) record_values of
          Some (i, (_, value_type)) \<Rightarrow> Right ((value_type, effects), RecordAccess (record_expression, (name, i))) |
          None \<Rightarrow> Left ()
      ) |
      Right _ \<Rightarrow> Left () |
      Left _ \<Rightarrow> Left ()
  )" |
  "either_type_of_expression context (EffectUnwrap expression) = (
    case (either_type_of_expression (context\<lparr> r_is_effect_unwrap := True \<rparr>) expression) of
      Right ((TEffect (wrapped_effects, t), effects), e) \<Rightarrow> (
        if (LocalRead \<notin> wrapped_effects \<and> LocalWrite \<notin> wrapped_effects) then
          Right ((t, effects \<union> wrapped_effects), EffectUnwrap e)
        else Left ()
      ) |
      Right _ \<Rightarrow> Left () |
      Left _ \<Rightarrow> Left ()
    )" |
  "either_type_of_expression context (RecordUpdate (record_expression, _, update_expressions)) = (
    case (either_type_of_expression context record_expression) of
      Right ((TRecord record_values, record_expression_effects), record_expression) \<Rightarrow> (
        let names = List.map (\<lambda>(_, (name, _)). name) update_expressions;
            typed_values = List.map_filter (\<lambda>x. x) (List.map (\<lambda>(_, (name, expression)). (
              case (either_type_of_expression context expression) of
                Right ((update_value_type, update_value_effects), update_value_expression) \<Rightarrow> (
                  case (find (\<lambda>(_, (record_value_name, record_value_type)). record_value_name = name \<and> record_value_type = update_value_type) record_values) of
                    Some (index, _) \<Rightarrow> Some ((index, name), ((update_value_type, update_value_effects), update_value_expression)) |
                    None \<Rightarrow> None
                  ) |
                Left _ \<Rightarrow> None
              )) update_expressions) in
          (if (distinct names \<and> (size typed_values = size update_expressions)) then
            Right (
              (
                TRecord record_values,
                record_expression_effects \<union> (fold (\<lambda>(_, ((_, effects), _)). \<lambda>(all_effects). effects \<union> all_effects) typed_values {})
              ), 
              RecordUpdate (record_expression, size record_values, map (\<lambda>((index, name), (_, update_value_expression)). (index, (name, update_value_expression))) typed_values)
            ) else Left ())
      ) |
      _ \<Rightarrow> Left ()
    )" |
  "either_type_of_expression context (SendEther (address_expression, value_expression)) = (
    let new_context = context\<lparr> r_is_effect_unwrap := False \<rparr> in
    case (either_type_of_expression new_context address_expression, either_type_of_expression new_context value_expression) of
      (Right ((address_expression_type, address_expression_effects), address_expression), Right ((value_expression_type, value_expression_effects), value_expression)) \<Rightarrow>
        if (address_expression_type = TAddress \<and> value_expression_type = TInt \<and> r_is_effect_unwrap context)  then
          Right ((TEffect({ Paying }, TUnit), address_expression_effects \<union> value_expression_effects), SendEther (address_expression, value_expression))
        else Left () |
      _ \<Rightarrow> Left ()
  )" |
  "either_type_of_expression context (IfExpression (condition_expression, true_expression, false_expression)) = (
    case (either_type_of_expression context condition_expression, either_type_of_expression context true_expression, either_type_of_expression context false_expression) of
      (Right ((TBool, condition_expression_effects), condition_expression), Right ((true_expression_type, true_expression_effects), true_expression), Right ((false_expression_type, false_expression_effects), false_expression)) \<Rightarrow> (
        if (true_expression_type = false_expression_type) then
          Right ((true_expression_type, condition_expression_effects \<union> true_expression_effects \<union> false_expression_effects), IfExpression (condition_expression, true_expression, false_expression))
        else
          Left ()
      ) |
      _ \<Rightarrow> Left ()
  )" |
  "either_type_of_expression context SenderExpression = (
    if (r_is_effect_unwrap context) then
      Right ((TEffect ({ ReadEnvironment }, TAddress), {}), SenderExpression)
    else
      Left ()
  )" |
  "either_type_of_expression context (MappingAccess (mapping_expression, key_expression)) = (
    case (either_type_of_expression context mapping_expression, either_type_of_expression context key_expression) of
      (Right ((TMapping (mapping_expression_key_type, mapping_expression_value_type), mapping_expression_effects), mapping_expression), Right ((key_expression_type, key_expression_effects), key_expression)) \<Rightarrow> (
        if ((mapping_expression_key_type = key_expression_type) \<and> (is_type_key_mapping key_expression_type)) then 
          Right ((mapping_expression_value_type, mapping_expression_effects \<union> key_expression_effects), MappingAccess (mapping_expression, key_expression))
        else
          Left ()
      ) |
      _ \<Rightarrow> Left ()
    )" |
  "either_type_of_expression context (NewMapping (key_type, value_type)) = (
    if (is_type_key_mapping key_type) then
      Right ((TMapping (key_type, value_type), {}), NewMapping (key_type, value_type))
    else
      Left ()
  )" |
  "either_type_of_expression context (MappingUpdate (mapping_expression, update_expressions)) = (
    case (either_type_of_expression context mapping_expression) of
      Right ((TMapping (mapping_key_type, mapping_value_type), mapping_expression_effects), mapping_expression) \<Rightarrow> (
        let update_types = List.map_filter (\<lambda>x. x) (
          map (\<lambda>(key_expression, value_expression). case (either_type_of_expression context key_expression, either_type_of_expression context value_expression) of
            (Right ((key_type, key_effects), key_expression), Right ((value_type, value_effects), value_expression)) \<Rightarrow>
              if ((key_type = mapping_key_type) \<and> (value_type = mapping_value_type)) then
                Some (key_effects \<union> value_effects, (key_expression, value_expression))
              else None |
          _ \<Rightarrow> None
        ) update_expressions) in
        if (is_type_key_mapping mapping_key_type) \<and> (size update_types = size update_expressions) then
          Right (
            (
              TMapping (mapping_key_type, mapping_value_type),
              mapping_expression_effects \<union> (fold (\<lambda>(effects, _). \<lambda>all_effects. all_effects \<union> effects) update_types {})
            ),
            MappingUpdate (mapping_expression, map (\<lambda>(_, update_expressions). update_expressions) update_types)
          )
        else Left ()
      ) |
      _ \<Rightarrow> Left ()
    )" | 
  "either_type_of_expression context (RequireExpression (condition_expression, pass_expression)) = (
    case (either_type_of_expression context condition_expression, either_type_of_expression context pass_expression) of
      (Right ((TBool, condition_effects), condition_expression), Right ((pass_type, pass_effects), pass_expression)) \<Rightarrow>
        Right ((pass_type, pass_effects \<union> condition_effects), RequireExpression (condition_expression, pass_expression)) |
      _ \<Rightarrow> Left ()
  )"

(* Whether a type can be an input or output *)
fun is_type_simple :: "astType \<Rightarrow> bool" where
  "is_type_simple TInt = True" |
  "is_type_simple TBool = True" |
  "is_type_simple TAddress = True" |
  "is_type_simple (TRecord values) = (
    List.list_all (\<lambda>(_, (_, record_type)). record_type = TInt \<or> record_type = TBool \<or> record_type = TAddress) values)" |
  "is_type_simple (TEffect (_, type)) = is_type_simple type" |
  "is_type_simple _ = False"

fun do_effects_match_type :: "ast_effect set \<Rightarrow> astType \<Rightarrow> astType  \<Rightarrow> bool" where
  "do_effects_match_type effects type (TEffect (return_type_effects, return_effect_type)) = ((effects \<subseteq> return_type_effects) \<and> (return_effect_type = type))" |
  "do_effects_match_type effects type return_type = ((effects = {}) \<and> (type = return_type))"

fun does_return_type_match_modifier :: "type_context \<Rightarrow> ast_function_modifier \<Rightarrow> astType \<Rightarrow> astType \<Rightarrow> bool" where
  "does_return_type_match_modifier context modifier (TEffect (modifier_effects, modifier_inner_type)) (TEffect (modifiee_effects, modifiee_inner_type)) = (
    (modifiee_effects \<subseteq> modifier_effects) \<and> (does_return_type_match_modifier context modifier (TEffect (modifier_effects, modifier_inner_type)) modifiee_inner_type)
  )" |
  "does_return_type_match_modifier context WithState (TEffect (modifier_effects, modifier_inner_type)) modifiee_type = (
    (modifier_effects \<supseteq> { LocalRead }) \<and> (modifier_inner_type = modifiee_type)
  )" |
  "does_return_type_match_modifier context UpdatingState (TEffect (modifier_effects, modifier_inner_type)) (TRecord (modifiee_record_values)) = (
    (modifier_effects \<supseteq> { LocalRead, LocalWrite }) \<and> (modifiee_record_values = [
      (0, (String.implode ''state'', r_state_type context)),
      (1, (String.implode ''value'', modifier_inner_type))
    ])
  )" |
  "does_return_type_match_modifier _ _ _ _ = False"

fun either_type_of_function :: "type_context \<Rightarrow> ast_function_definition \<Rightarrow> (ast_function_definition, unit) either" where
  "either_type_of_function context definition = (
    if ((\<not>(r_exported definition)) \<or> ((is_type_simple (r_argument_type definition)) \<and> (is_type_simple (r_return_type definition)))) then (
      case (r_body definition) of
        FunctionExpression body_expression \<Rightarrow> (
          case (either_type_of_expression (add_type_to_context context (r_argument_name definition) (r_argument_type definition)) body_expression) of
            Right ((body_type, body_effects), typed_body) \<Rightarrow> (
              if (do_effects_match_type body_effects body_type (r_return_type definition)) then Right (definition\<lparr>r_body := FunctionExpression typed_body\<rparr>)
              else Left ()
            ) |
            Left _ \<Rightarrow> Left ()
          ) |
        FunctionModifier (name, modifier) \<Rightarrow> (
          case ((type_for_name_in_context context name), r_return_type definition) of
            (Some (Function (TRecord argument_record_values, modifiee_return_type)), modifier_return_type) \<Rightarrow>
              if (
                (argument_record_values = [(0, (String.implode ''state'', r_state_type context)), (1, (String.implode ''arg'', r_argument_type definition))]) \<and>
                (does_return_type_match_modifier context modifier modifier_return_type modifiee_return_type)
              ) then Right definition else Left () |
            _ \<Rightarrow> Left ()
        )
      )
    else Left ()
  )
  "

fun is_init_definition_valid :: "type_context \<Rightarrow> ast_function_definition \<Rightarrow> bool" where
  "is_init_definition_valid context init_definition = (
    \<not>(r_exported init_definition) \<and>
    (is_type_simple (r_argument_type init_definition)) \<and>
    (r_state_type context = r_return_type init_definition)
  )"

fun context_of_program :: "ast_program \<Rightarrow> type_context" where
  "context_of_program program = \<lparr>
    r_variable_environment = map (\<lambda>def. (r_function_name def, Function (r_argument_type def, r_return_type def))) 
      (filter (\<lambda>def. (r_function_name def) \<noteq> AST_INIT_NAME) (r_defined_functions program)),
    r_is_effect_unwrap = False,
    r_state_type = r_program_state_type program
  \<rparr>"

fun either_type_of_program :: "ast_program \<Rightarrow> (typed_program, unit) either" where
  "either_type_of_program program = (
    let context = context_of_program (program);
        types = map (either_type_of_function context) (r_defined_functions program); 
        correct_types = List.map_filter (option_of_either) types in (
      case (List.find (\<lambda>def. (r_function_name def) = AST_INIT_NAME) (r_defined_functions program)) of 
        Some init_function_definition \<Rightarrow> (
          if (size types = size correct_types) \<and> (is_init_definition_valid context init_function_definition) then Right \<lparr>
            r_defined_functions = correct_types,
            r_program_state_type = r_program_state_type program,
            r_exported_functions = (filter r_exported  correct_types)
          \<rparr>
          else Left ()
        ) |
        None \<Rightarrow> Left ()
    )
  )
  "


end
