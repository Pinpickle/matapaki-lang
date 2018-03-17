open Compiler_theory

let pretty_print_binary_operator operator =
  match operator with
    | Ast.And -> "&&"
    | Ast.Or -> "||"
    | Ast.Plus -> "+"
    | Ast.Minus -> "-"
    | Ast.Multiply -> "*"
    | Ast.Divide -> "/"
    | Ast.Mod -> "%"
    | Ast.Equal -> "="
    | Ast.Greater -> ">"
    | Ast.GreaterEqual -> ">="
    | Ast.Lesser -> "<"
    | Ast.LesserEqual -> "<=";;

let pretty_print_value value =
  match value with
    | Ast.Integer n -> Big_int.string_of_big_int (Arith.integer_of_int n)
    | Ast.Bool b -> string_of_bool b
    | Ast.AddressLiteral n -> "@" ^ Big_int.string_of_big_int (Arith.integer_of_int n);;

let pretty_print_ast_effect e =
  match e with
    | Ast.LocalRead -> "Read"
    | Ast.LocalWrite -> "Write"
    | Ast.Paying -> "Paying"
    | Ast.ReadEnvironment -> "ReadEnv";;

let rec list_of_set set =
  match set with
    | Set.Set xs -> xs;;

let rec pretty_print_ast_type ast_type =
  match ast_type with
    | Ast.TInt -> "Int"
    | Ast.TBool -> "Bool"
    | Ast.TRecord types -> "{" ^
      String.concat ",\n" (List.map (fun (_, (name, name_type)) -> name ^ " : " ^ pretty_print_ast_type name_type) types)  ^ "}\n"
    | Ast.TEffect (es, t) ->
      "Effect (" ^ String.concat "," (List.map pretty_print_ast_effect (list_of_set es)) ^ ") " ^ (pretty_print_ast_type t);
    | Ast.Function (input, output) -> pretty_print_ast_type input ^ " -> " ^ pretty_print_ast_type output
    | Ast.TAddress -> "Address"
    | Ast.TMapping (key_type, value_type) -> "[" ^ pretty_print_ast_type key_type ^ " -> " ^ pretty_print_ast_type value_type ^ "]";;

let rec pretty_print_record_values values =
  "{" ^ String.concat ",\n" (List.map (fun (_, (name, expression)) -> name ^ " = " ^ pretty_print_ast_expression expression) values) ^ "}"
and pretty_print_ast_expression expression =
  match expression with
    | Ast.BinaryOperator (operator, (e1, e2)) -> "(" ^ pretty_print_ast_expression e1 ^ " " ^ pretty_print_binary_operator operator ^ " " ^ pretty_print_ast_expression e2 ^ ")"
    | Ast.Value v -> pretty_print_value v
    | Ast.Variable name -> name
    | Ast.LetBinding ((name, assignment), inner_expression) ->
      "let " ^ name ^ " = " ^ pretty_print_ast_expression assignment ^ ";\n" ^ pretty_print_ast_expression inner_expression
    | Ast.FunctionApplication (name, argument) -> name ^ " (" ^ (pretty_print_ast_expression argument) ^ ")"
    | Ast.RecordLiteral (values) -> pretty_print_record_values values
    | Ast.RecordAccess (expression, (name, _)) -> "(" ^ pretty_print_ast_expression expression ^ ")." ^ name
    | Ast.EffectUnwrap (expression) -> "(" ^ pretty_print_ast_expression expression ^ ")!"
    | Ast.RecordUpdate (expression, (_, values)) -> "(" ^ pretty_print_ast_expression expression ^ ") with " ^ pretty_print_record_values values
    | Ast.SendEther (address_expression, value_expression) -> "(send " ^ pretty_print_ast_expression value_expression ^ " to " ^ pretty_print_ast_expression address_expression ^ ")"
    | Ast.IfExpression (condition_expression, (true_expression, false_expression)) ->
        "if " ^ pretty_print_ast_expression condition_expression ^ " then \n  " ^ pretty_print_ast_expression true_expression ^ "\nelse\n  " ^ pretty_print_ast_expression false_expression
    | Ast.SenderExpression -> "sender"
    | Ast.NewMapping (key_type, value_type) -> pretty_print_ast_type (Ast.TMapping (key_type, value_type))
    | Ast.MappingAccess (mapping_expression, key_expression) -> "(" ^ pretty_print_ast_expression mapping_expression ^ ")[" ^ pretty_print_ast_expression key_expression ^ "]"
    | Ast.MappingUpdate (mapping_expression, entries_expressions) -> "((" ^ pretty_print_ast_expression mapping_expression ^ ") with [" ^ 
      (String.concat "\n" (List.map (fun (key_expression, value_expression) -> "(" ^ pretty_print_ast_expression key_expression ^ ") -> (" ^ pretty_print_ast_expression value_expression ^ ")") entries_expressions))
    | Ast.RequireExpression (condition_expression, pass_expression) -> "require (" ^ pretty_print_ast_expression condition_expression ^ ") then (" ^ pretty_print_ast_expression pass_expression ^ ")";;

let pretty_print_ast_modifier modifier =
  match modifier with
    | Ast.WithState -> "with_state"
    | Ast.UpdatingState -> "updating_state"

let pretty_print_ast_function function_block =
  (if Ast.r_exported function_block then "export " else "") ^ "fun " ^ Ast.r_function_name function_block ^ ": " ^
  pretty_print_ast_type (Ast.r_argument_type function_block) ^ " -> " ^
  pretty_print_ast_type (Ast.r_return_type function_block) ^ " \n  " ^ (match Ast.r_body function_block with
    | Ast.FunctionExpression expression -> Ast.r_argument_name function_block ^ " = " ^ pretty_print_ast_expression expression
    | Ast.FunctionModifier (modifiee, modifier) -> "as " ^ modifiee ^ " " ^ pretty_print_ast_modifier modifier)
  ;;

let pretty_print_ast_state state_type =
  "State " ^ pretty_print_ast_type state_type;;

let pretty_print_ast_program program =
  (String.concat "\n\n" ([pretty_print_ast_state (Ast.r_program_state_type program)] @ (List.map pretty_print_ast_function (Ast.r_defined_functions program))))