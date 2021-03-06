theory Ast
  imports Main List
begin

datatype astValue =
  UInteger "nat" |
  AddressLiteral "nat" |
  Bool "bool"

datatype astBinaryOperator
  = Plus
  | Minus
  | Or
  | And 
  | Greater 
  | Lesser 
  | GreaterEqual
  | LesserEqual
  | Equal
  | Multiply
  | Divide
  | Mod

datatype ast_effect = LocalRead | LocalWrite | Paying | ReadEnvironment | Payable

datatype astType
  = TUint
  | TBool
  | Function "astType * astType"
  | TRecord "(nat * (String.literal * astType)) list"
  | TEffect "(ast_effect set) * astType"
  | TAddress
  | TMapping "astType * astType"

definition "TUnit = TRecord []"

datatype astExpression
  = BinaryOperator "astBinaryOperator * astExpression * astExpression"
  | Value "astValue"
  | LetBinding "(String.literal * astExpression) * astExpression"
  | Variable "String.literal"
  | FunctionApplication "String.literal * astExpression"
  | RecordLiteral "(nat * (String.literal * astExpression)) list"
  (* Record expression, (value name, value index) *)
  | RecordAccess "(astExpression * (String.literal * nat))"
  | EffectUnwrap "astExpression"
  | RecordUpdate "astExpression * nat * (nat * (String.literal * astExpression)) list"
  | SendEther "astExpression * astExpression"
  | IfExpression "astExpression * astExpression * astExpression"
  | SenderExpression
  | BalanceExpression
  | ValueExpression
  | AddressExpression
  | RequireExpression "astExpression * astExpression"
  | NewMapping "astType * astType"
  | MappingAccess "astExpression * astExpression"
  | MappingUpdate "astExpression * (astExpression * astExpression) list"

datatype ast_function_modifier = WithState | UpdatingState

datatype ast_function_body
  = FunctionExpression astExpression
  | FunctionModifier "String.literal * ast_function_modifier"

record ast_function_definition =
  r_function_name :: String.literal
  r_argument_name :: String.literal
  r_argument_type :: astType
  r_return_type :: astType
  r_body :: ast_function_body
  r_exported :: bool

record ast_program =
  r_defined_functions :: "ast_function_definition list"
  r_program_state_type :: "astType"

record typed_program = ast_program +
  r_exported_functions :: "ast_function_definition list"
  r_init_function_payable :: bool

definition "make_ast_function = ast_function_definition.make"
definition "make_ast_program = ast_program.make"
definition "AST_INIT_NAME = String.implode ''init''"


datatype ('a, 'b) either = Right 'a | Left 'b

fun option_of_either :: "('a, 'b) either \<Rightarrow> 'a option" where
  "option_of_either (Right val) = Some val" |
  "option_of_either (Left _) = None"

definition set_of_list :: "'a list \<Rightarrow> 'a set" where
  "set_of_list xs = set xs"

fun type_is_payable :: "astType \<Rightarrow> bool" where
  "type_is_payable ( TEffect (effects, _)) = (Payable \<in> effects)" |
  "type_is_payable _ = False"

end
