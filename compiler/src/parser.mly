%{
  let add_indices_to_list values =
    let rec inner values n =
      match values with
        | [] -> []
        | x::xs -> (n, x) :: inner xs (Compiler_theory.Arith.suc (n))
    in inner values Compiler_theory.Arith.zero_nat;;
  
  let rec add_zeroes_to_list values =
    match values with
      | [] -> []
      | x::xs -> (Compiler_theory.Arith.zero_nat, x) :: add_zeroes_to_list xs
%}

%token LEFT_PAREN
%token RIGHT_PAREN
%token <bool> BOOL
%token <Big_int.big_int> INT
%token TINT
%token TBOOL
%token LET
%token <string> IDENTIFIER
%token EQUALS
%token SEMICOLON
%token EOF
%token FUN
%token COLON
%token ARROW
%token EXPORT
%token RECORD_OPEN
%token RECORD_CLOSE
%token COMMA
%token PERIOD
%token TEFFECT
%token WRITE
%token READ
%token EXCLAMATION
%token WITH_STATE
%token UPDATING_STATE
%token STATE
%token AS
%token WITH
%token AT
%token SEND
%token TO
%token PAYING
%token IF
%token THEN
%token ELSE
%token SENDER
%token READ_ENVIRONMENT
%token SQUARE_OPEN
%token SQUARE_CLOSE
%token REQUIRE
%token <Compiler_theory.Ast.astBinaryOperator> BINARY_OPERATOR

%start <unit Compiler_theory.Ast.ast_program_ext option> prog
%%

prog: state_type = state_declaration; functions = function_list; EOF { Some (Compiler_theory.Ast.make_ast_program functions state_type) }

expression:
  | LET; iden = IDENTIFIER; EQUALS; assignment = expression; SEMICOLON; inner = expression
    { Compiler_theory.Ast.LetBinding ((iden, assignment), inner) }
  | SEND; value_expression = expression; TO; address_expression = expression
    { Compiler_theory.Ast.SendEther (address_expression, value_expression) }
  | IF; condition_expression = expression; THEN; true_expression = expression; ELSE; false_expression = expression
    { Compiler_theory.Ast.IfExpression (condition_expression, (true_expression, false_expression)) }
  | REQUIRE; condition_expression = expression; THEN; pass_expression = expression;
    { Compiler_theory.Ast.RequireExpression (condition_expression, pass_expression) }
  | e = expression_with_operator
    { e }
  ;

function_block:
  | export = EXPORT?; FUN; name = IDENTIFIER; COLON; input_type = type_expr; ARROW; output_type = type_expr; argument_name_and_body = function_body;
    { let (argument_name, body) = argument_name_and_body in
      Compiler_theory.Ast.make_ast_function name argument_name input_type output_type body (not (export = None)) }
  ;

function_body:
  | argument_name = IDENTIFIER; EQUALS; body = expression; { (argument_name, Compiler_theory.Ast.FunctionExpression body) }
  | AS; modifiee_name = IDENTIFIER; modifier = function_modifier { ("arg", Compiler_theory.Ast.FunctionModifier(modifiee_name, modifier)) }
  ;

function_modifier:
  | WITH_STATE { Compiler_theory.Ast.WithState }
  | UPDATING_STATE { Compiler_theory.Ast.UpdatingState }
  ;

state_declaration: STATE; t = type_expr { t }

function_list: functions = function_list_rev { List.rev functions }

function_list_rev:
  | { [] }
  | function_def = function_block; rest = function_list_rev { function_def :: rest }

type_expr:
  | TINT { Compiler_theory.Ast.TInt }
  | TBOOL { Compiler_theory.Ast.TBool }
  | m = mapping_type { Compiler_theory.Ast.TMapping m }
  | r = record_type { r }
  | e = effect_type { e }
  ;

effect_type:
  | TEFFECT; LEFT_PAREN; effects = separated_list(COMMA, effect); RIGHT_PAREN; t = type_expr
    { Compiler_theory.Ast.TEffect ((Compiler_theory.Set.Set effects), t) }
  ;

effect:
  | WRITE { Compiler_theory.Ast.LocalWrite }
  | READ { Compiler_theory.Ast.LocalRead }
  | PAYING { Compiler_theory.Ast.Paying }
  | READ_ENVIRONMENT { Compiler_theory.Ast.ReadEnvironment }
  ;

record_type:
  | RECORD_OPEN; types = separated_list(COMMA, record_type_value); RECORD_CLOSE
    { Compiler_theory.Ast.TRecord (add_indices_to_list types) }
  ;

record_type_value:
  | iden = IDENTIFIER; COLON; value = type_expr { (iden, value) }
  ;

mapping_type:
  | SQUARE_OPEN; key_type = type_expr; ARROW; value_type = type_expr; SQUARE_CLOSE
    { (key_type, value_type) }

record_expression:
  | values = record_literal_list
    { Compiler_theory.Ast.RecordLiteral (add_indices_to_list values) }
  ;

record_literal_list:
  | RECORD_OPEN; values = separated_list(COMMA, record_expression_value); RECORD_CLOSE
    { values }

record_expression_value:
  | iden = IDENTIFIER; EQUALS; assignment = expression; { (iden, assignment) }
  ;

expression_with_operator:
  | e1 = expression_with_operator; operator = BINARY_OPERATOR; e2 = expression_with_value
    { Compiler_theory.Ast.BinaryOperator (operator, (e1, e2)) }
  | e = expression_record_update { e }
  ;

record_access:
  | e = expression_with_value; PERIOD; name = IDENTIFIER;
    { Compiler_theory.Ast.RecordAccess (e, (name, Compiler_theory.Arith.zero_nat)) }
  ;

expression_record_update:
  | e = expression_with_value; WITH; values = record_literal_list
    { Compiler_theory.Ast.RecordUpdate (e, (Compiler_theory.Arith.zero_nat, add_zeroes_to_list values)) }
  | e = expression_with_value { e }
  ;

mapping_update_value:
  | key_expr = expression_with_value; ARROW; value_expr = expression
    { (key_expr, value_expr) }
  ;

expression_with_value:
  | v = value
    { Compiler_theory.Ast.Value v }
  | LEFT_PAREN; e = expression; RIGHT_PAREN
    { e }
  | record = record_expression { record }
  | e = record_access
    { e }
  | mapping_expr = expression_with_value; SQUARE_OPEN; key_expr = expression; SQUARE_CLOSE
    { Compiler_theory.Ast.MappingAccess (mapping_expr, key_expr) }
  | m = mapping_type
    { Compiler_theory.Ast.NewMapping m }
  | mapping_expr = expression_with_value; WITH; SQUARE_OPEN; updates = separated_list(COMMA, mapping_update_value); SQUARE_CLOSE
    { Compiler_theory.Ast.MappingUpdate (mapping_expr, updates) }
  | e = expression_with_value; EXCLAMATION; { Compiler_theory.Ast.EffectUnwrap e }
  | SENDER; { Compiler_theory.Ast.SenderExpression }
  | name = IDENTIFIER; argument = expression;
    { Compiler_theory.Ast.FunctionApplication (name, argument) }
  | iden = IDENTIFIER;
    { Compiler_theory.Ast.Variable iden }
  ;

value:
  | n = INT
    { Compiler_theory.Ast.Integer (Compiler_theory.Arith.Int_of_integer (n)) }
  | b = BOOL
    { Compiler_theory.Ast.Bool b }
  | AT; n = INT
    { Compiler_theory.Ast.AddressLiteral (Compiler_theory.Arith.Int_of_integer (n)) }
  ;
