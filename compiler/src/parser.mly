%{
  let add_indices_to_list values =
    let rec inner values n =
      match values with
        | [] -> []
        | x::xs -> (n, x) :: inner xs (Compiler_theory.Arith.suc (n))
    in inner values Compiler_theory.Arith.zero_nat
%}

%token LEFT_PAREN
%token RIGHT_PAREN
%token <bool> BOOL
%token <int> INT
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
%token TUNIT
%token RECORD_OPEN
%token RECORD_CLOSE
%token COMMA
%token PERIOD
%token <Compiler_theory.Ast.astBinaryOperator> BINARY_OPERATOR

%start <unit Compiler_theory.Ast.ast_program_ext option> prog
%%

prog: functions = function_list; EOF { Some (Compiler_theory.Ast.make_ast_program functions) }

expression:
  | LET; iden = IDENTIFIER; EQUALS; assignment = expression; SEMICOLON; inner = expression
    { Compiler_theory.Ast.LetBinding ((iden, assignment), inner) }
  | e = expression_with_operator
    { e }
  ;

function_block:
  | export = EXPORT?; FUN; name = IDENTIFIER; COLON; input_type = type_expr; ARROW; output_type = type_expr; argument_name = IDENTIFIER; EQUALS; body = expression;
    { Compiler_theory.Ast.make_ast_function name argument_name input_type output_type body (not (export = None)) }
  ;

function_list: functions = function_list_rev { List.rev functions }

function_list_rev:
  | { [] }
  | function_def = function_block; rest = function_list_rev { function_def :: rest }

type_expr:
  | TINT { Compiler_theory.Ast.TInt }
  | TBOOL { Compiler_theory.Ast.TBool }
  | r = record_type { r }
  ;

record_type:
  | RECORD_OPEN; types = separated_list(COMMA, record_type_value); RECORD_CLOSE
    { Compiler_theory.Ast.TRecord (add_indices_to_list types) }
  ;

record_type_value:
  | iden = IDENTIFIER; COLON; value = type_expr { (iden, value) }
  ;

record_expression:
  | RECORD_OPEN; values = separated_list(COMMA, record_expression_value); RECORD_CLOSE
    { Compiler_theory.Ast.RecordLiteral (add_indices_to_list values) }
  ;

record_expression_value:
  | iden = IDENTIFIER; EQUALS; assignment = expression; { (iden, assignment) }
  ;

expression_with_operator:
  | e1 = expression_with_operator; operator = BINARY_OPERATOR; e2 = expression_with_value
    { Compiler_theory.Ast.BinaryOperator (operator, (e1, e2)) }
  | e = expression_with_value { e }
  ;

record_access:
  | e = expression_with_value; PERIOD; name = IDENTIFIER; { Compiler_theory.Ast.RecordAccess (e, (name, [])) }

expression_with_value:
  | v = value
    { Compiler_theory.Ast.Value v }
  | LEFT_PAREN; e = expression; RIGHT_PAREN
    { e }
  | record = record_expression { record }
  | e = record_access
    { e }
  | name = IDENTIFIER; argument = expression;
    { Compiler_theory.Ast.FunctionApplication (name, argument) }
  | iden = IDENTIFIER;
    { Compiler_theory.Ast.Variable iden }
  ;

value:
  | n = INT
    { Compiler_theory.Ast.Integer (Compiler_theory.Arith.Int_of_integer (Big_int.big_int_of_int n)) }
  | b = BOOL
    { Compiler_theory.Ast.Bool b }
  ;
