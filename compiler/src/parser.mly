%token LEFT_PAREN
%token RIGHT_PAREN
%token <bool> BOOL
%token <int> INT
%token LET
%token <string> IDENTIFIER
%token EQUALS
%token SEMICOLON
%token EOF
%token <Compiler_theory.Ast.astBinaryOperator> BINARY_OPERATOR

%start <Compiler_theory.Ast.astExpression option> prog
%%

prog: expr = expression EOF { Some expr }

expression:
  | LET; iden = IDENTIFIER; EQUALS; assignment = expression; SEMICOLON; inner = expression
    { Compiler_theory.Ast.LetBinding ((iden, assignment), inner) }
  | e = expression_with_operator
    { e }
  ;

expression_with_operator:
  | e1 = expression_with_operator; operator = BINARY_OPERATOR; e2 = expression_with_value
    { Compiler_theory.Ast.BinaryOperator (operator, (e1, e2)) }
  | e = expression_with_value { e }
  ;

expression_with_value:
  | v = value
    { Compiler_theory.Ast.Value v }
  | LEFT_PAREN; e = expression; RIGHT_PAREN
    { e }
  | iden = IDENTIFIER;
    { Compiler_theory.Ast.Variable iden }
  ;

value:
  | n = INT
    { Compiler_theory.Ast.Integer (Compiler_theory.Arith.Int_of_integer (Big_int.big_int_of_int n)) }
  | b = BOOL
    { Compiler_theory.Ast.Bool b }
  ;
