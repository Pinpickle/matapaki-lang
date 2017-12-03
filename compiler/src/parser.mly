%token LEFT_PAREN
%token RIGHT_PAREN
%token <bool> BOOL
%token <int> INT
%token EOF
%token <Compiler_theory.Ast.astBinaryOperator> BINARY_OPERATOR

%start <Compiler_theory.Ast.astExpression option> prog
%%

prog: expr = expression EOF { Some expr }

expression:
  | e1 = expression; operator = BINARY_OPERATOR; e2 = sub_expression
    { Compiler_theory.Ast.BinaryOperator (operator, (e1, e2)) }
  | e = sub_expression
    { e }
  ;

sub_expression:
  | v = value
    { Compiler_theory.Ast.Value v }
  | LEFT_PAREN; e = expression; RIGHT_PAREN
    { e }
  ;

value:
  | n = INT
    { Compiler_theory.Ast.Integer (Compiler_theory.Arith.Int_of_integer (Big_int.big_int_of_int n)) }
  | b = BOOL
    { Compiler_theory.Ast.Bool b }
  ;
