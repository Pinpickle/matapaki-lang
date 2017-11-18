%token LEFT_PAREN
%token RIGHT_PAREN
%token TRUE
%token FALSE
%token <int> INT
%token PLUS
%token OR
%token EOF

%start <Parse_ast.expression option> prog
%%

prog: expr = expression EOF { Some expr }

expression:
  | e1 = expression; PLUS; e2 = value
    { Parse_ast.Plus (e1, e2) }
  | e1 = expression; OR; e2 = value
    { Parse_ast.Or (e1, e2) }
  | v = value
    { v }
  ;

value:
  | n = INT
    { Parse_ast.Integer n }
  | TRUE
    { Parse_ast.Bool true }
  | FALSE
    { Parse_ast.Bool false }
  | LEFT_PAREN; e = expression; RIGHT_PAREN
    { e }
  ;
