theory Compiler
  imports Main "~~/src/HOL/Library/Code_Target_Int" "./Ast" "./Codegen" "./Types" "~~/src/HOL/Library/Code_Char"
begin                                                                                                     

export_code
    either_type_of_expression
    integers_of_instructions
    instructions_of_program
    either_type_of_program
    Plus
    TInt
    Integer
    int_of_integer
    integer_of_int
    Right
    inst_code
    make_ast_function
    make_ast_program
    r_function_name
    r_body
    r_return_type
    r_argument_type
  in OCaml
  file "src/compiler_theory.ml"

end
