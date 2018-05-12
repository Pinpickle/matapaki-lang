theory Compiler
  imports Main "~~/src/HOL/Library/Code_Target_Numeral" "./Ast" "./Codegen" "./Types" "~~/src/HOL/Library/Code_Char" "./eth-isabelle/lem/KeccakAuxiliary"
begin                                                                                                     

code_reserved OCaml String
code_reserved OCaml List

code_printing constant keccak_of_string \<rightharpoonup> (OCaml) "Computils.keccakofstring" 

export_code
    either_type_of_expression
    integers_of_instructions
    instructions_of_program
    either_type_of_program
    name_of_type
    Plus
    TUint
    WithState
    FunctionExpression
    UInteger
    LocalRead
    int_of_integer
    integer_of_int
    integer_of_nat
    nat_of_integer
    set_of_list
    Right
    inst_code
    make_ast_function
    make_ast_program
    r_function_name
    r_body
    r_return_type
    r_argument_type
    r_exported_functions
    r_init_function_payable
  in OCaml
  file "compiler_theory.ml"

end
