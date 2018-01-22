open String
open Compiler_theory

let bytecode_as_string instructions =
  concat "" (List.map (Printf.sprintf "%02x") instructions)

let instructions_to_int_list instructions =
  List.map (fun compilerInt -> Big_int.int_of_big_int (Arith.integer_of_int (compilerInt))) (Codegen.integers_of_instructions instructions)

let compile ast program_type = 
  bytecode_as_string (instructions_to_int_list (Codegen.instructions_of_program ast program_type))

let either_type_of_program program = Types.either_type_of_program program
