open String
open Compiler_theory

let bytecode_as_string instructions =
  concat "" (List.map (Printf.sprintf "%02x") instructions)

let instructions_to_int_list instructions =
  List.map (fun compilerInt -> Big_int.int_of_big_int (Arith.integer_of_int (compilerInt))) (Codegen.integers_of_instructions instructions)

let compile ast = 
  bytecode_as_string (instructions_to_int_list (Codegen.instructions_of_program ast))

let is_typed program =
  match Types.either_type_of_program program with
    | Right _ -> true
    | Left _ -> false;;
