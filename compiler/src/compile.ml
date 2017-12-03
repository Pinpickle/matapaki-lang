open Compiler_theory

let bytecode_as_string instructions =
  String.concat "" (List.map (fun inst -> String.concat "" (List.map (Printf.sprintf "%02x") inst)) instructions)

let instruction_to_int_list instruction =
  List.map (fun compilerInt -> Big_int.int_of_big_int (Arith.integer_of_int (compilerInt))) (Codegen.integers_of_instruction instruction)

let compile ast = 
  bytecode_as_string (List.map instruction_to_int_list (Codegen.instructions_of_expression ast))

let is_typed ast =
  match Types.either_type_of_expression ast with
    | Right _ -> true
    | Left _ -> false;;
