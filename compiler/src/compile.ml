open Compiler_theory

let bytecodeAsString instructions =
  String.concat "" (List.map (fun inst -> String.concat "" (List.map (Printf.sprintf "%02x") inst)) instructions)

let instructionToIntList instruction =
  List.map (fun compilerInt -> Big_int.int_of_big_int (Arith.integer_of_int (compilerInt))) (Compiler.instToInts instruction)

let compile ast = 
  bytecodeAsString (List.map instructionToIntList (Compiler.compileToBytecode ast))

