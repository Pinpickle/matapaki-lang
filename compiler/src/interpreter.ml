open Compiler_theory

let rec big_step ast =
  print_endline (Parse_ast.stringify_ast ast);
  match Compiler.step(ast) with
    | Compiler.Right Compiler.Value v -> Compiler.Right v
    | Compiler.Right next_ast -> big_step next_ast
    | Compiler.Left error -> Compiler.Left error;;

let stringify_value output =
  match output with
    | Compiler.Right Compiler.Integer Arith.Int_of_integer n -> Big_int.string_of_big_int n
    | Compiler.Right Compiler.Bool b -> string_of_bool b
    | Compiler.Left _ -> "Failure";;

let run ast =
  match Compiler.findType ast with
    | Compiler.Right _ -> print_string (stringify_value (big_step ast))
    | Compiler.Left _ -> print_string "Type error";;
