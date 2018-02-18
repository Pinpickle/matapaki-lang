open Yojson.Basic

let type_as_list ast_type name : Yojson.json = `List (
  match ast_type with
    | Compiler_theory.Ast.TRecord values -> List.map (fun (_, (name, ast_type)) -> `Assoc [
        ("name", `String name);
        ("type", `String (Compiler_theory.Codegen.name_of_type ast_type))
      ]) values;
    | ast_type -> [
      `Assoc [
        ("name", `String name);
        ("type", `String (Compiler_theory.Codegen.name_of_type ast_type))
      ]
    ])

let json_abi_of_function (ast_function) : Yojson.json = `Assoc [
  ("type", `String "function");
  ("name", `String (Compiler_theory.Ast.r_function_name ast_function));
  ("inputs", type_as_list (Compiler_theory.Ast.r_argument_type ast_function) (Compiler_theory.Ast.r_argument_name ast_function));
  ("outputs", type_as_list (Compiler_theory.Ast.r_return_type ast_function) "");
  ("stateMutability", `String "nonpayable");
  ("payable", `Bool false)
]

let program_and_type_to_json typed_ast : Yojson.json = `Assoc [
  ("bytecode", `String (Compile.compile typed_ast));
  ("interface", `List (List.map json_abi_of_function (Compiler_theory.Ast.r_exported_functions typed_ast)))
]

