open Yojson.Basic

let list_of_set set =
  match set with
    | Compiler_theory.Set.Set xs -> xs;;

type state_mutability = Pure | View | NonPayable | Payable

let mutability_of_effect effect =
  match effect with
    | Compiler_theory.Ast.ReadEnvironment -> View
    | Compiler_theory.Ast.Payable ->  Payable
    | Compiler_theory.Ast.Paying -> NonPayable
    | Compiler_theory.Ast.LocalWrite -> NonPayable
    | Compiler_theory.Ast.LocalRead -> View;;

let index_of_state_mutability mutability =
  match mutability with
    | Pure -> 0
    | View -> 1
    | NonPayable -> 2
    | Payable -> 3;;

let mutability_of_type return_type =
  match return_type with
    | Compiler_theory.Ast.TEffect (effects, _) -> 
      let mutabilities = 
        List.sort
          (fun a b -> index_of_state_mutability a - index_of_state_mutability b)
          (List.map (mutability_of_effect) (list_of_set effects))
      in (match mutabilities with
        | mutability :: _ -> mutability
        | [] -> Pure)
    | _ -> Pure;;

let string_of_mutability mutability =
  match mutability with
    | Pure -> "pure"
    | View -> "view"
    | NonPayable -> "nonpayable"
    | Payable -> "payable";;

let payable_of_mutability mutability = mutability = Payable;;

let constant_of_mutability mutability = (mutability = View) || (mutability = Pure);;

let rec type_as_list ast_type name : Yojson.json = 
  match ast_type with
    | Compiler_theory.Ast.TEffect (_, effect_type) -> type_as_list effect_type name
    | Compiler_theory.Ast.TRecord values -> `List (List.map (fun (_, (name, ast_type)) -> `Assoc [
        ("name", `String name);
        ("type", `String (Compiler_theory.Codegen.name_of_type ast_type))
      ]) values);
    | ast_type -> `List [
      `Assoc [
        ("name", `String name);
        ("type", `String (Compiler_theory.Codegen.name_of_type ast_type))
      ]
    ]

let json_abi_of_function (ast_function) : Yojson.json =
  let mutability = mutability_of_type (Compiler_theory.Ast.r_return_type ast_function) in `Assoc [
  ("type", `String "function");
  ("name", `String (Compiler_theory.Ast.r_function_name ast_function));
  ("inputs", type_as_list (Compiler_theory.Ast.r_argument_type ast_function) (Compiler_theory.Ast.r_argument_name ast_function));
  ("outputs", type_as_list (Compiler_theory.Ast.r_return_type ast_function) "");
  ("stateMutability", `String (string_of_mutability mutability));
  ("payable", `Bool (payable_of_mutability mutability));
  ("constant", `Bool (constant_of_mutability mutability))
];;

let constructor_function_abi_of_ast (ast) : Yojson.json = `Assoc [
  ("type", `String "constructor");
  ("inputs", `List []);
  ("stateMutability", `String (string_of_mutability (if Compiler_theory.Ast.r_init_function_payable ast then Payable else NonPayable)));
  ("payable", `Bool (Compiler_theory.Ast.r_init_function_payable ast));
];;

let program_and_type_to_json typed_ast : Yojson.json = `Assoc [
  ("bytecode", `String (Compile.compile typed_ast));
  ("interface", `List (constructor_function_abi_of_ast typed_ast :: List.map json_abi_of_function (Compiler_theory.Ast.r_exported_functions typed_ast)));
]

