theory Codegen
  imports Main List "./Ast" "~~/src/HOL/Library/Code_Target_Int" "./eth-isabelle/ContractSem" "./eth-isabelle/RelationalSem" "./eth-isabelle/ProgramInAvl"
begin

type_synonym variable_location_list = "(String.literal  * nat) list"
record program_function = ast_function_definition +
  r_instructions :: "inst list"
record codegen_context =
  r_variable_locations :: variable_location_list
  r_program_functions :: "program_function list"

definition NULL_POINTER_ADDRESS :: "nat" where 
  "NULL_POINTER_ADDRESS = 0"

definition RE_ENTRANCY_FLAG_ADDRESS :: "nat" where 
  "RE_ENTRANCY_FLAG_ADDRESS = 32"

definition NEXT_FREE_MEMORY_ADDRESS :: "nat" where 
  "NEXT_FREE_MEMORY_ADDRESS = 64"

definition FRAME_POINTER_ADDRESS :: "nat" where 
  "FRAME_POINTER_ADDRESS = 96"

definition INITIAL_INSTRUCTIONS_LENGTH :: "nat" where
  "INITIAL_INSTRUCTIONS_LENGTH = 34"

(*
  Layout of memory during execution:
  
  0: Null pointer
  1: Re-entrancy flag
  2: Next available memory address (initialised to 4) 
  3: Frame pointer
*)

fun number_to_words :: "int \<Rightarrow> 8 word list" where
  "number_to_words i = (word_rsplit (word_of_int i::256 word))"

fun offset_for_name_in_context :: "variable_location_list \<Rightarrow> String.literal \<Rightarrow> nat" where
  "offset_for_name_in_context [] _ = 0" |
  "offset_for_name_in_context (Cons (name, location) rest) required_name = (
    if (name = required_name) then location
    else offset_for_name_in_context rest required_name
  )"

fun bytes_of_instructions :: "inst list \<Rightarrow> byte list" where
  "bytes_of_instructions insts = List.concat (map inst_code insts)"

fun location_of_function_name_in_functions :: "nat \<Rightarrow> program_function list \<Rightarrow> String.literal \<Rightarrow> nat" where
  "location_of_function_name_in_functions offset [] _ = offset" |
  "location_of_function_name_in_functions offset (checking_function # rest) name = (
    if (r_function_name checking_function = name) then offset else
    location_of_function_name_in_functions (offset + size (bytes_of_instructions (r_instructions checking_function))) rest name
  )"

value "(implode ''oo'')::String.literal"
value "location_of_function_name_in_functions 0 [
  \<lparr>r_function_name = (implode ''aao'')::String.literal, r_argument_name = (implode ''doo'')::String.literal, r_argument_type = TInt, r_return_type = TInt, r_body = Value (Integer 5), r_instructions = [Push_N [5]]\<rparr>,
  \<lparr>r_function_name = (implode ''foo'')::String.literal, r_argument_name = (implode ''doo'')::String.literal, r_argument_type = TInt, r_return_type = TInt, r_body = Value (Integer 5), r_instructions = [Push_N [5]]\<rparr>
] (implode ''foo'')"

fun location_of_function_name_in_context :: "codegen_context \<Rightarrow> String.literal \<Rightarrow> nat" where
  "location_of_function_name_in_context context name = location_of_function_name_in_functions INITIAL_INSTRUCTIONS_LENGTH (r_program_functions context) name"

fun bytes_of_value :: "astValue \<Rightarrow> 8 word list" where
  "bytes_of_value (Integer i) = number_to_words i" |
  "bytes_of_value (Bool b) = (if b then [1] else [0])"

fun instructions_of_binary_operator :: "astBinaryOperator \<Rightarrow> inst list" where
  "instructions_of_binary_operator Plus = [Arith ADD]" |
  "instructions_of_binary_operator Minus = [Arith SUB]" |
  "instructions_of_binary_operator Or = [Bits inst_OR]" |
  "instructions_of_binary_operator And = [Bits inst_AND]"

fun count_max_let_binding :: "astExpression \<Rightarrow> nat" where
  "count_max_let_binding (BinaryOperator (_, e1, e2)) = max (count_max_let_binding e1) (count_max_let_binding e2)" |
  "count_max_let_binding (Value _) = 0" |
  "count_max_let_binding (Variable _) = 0" |
  "count_max_let_binding (LetBinding ((name, assignment), inner_expression)) = 1 + max (count_max_let_binding assignment) (count_max_let_binding inner_expression)" |
  "count_max_let_binding (FunctionApplication (_, argument)) = count_max_let_binding argument"

fun place_offset_from_stored_address :: "nat \<Rightarrow> nat \<Rightarrow> inst list" where
  "place_offset_from_stored_address address offset = [
    Stack (PUSH_N (number_to_words address)),
    Memory MLOAD,
    Stack (PUSH_N (number_to_words (offset))),
    Arith ADD
  ]"

definition place_offset_from_frame_pointer :: "nat \<Rightarrow> inst list" where
  "place_offset_from_frame_pointer = place_offset_from_stored_address FRAME_POINTER_ADDRESS"

fun instructions_of_expression :: "codegen_context \<Rightarrow> astExpression => inst list" where
  "instructions_of_expression context (BinaryOperator (operator, e1, e2)) =
    (instructions_of_expression context e1) @ (instructions_of_expression context e2) @ instructions_of_binary_operator operator" |
  "instructions_of_expression _ (Value v) = [Stack (PUSH_N (bytes_of_value v))]" |
  "instructions_of_expression context (LetBinding ((name, assignment), inner_expression)) = (
    let variable_offset = (max (count_max_let_binding assignment) (count_max_let_binding inner_expression)) * 32 in (
      (* Place value on top of stack *)
      (instructions_of_expression context assignment) @
      (* Place address on top of stack *)
      (place_offset_from_frame_pointer variable_offset) @
      (* Save the value to the address *)
      [Memory MSTORE] @
      (* Continue with execution *)
      (instructions_of_expression 
        (context\<lparr>r_variable_locations := ((name, variable_offset) # (r_variable_locations context))\<rparr>) 
        inner_expression
      ) 
    )
  )" |
  "instructions_of_expression context (Variable name) = (
    (* Place address on top of stack *)
    (place_offset_from_frame_pointer (offset_for_name_in_context (r_variable_locations context) name)) @
    (* Load from address *)
    [Memory MLOAD]
  )" |
  "instructions_of_expression context (FunctionApplication (name, argument)) =  
    (* Stack: argument, ... *)
    instructions_of_expression context argument @
    (* Stack: frame pointer, argument, ... *)
    place_offset_from_frame_pointer 0 @ [
      (* Stack: argument, frame pointer, ... *)
      Swap 0,
      (* Update frame pointer to be next free memory address *)
      Stack (PUSH_N (number_to_words (NEXT_FREE_MEMORY_ADDRESS))),
      Stack (PUSH_N (number_to_words (FRAME_POINTER_ADDRESS))),
      Memory MLOAD,
      Memory MSTORE,
      (* Stack: function address, argument, frame pointer, ... *)
      Stack (PUSH_N [word_of_int (location_of_function_name_in_context context name)]),
      (* Stack: program counter, function address, argument, frame pointer, ... *)
      Pc PC,
      Stack (PUSH_N [7]),
      (* Stack: return address, function address, argument, frame pointer, ... *)
      Arith ADD,
      (* Stack: argument, function address, return address, frame pointer, ... *)
      Swap 1,
      (* Stack: function address, argument, return address, frame pointer, ... *)
      Swap 0,
      Pc JUMP,
      (* Stack: return value, frame pointer, *)
      Pc JUMPDEST,
      Swap 0,
      Stack (PUSH_N (number_to_words FRAME_POINTER_ADDRESS)),
      (* Stack: return value *)
      Memory MSTORE
    ]
  "

fun populate_function_with_instructions :: "codegen_context \<Rightarrow> program_function \<Rightarrow> program_function" where
  "populate_function_with_instructions context function = function\<lparr>
    r_instructions := (let argument_offset = count_max_let_binding (r_body function) in ([
      Pc JUMPDEST
    ] @
      (* Make room for all let bindings in this function *)
      place_offset_from_frame_pointer ((argument_offset + 1) * 16) @ 
    [
      Stack (PUSH_N (number_to_words (NEXT_FREE_MEMORY_ADDRESS))),
      Memory MSTORE
    ] @
      (* Save argument as a variable *)
      place_offset_from_frame_pointer (argument_offset * 16) @ [
      Memory MSTORE
    ] @
      (* Execute function body *)
      instructions_of_expression (context\<lparr>r_variable_locations := 
        (r_argument_name function, argument_offset) # (r_variable_locations context)
      \<rparr>) (r_body function) @
      (* Return to caller *)
    [
      Swap 0,
      Pc JUMP
    ]
    )
    )
  \<rparr>"

fun instructions_of_ast_functions :: "codegen_context \<Rightarrow> ast_function_definition list \<Rightarrow> inst list * codegen_context" where
  "instructions_of_ast_functions context ast_functions = (
    let 
      context_without_instructions = context\<lparr>
        r_program_functions := map (\<lambda>function. ast_function_definition.extend function \<lparr>r_instructions = []\<rparr>) ast_functions
      \<rparr>; 
      context_with_instructions = context\<lparr>
        r_program_functions := map (populate_function_with_instructions context_without_instructions) (r_program_functions context_without_instructions)
      \<rparr>;
      final_context = context\<lparr>
        r_program_functions := map (populate_function_with_instructions context_with_instructions) (r_program_functions context_with_instructions)
      \<rparr>;
      function_instructions = List.concat (map r_instructions (r_program_functions final_context)) in (
      [
        Stack (PUSH_N (number_to_words ((size (bytes_of_instructions function_instructions)) + 34))),
        Pc JUMP
      ] @ function_instructions @ [Pc JUMPDEST],
      final_context
    ))
  "
      
      

fun instructions_of_program :: "ast_program \<Rightarrow> inst list" where
  "instructions_of_program program = (
    let (instructions, context) = instructions_of_ast_functions \<lparr>r_variable_locations = [], r_program_functions = []\<rparr> (r_defined_functions program);
        binding_offset = count_max_let_binding (r_main_expression program) in
      instructions @ [
        Stack (PUSH_N [(10 * 16)]),
        Stack (PUSH_N (number_to_words (FRAME_POINTER_ADDRESS))),
        Memory MSTORE
      ] @ place_offset_from_frame_pointer (binding_offset * 32) @ [
        Stack (PUSH_N (number_to_words (NEXT_FREE_MEMORY_ADDRESS))),
        Memory MSTORE
      ] @ (
      instructions_of_expression 
        context
        (r_main_expression program)
      )
   )"

fun integers_of_instructions :: "inst list \<Rightarrow> int list" where
  "integers_of_instructions insts = map (\<lambda> w. unat w) (bytes_of_instructions insts)"

end
