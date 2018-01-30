theory Codegen
  imports Main List "./Ast" "~~/src/HOL/Library/Code_Target_Int" "./eth-isabelle/ContractSem" "./eth-isabelle/RelationalSem" "./eth-isabelle/ProgramInAvl" "./eth-isabelle/lem/Keccak"
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

definition REVERT_INSTRUCTION :: "inst" where
  "REVERT_INSTRUCTION = Unknown 253"

definition REVERT_WITH_NO_DATA :: "inst list" where
  "REVERT_WITH_NO_DATA = [Stack (PUSH_N [0]), Stack (PUSH_N [0]), REVERT_INSTRUCTION]"

(* The NOT instruction flips every bit.
   We only want to flip the first bit so 0 becomes 1 and vice versa.  *)
definition BOOLEAN_NOT :: "inst list" where
  "BOOLEAN_NOT = [Stack (PUSH_N [0]), Arith inst_EQ]"

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
  "count_max_let_binding (FunctionApplication (_, argument)) = count_max_let_binding argument" |
  "count_max_let_binding UnitLiteral = 0"

fun place_offset_from_stored_address :: "nat \<Rightarrow> nat \<Rightarrow> inst list" where
  "place_offset_from_stored_address address offset = [
    Stack (PUSH_N (number_to_words address)),
    Memory MLOAD,
    Stack (PUSH_N (number_to_words (offset))),
    Arith ADD
  ]"

definition place_offset_from_frame_pointer :: "nat \<Rightarrow> inst list" where
  "place_offset_from_frame_pointer = place_offset_from_stored_address FRAME_POINTER_ADDRESS"

(* Assumes the argument to the function is on the top of the stack *)
fun instructions_to_call_function_of_name :: "codegen_context \<Rightarrow> String.literal \<Rightarrow> inst list" where
  "instructions_to_call_function_of_name context name = (* Stack: frame pointer, argument, ... *)
    place_offset_from_frame_pointer 0 @ [
      (* Stack: argument, frame pointer, ... *)
      Swap 0,
      (* Update frame pointer to be next free memory address *)
      Stack (PUSH_N (number_to_words (NEXT_FREE_MEMORY_ADDRESS))),
      Memory MLOAD,
      Stack (PUSH_N (number_to_words (FRAME_POINTER_ADDRESS))),
      Memory MSTORE,
      (* Stack: function address, argument, frame pointer, ... *)
      Stack (PUSH_N (number_to_words (location_of_function_name_in_context context name))),
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
    ]"

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
    (place_offset_from_frame_pointer ((offset_for_name_in_context (r_variable_locations context) name) * 32)) @
    (* Load from address *)
    [Memory MLOAD]
  )" |
  "instructions_of_expression context UnitLiteral = [Stack (PUSH_N [0])]" |
  "instructions_of_expression context (FunctionApplication (name, argument)) =  
    (instructions_of_expression context argument) @
    (instructions_to_call_function_of_name context name)
  "

fun populate_function_with_instructions :: "codegen_context \<Rightarrow> program_function \<Rightarrow> program_function" where
  "populate_function_with_instructions context function = function\<lparr>
    r_instructions := (let argument_offset = count_max_let_binding (r_body function) in ([
      Pc JUMPDEST
    ] @
      (* Make room for all let bindings in this function *)
      place_offset_from_frame_pointer ((argument_offset + 1) * 32) @ 
    [
      Stack (PUSH_N (number_to_words (NEXT_FREE_MEMORY_ADDRESS))),
      Memory MSTORE
    ] @
      (* Save argument as a variable *)
      place_offset_from_frame_pointer (argument_offset * 32) @ [
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

fun instructions_of_program_context_functions :: "codegen_context \<Rightarrow> inst list" where
  "instructions_of_program_context_functions context = List.concat (map r_instructions (r_program_functions context))"

fun function_jump_location :: "inst list \<Rightarrow> nat" where
  "function_jump_location function_instructions = (size (bytes_of_instructions function_instructions)) + 34"

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
      function_instructions = instructions_of_program_context_functions final_context in (
      [
        Stack (PUSH_N (number_to_words (function_jump_location function_instructions))),
        Pc JUMP
      ] @ function_instructions @ [Pc JUMPDEST],
      final_context
    ))
  "

fun return_32_byte_value_on_stack :: "unit \<Rightarrow> inst list" where
  "return_32_byte_value_on_stack _ =
    place_offset_from_stored_address NEXT_FREE_MEMORY_ADDRESS 0 @ [
      Memory MSTORE,
      Stack (PUSH_N [32])
    ] @ place_offset_from_stored_address NEXT_FREE_MEMORY_ADDRESS 0 @ [
      Misc RETURN
    ]"

fun name_of_type :: "astType \<Rightarrow> String.literal" where
  "name_of_type TInt = String.implode ''int256''" |
  "name_of_type TBool = String.implode ''bool''" |
  "name_of_type TUnit = String.implode ''''" |
  "name_of_type _ = String.implode ''''"

fun function_string_representation :: "String.literal \<Rightarrow> astType \<Rightarrow> astType \<Rightarrow> string" where
  "function_string_representation name input_type output_type = List.concat [
    String.explode name,
    ''('',
    String.explode (name_of_type input_type),
    '')''
  ]"

fun function_signature :: "String.literal \<Rightarrow> astType \<Rightarrow> astType \<Rightarrow> byte list" where
  "function_signature name input_type output_type = take 4 (word_rsplit (keccak (map (\<lambda>c. word_of_int (nat_of_char c)) (function_string_representation name input_type output_type))))"

(* This function returns instructions that assumes there is a value
   of the given type at the top of the stack. These instructions take
   this value and returns it *)
fun return_instructions_for_type :: "astType \<Rightarrow> inst list" where
  "return_instructions_for_type TInt = return_32_byte_value_on_stack ()" |
  "return_instructions_for_type TBool = return_32_byte_value_on_stack ()" |
  "return_instructions_for_type TUnit = [Misc STOP]" |
  "return_instructions_for_type _ = REVERT_WITH_NO_DATA (* Other types are impossible in a well-typed program *)"

fun extract_type_from_call_data :: "astType \<Rightarrow> inst list" where
  "extract_type_from_call_data TInt = [
    Stack (PUSH_N [4]),
    Stack CALLDATALOAD
  ]" |
  "extract_type_from_call_data TBool = [
    Stack (PUSH_N [4]),
    Stack CALLDATALOAD
  ]" |
  "extract_type_from_call_data TUnit = [
    Stack (PUSH_N [0])
  ]" |
  "extract_type_from_call_data _ = REVERT_WITH_NO_DATA"

(* Assumes the function signature is at the top of the stack *)
fun check_and_execute_function :: "codegen_context \<Rightarrow> program_function \<Rightarrow> inst list" where
  "check_and_execute_function context function = (
    let init_instructions = [
      (* [input function signature * 2, ...] *)
      Dup 0,
      (* [function signature, input function signature * 2, ...] *)
      Stack (PUSH_N (function_signature (r_function_name function) (r_argument_type function) (r_return_type function))),
      (* [is signature equal, input function signature, ...] *)
      Arith inst_EQ
      (* [is signature not equal, input function signature, ...] *)
    ] @ BOOLEAN_NOT;
      (* [distance, is signature not equal, input function signature, ...]
      Stack (PUSH_N distance_to_continuation) *)
      call_instructions = [
      (* [pc, distance, is signature not equal, input function signature, ...] *)
      Pc PC,
    (* [continuation location, is signature not equal, input function signature, ...] *)
    Arith ADD,
    (* [input function signature, ...] *)
    Pc JUMPI,
    (* [...] *)
    Stack POP] @
    (* [argument, input function signature, ...] *)
    extract_type_from_call_data (r_argument_type function) @
    instructions_to_call_function_of_name context (r_function_name function) @
    return_instructions_for_type (r_return_type function) in (
      init_instructions @ [
      Stack (PUSH_N (number_to_words (size (bytes_of_instructions call_instructions))))
      ] @ call_instructions @ [
        Pc JUMPDEST
      ])
    )
  "

fun functions_to_selection_instruction :: "codegen_context \<Rightarrow> program_function list \<Rightarrow> inst list" where
  "functions_to_selection_instruction context functions = [
    Stack (PUSH_N [0]),
    Stack CALLDATALOAD,
    (* From Solidity source *)
    Stack (PUSH_N [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
    Swap 0,
    Arith DIV,
    Stack (PUSH_N [255, 255, 255, 255]),
    Bits inst_AND
  ] @ List.concat (map (check_and_execute_function context) functions) @ [
    Stack (PUSH_N [0]),
    Stack (PUSH_N [0]),
    REVERT_INSTRUCTION
  ]"
    
fun init_instructions_naive :: "codegen_context \<Rightarrow> nat \<Rightarrow> inst list" where
  "init_instructions_naive context offset = [
    (* [pre-marker pc] *)
    Pc PC,
    (* Push a marker to the stack, this will be overwritten in memory
       [marker, pre-marker pc]*)
    Stack (PUSH_N (number_to_words 0)),
    (* [post-marker pc, marker, pre-marker pc] *)
    Pc PC,
    (* [destination - (post-marker pc), (post-marker pc), marker, pre-marker pc] *)
    Stack (PUSH_N (number_to_words (offset - 36))),
    (* [destination, marker, pre-marker pc] *)
    Arith ADD,
    (* Jump to JUMPDEST at the bottom of this section if marker is 1
       [pre-marker pc] *)
    Pc JUMPI,
    (* Put existing code into memory  *)
    Info CODESIZE,
    Stack (PUSH_N [0]),
    Stack (PUSH_N [0]),
    Memory CODECOPY,
    (* Then overwrite the marker at pre-marker pc + 3  *)
    Stack (PUSH_N [2]),
    Arith ADD,
    Stack (PUSH_N [1]),
    Swap 0,
    Memory MSTORE,
    (* Then return the data *)
    Info CODESIZE,
    Stack (PUSH_N [0]),
    Misc RETURN,
    (* Continue with execution if jumped to *)
    Pc JUMPDEST,
    Stack POP
  ]"

fun init_instructions :: "codegen_context \<Rightarrow> inst list" where
  "init_instructions context = init_instructions_naive context (size (bytes_of_instructions (init_instructions_naive context 0)))"

fun instructions_of_program :: "ast_program \<Rightarrow> inst list" where
  "instructions_of_program program = (
    let (instructions, context) = instructions_of_ast_functions \<lparr>r_variable_locations = [], r_program_functions = []\<rparr> (r_defined_functions program) in
      instructions @ [
        Stack (PUSH_N [(10 * 16)]),
        Stack (PUSH_N [(10 * 16)]),
        Stack (PUSH_N (number_to_words (NEXT_FREE_MEMORY_ADDRESS))),
        Memory MSTORE,
        Stack (PUSH_N (number_to_words (FRAME_POINTER_ADDRESS))),
        Memory MSTORE
      ] @ init_instructions context @ (
      functions_to_selection_instruction 
        context
        (filter (r_exported) (r_program_functions context))
      )
   )"

fun integers_of_instructions :: "inst list \<Rightarrow> int list" where
  "integers_of_instructions insts = map (\<lambda> w. unat w) (bytes_of_instructions insts)"

end
