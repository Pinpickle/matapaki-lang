theory Codegen
  imports Main List "./Ast" "~~/src/HOL/Library/Code_Target_Numeral" "./eth-isabelle/ContractSem" "./eth-isabelle/RelationalSem" "./eth-isabelle/ProgramInAvl" "./eth-isabelle/lem/Keccak"
begin

type_synonym variable_location_list = "(String.literal  * nat) list"
record program_function = ast_function_definition +
  r_instructions :: "inst list"
record codegen_context =
  r_variable_locations :: variable_location_list
  r_program_functions :: "program_function list"
  r_codegen_state_type :: "astType"

definition "STORAGE_ADDRESS_MASK = ((word_cat (1::1 word) (0::255 word))::256 word)"
definition "STORAGE_RE_ENTRANCY_FLAG_STATE_ADDRESS = 0"
definition "STORAGE_STATE_ADDRESS = unat (STORAGE_ADDRESS_MASK)"

definition "NULL_POINTER_ADDRESS = 0"
definition "NEXT_FREE_MEMORY_ADDRESS = 32"
definition "FRAME_POINTER_ADDRESS = 64"
definition "TEMP_VARIABLE_ADDRESS = 96"

definition "INITIAL_INSTRUCTIONS_LENGTH = 34"
definition "REVERT_INSTRUCTION = Unknown 253"
definition "REVERT_WITH_NO_DATA = [Stack (PUSH_N [0]), Stack (PUSH_N [0]), REVERT_INSTRUCTION]"
definition "IS_ADDRESS_STORAGE = [Stack (PUSH_N [0]), Swap 0, Sarith SGT]" 

(* The NOT instruction flips every bit.
   We only want to flip the first bit so 0 becomes 1 and vice versa.  *)
definition "BOOLEAN_NOT = [Stack (PUSH_N [0]), Arith inst_EQ]"

fun number_to_words_padded :: "int \<Rightarrow> 8 word list" where
  "number_to_words_padded i = (word_rsplit (word_of_int i::256 word))"

fun remove_word_padding :: "8 word list \<Rightarrow> 8 word list" where
  "remove_word_padding (w # ws) = (if (w = 0) then remove_word_padding ws else w # ws)" |
  "remove_word_padding [] = []"

fun number_to_words :: "int \<Rightarrow> 8 word list" where
  "number_to_words i = number_to_words_padded i"

definition number_to_words_minimum :: "int \<Rightarrow> 8 word list" where
  "number_to_words_minimum i = remove_word_padding (number_to_words_padded i)"

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
  "bytes_of_value (Integer i) = number_to_words_minimum i" |
  "bytes_of_value (Bool b) = (if b then [1] else [0])" |
  "bytes_of_value (AddressLiteral a) = number_to_words_minimum a"

fun instructions_of_binary_operator :: "astBinaryOperator \<Rightarrow> inst list" where
  "instructions_of_binary_operator Plus = [Arith ADD]" |
  "instructions_of_binary_operator Minus = [Swap 0, Arith SUB]" |
  "instructions_of_binary_operator Multiply = [Arith MUL]" |
  "instructions_of_binary_operator Divide = [Swap 0, Sarith SDIV]" |
  "instructions_of_binary_operator Mod = [Swap 0, Sarith SMOD]" |
  "instructions_of_binary_operator Greater = [Swap 0, Sarith SGT]" |
  "instructions_of_binary_operator GreaterEqual = [Swap 0, Sarith SLT] @ BOOLEAN_NOT" |
  "instructions_of_binary_operator Lesser = [Swap 0, Sarith SLT]" |
  "instructions_of_binary_operator LesserEqual = [Swap 0, Sarith SGT] @ BOOLEAN_NOT" |
  "instructions_of_binary_operator Equal = [Arith inst_EQ]" |
  "instructions_of_binary_operator Or = [Bits inst_OR]" |
  "instructions_of_binary_operator And = [Bits inst_AND]"

fun count_max_let_binding :: "astExpression \<Rightarrow> nat" where
  "count_max_let_binding (BinaryOperator (_, e1, e2)) = max (count_max_let_binding e1) (count_max_let_binding e2)" |
  "count_max_let_binding (Value _) = 0" |
  "count_max_let_binding (Variable _) = 0" |
  "count_max_let_binding (LetBinding ((name, assignment), inner_expression)) = 1 + max (count_max_let_binding assignment) (count_max_let_binding inner_expression)" |
  "count_max_let_binding (FunctionApplication (_, argument)) = count_max_let_binding argument" |
  "count_max_let_binding (RecordAccess (expression, _)) = count_max_let_binding expression" |
  "count_max_let_binding (RecordLiteral (values)) = fold max (map (\<lambda>(_, (_, expression)). count_max_let_binding expression) values) 0" |
  "count_max_let_binding (EffectUnwrap expression) = count_max_let_binding expression" |
  "count_max_let_binding (RecordUpdate (expression, _, values)) = fold max (map (\<lambda>(_, (_, expression)). count_max_let_binding expression) values) (count_max_let_binding expression)" |
  "count_max_let_binding (SendEther (address_expression, value_expression)) =
    max (count_max_let_binding address_expression) (count_max_let_binding value_expression)" |
  "count_max_let_binding (IfExpression (condition_expression, true_expression, false_expression)) =
    max (count_max_let_binding condition_expression) (max (count_max_let_binding true_expression) (count_max_let_binding false_expression))" |
  "count_max_let_binding (SenderExpression) = 0" |
  "count_max_let_binding (NewMapping _) = 0" |
  "count_max_let_binding (MappingAccess (mapping_expr, key_expr)) = max (count_max_let_binding mapping_expr) (count_max_let_binding key_expr)" |
  "count_max_let_binding (MappingUpdate (mapping_expr, update_exprs)) = max 
    (count_max_let_binding mapping_expr)
    (fold 
      max 
      (map (\<lambda>(key_expr, value_expr). max (count_max_let_binding key_expr) (count_max_let_binding value_expr)) update_exprs)
      0)" |
  "count_max_let_binding (RequireExpression (condition_expression, pass_expression)) = max (count_max_let_binding condition_expression) (count_max_let_binding pass_expression)"

fun place_offset_from_stored_address :: "nat \<Rightarrow> nat \<Rightarrow> inst list" where
  "place_offset_from_stored_address address offset = [
    Stack (PUSH_N (number_to_words_minimum address)),
    Memory MLOAD,
    Stack (PUSH_N (number_to_words_minimum (offset))),
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
      Stack (PUSH_N (number_to_words_minimum (NEXT_FREE_MEMORY_ADDRESS))),
      Memory MLOAD,
      Stack (PUSH_N (number_to_words_minimum (FRAME_POINTER_ADDRESS))),
      Memory MSTORE,
      (* Stack: function address, argument, frame pointer, ... *)
      Stack (PUSH_N (number_to_words_padded (location_of_function_name_in_context context name))),
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
      Stack (PUSH_N (number_to_words_minimum FRAME_POINTER_ADDRESS)),
      (* Stack: return value *)
      Memory MSTORE
    ]"

definition create_record :: "(nat * inst list) list \<Rightarrow> bool \<Rightarrow> nat \<Rightarrow> inst list" where
  "create_record values partial num_values = [
      (* First reserve the memory we need for this record*)
      (* [Next free address pointer] *)
      Stack (PUSH_N (number_to_words_minimum (NEXT_FREE_MEMORY_ADDRESS))),
      (* [free address] *)
      Memory MLOAD,
      (* [free address, free address] *)
      Dup 0,
      (* [record size, free address, free address] *)
      Stack (PUSH_N (number_to_words_minimum (num_values * 40 + 32))),
      (* [new free address, free address] *)
      Arith ADD,
      (* [free address pointer, new free address, free address] *)
      Stack (PUSH_N (number_to_words_minimum (NEXT_FREE_MEMORY_ADDRESS))),
      (* [free address] *)
      Memory MSTORE
    ] @ (if partial then ([
      (* [free address, base record] *)
      (* [base record, free address] *)
      Swap 0,
      (* [free address, base record, free address *)
      Dup 1,
      (* [free address] *)
      Memory MSTORE]) else []) @ List.concat (map (\<lambda>(i, instructions).
      (* [value, free address] *)
      instructions @ [
      (* [free address, value, free address] *)
      Dup 1,
      (* [record offset, free address, value, free address] *)
      Stack (PUSH_N (number_to_words_minimum (i * 40 + 32))),
      (* [value address, value, free address] *)
      Arith ADD,
      (* [value address, value address, value, free address] *)
      Dup 0,
      (* [true (byte), value address, value address, value, free address] *)
      Stack (PUSH_N [1]),
      (* [value address, true (byte), value address, value, free address] *)
      Swap 0,
      (* [value address, value, free address] *)
      Memory MSTORE8,
      (* [8, value address, value, free address] *)
      Stack (PUSH_N [8]),
      (* [value address, value, free address] *)
      Arith ADD,
      (* [free address] *)
      Memory MSTORE
    ]) values)"

definition create_full_record :: "(nat * inst list) list \<Rightarrow> inst list" where
  "create_full_record values = create_record values False (size values)"

fun fetch_state :: "astType \<Rightarrow> inst list" where
  "fetch_state _ = [
    Stack (PUSH_N (number_to_words_minimum (STORAGE_STATE_ADDRESS))),
    Storage SLOAD
  ]"

definition branch_if :: "inst list \<Rightarrow> inst list \<Rightarrow> inst list" where
  "branch_if true_instructions false_instructions = ( 
    let jumped_true_instructions = [Pc JUMPDEST] @ true_instructions;
        jumped_false_instructions = false_instructions @ [
          Stack (PUSH_N (number_to_words_minimum (size (bytes_of_instructions jumped_true_instructions) + 3))),
          Pc PC,
          Arith ADD,
          Pc JUMP
        ] in [
      Stack (PUSH_N (number_to_words_minimum (size (bytes_of_instructions jumped_false_instructions) + 3))),
      Pc PC,
      Arith ADD,
      Pc JUMPI
    ] @ jumped_false_instructions @ jumped_true_instructions @ [Pc JUMPDEST])"

definition check_reentrancy :: "inst list \<Rightarrow> inst list" where
  "check_reentrancy instructions = [
    Stack (PUSH_N (number_to_words_minimum STORAGE_RE_ENTRANCY_FLAG_STATE_ADDRESS)),
    Storage SLOAD,
    Stack (PUSH_N [2]),
    Arith inst_EQ
  ] @ branch_if
    (* Re-entrancy flag is set, don't proceed *)
    REVERT_WITH_NO_DATA
    (* No flag, we're good to keep going *)
    []
  @ instructions"

definition wrap_reentrancy :: "inst list \<Rightarrow> inst list" where
  "wrap_reentrancy instructions =
    check_reentrancy ([
      Stack (PUSH_N [2]),
      Stack (PUSH_N (number_to_words_minimum STORAGE_RE_ENTRANCY_FLAG_STATE_ADDRESS)),
      Storage SSTORE
    ] @ instructions @ [
      Stack (PUSH_N [1]),
      Stack (PUSH_N (number_to_words_minimum STORAGE_RE_ENTRANCY_FLAG_STATE_ADDRESS)),
      Storage SSTORE
    ])"

definition "keccak_stack_value = [
  Stack (PUSH_N (number_to_words_minimum (NEXT_FREE_MEMORY_ADDRESS))),
  Memory MLOAD,
  Memory MSTORE,
  Stack (PUSH_N [32]),
  Stack (PUSH_N (number_to_words_minimum (NEXT_FREE_MEMORY_ADDRESS))),
  Memory MLOAD,
  Arith SHA3
]"

definition "create_mapping_storage_address = [
  Bits inst_XOR
] @ keccak_stack_value"

fun update_mapping :: "((inst list) * (inst list)) list \<Rightarrow> inst list" where
  "update_mapping [] = []" |
  "update_mapping ((key_instructions, value_instructions) # entries) = [
    Stack (PUSH_N (number_to_words_minimum (NEXT_FREE_MEMORY_ADDRESS))),
    Memory MLOAD,
    Dup 0,
    Stack (PUSH_N [32 * 3]),
    Arith ADD,
    Stack (PUSH_N (number_to_words_minimum (NEXT_FREE_MEMORY_ADDRESS))),
    Memory MSTORE,
    Swap 0,
    Dup 1,
    Memory MSTORE
  ] @ key_instructions @ [
    Dup 1,
    Stack (PUSH_N [32]),
    Arith ADD,
    Memory MSTORE
  ] @ value_instructions @ [
    Dup 1,
    Stack (PUSH_N [32 * 2]),
    Arith ADD,
    Memory MSTORE
  ] @ (update_mapping entries)"

(* Iterates over all entries in memory for the mapping at the top of the stack. *)
fun iterate_memory_mapping_entries :: "(inst list * inst list * inst list) \<Rightarrow> inst list" where
  "iterate_memory_mapping_entries (default_instructions, memory_instructions, storage_instructions) = [
    Pc JUMPDEST,
    (* [pc, mapping address] *)
    Pc PC,
    Stack (PUSH_N [1]),
    Swap 0,
    (* [loopback address, mapping address] *)
    Arith SUB,
    (* [mapping address, loopback address] *)
    Swap 0,
    (* [mapping address, mapping address, loopback address] *)
    Dup 0,
    Stack (PUSH_N [0]),
    Arith inst_EQ
  ] @ branch_if ([
      Stack POP,
      Stack POP
    ] @ default_instructions) (
      [
        Dup 0
        (* [is address memory, mapping address, loopback address] *)
      ] @ IS_ADDRESS_STORAGE @ branch_if ([
          Dup 0,
          (* [value offset, mapping address, mapping address, loopback] *)
          Stack (PUSH_N [64]),
          (* [value location, mapping address, loopback] *)
          Arith ADD,
          (* [value, mapping address, loopback] *)
          Memory MLOAD,
          Dup 1,
          Stack (PUSH_N [32]),
          Arith ADD,
          (* [key, value, mapping address, loopback] *)
          Memory MLOAD
        ] @ memory_instructions @ [
          Memory MLOAD,
          Swap 0,
          Pc JUMP
        ]) (
        [
          Swap 0,
          Stack POP
        ] @ storage_instructions)
      )"

fun access_mapping :: "(inst list * inst list) \<Rightarrow> inst list" where
  "access_mapping (mapping_instructions, key_instructions) =
    key_instructions @ mapping_instructions @ [
      Pc JUMPDEST,
      (* [pc, mapping address, key] *)
      Pc PC,
      Stack (PUSH_N [1]),
      Swap 0,
      (* [loopback address, mapping address, key] *)
      Arith SUB,
      (* [mapping address, loopback address, key] *)
      Swap 0,
      (* [mapping address, mapping address, loopback address, key] *)
      Dup 0,
      Stack (PUSH_N [0]),
      Arith inst_EQ
    ] @ branch_if [
      Stack POP,
      Stack POP,
      Stack POP,
      Stack (PUSH_N [0])
    ] (
      [
        Dup 0
        (* [is address memory, mapping address, loopback address, key] *)
      ] @ IS_ADDRESS_STORAGE @ (branch_if ([
          Dup 0,
          (* [key offset, mapping address, mapping address, loopback, key] *)
          Stack (PUSH_N [32]),
          (* [key location, mapping address, loopback, key] *)
          Arith ADD,
          (* [key at address, mapping address, loopback, key] *)
          Memory MLOAD,
          Dup 3,
          (* [is key correct, mapping address, loopback, key] *)
          Arith inst_EQ
        ] @ branch_if [
            Swap 1,
            Stack POP,
            Stack POP,
            Stack (PUSH_N [64]),
            Arith ADD,
            Memory MLOAD
          ] [
            Memory MLOAD,
            Swap 0,
            Pc JUMP
          ]
        ) ([
            Swap 0,
            Stack POP
          ] @ create_mapping_storage_address @ [
            Storage SLOAD
          ])
        )
    )"
          

fun access_record_raw :: "inst list \<Rightarrow> nat \<Rightarrow> (inst list * inst list * inst list) \<Rightarrow> inst list" where
  "access_record_raw instructions i (default_instructions, memory_instructions, storage_instructions) = 
  (* [record address] *)
  instructions @ 
  [ Pc JUMPDEST,
    Pc PC,
    Stack (PUSH_N [1]),
    Swap 0,
    Arith SUB,
    (* [record address, loopback address] *)
    Swap 0,
    Dup 0,
    Stack (PUSH_N [0]),
    Arith inst_EQ
  ] @ branch_if ([
    Stack POP,
    Stack POP,
    Stack (PUSH_N [0])
  ] @ default_instructions) ([
    (* [record address, record address, loopback address] *)
    Dup 0,
    Stack (PUSH_N (word_rsplit STORAGE_ADDRESS_MASK)),
    Bits inst_AND,
    Stack (PUSH_N [0]),
    (* [is address memory, record address, loopback address] *)
    Arith inst_EQ
  ] @ (
    branch_if (
      [
        Dup 0,
        (* [value offset, record address, record address, loopback] *)
        Stack (PUSH_N (number_to_words_minimum (i * 40 + 32))),
        (* [value location, record address, loopback] *)
        Arith ADD,
        (* [is_valid, record address, loopback] *)
        Memory MLOAD
      ] @ branch_if
        ([
          Swap 0,
          Stack POP
        ] @ memory_instructions)
        [
          Memory MLOAD,
          Swap 0,
          Pc JUMP
        ]
    )
  ) 
  ( 
    [
      Swap 0,
      Stack POP
    ] @ storage_instructions
  ))"

definition access_record :: "inst list \<Rightarrow> nat \<Rightarrow> inst list" where
  "access_record instructions i =
    access_record_raw
      instructions
      i
      (
        [],
        [
          Stack (PUSH_N (number_to_words_minimum (32 + i * 40 + 8))),
          (* [value location, record address] *)
          Arith ADD,
          (* [value, record address] *)
          Memory MLOAD
        ],
        [
          Stack (PUSH_N (number_to_words_minimum (i))),
          Arith ADD,
          Storage SLOAD
        ]
      )
  "

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
    (instructions_of_expression context argument) @
    (instructions_to_call_function_of_name context name)
  " |
  "instructions_of_expression context (RecordLiteral (values)) =
    create_full_record (
      map
        (\<lambda>(i, (_, expression)). (i, instructions_of_expression context expression)) 
        values
    )" |
  "instructions_of_expression context (RecordAccess (expression, name, i)) =
    access_record (instructions_of_expression context expression) i" |
  "instructions_of_expression context (EffectUnwrap expression) = (instructions_of_expression context expression)" |
  "instructions_of_expression context (RecordUpdate (expression, num_values, values)) = 
    (* Get address of record to be updated *)
    (instructions_of_expression context expression) @
    create_record (
      map (\<lambda>(i, (_, value_expression)). (i, instructions_of_expression context value_expression))
      values
    ) True num_values
  " |
  "instructions_of_expression context (SendEther (address_expression, value_expression)) = [
    Stack (PUSH_N [0]),
    Stack (PUSH_N [0]),
    Stack (PUSH_N [0]),
    Stack (PUSH_N [0])
  ] @ instructions_of_expression context value_expression
    @ instructions_of_expression context address_expression
    @ [
    (* For the time being, forward all of the gas *)
    Stack (PUSH_N (word_rsplit ((word_cat (1::1 word) (0::255 word))::256 word))),
    Misc CALL
  ] @ branch_if [
    (* Call was successful, push unit onto the stack *)
    Stack (PUSH_N [NULL_POINTER_ADDRESS])
  ] REVERT_WITH_NO_DATA" |
  "instructions_of_expression context (IfExpression (condition_expression, true_expression, false_expression)) =
    instructions_of_expression context condition_expression @
    branch_if
      (instructions_of_expression context true_expression)
      (instructions_of_expression context false_expression)
  " |
  "instructions_of_expression context SenderExpression = [Info CALLER]" |
  "instructions_of_expression context (NewMapping _) = [Stack (PUSH_N [0])]" |
  "instructions_of_expression context (MappingUpdate (mapping_expression, entries_expressions)) = (instructions_of_expression context mapping_expression) @ update_mapping (map (\<lambda>(key_expression, value_expression). (
    instructions_of_expression context key_expression,
    instructions_of_expression context value_expression
  )) entries_expressions)" |
  "instructions_of_expression context (MappingAccess (mapping_expr, key_expr)) = access_mapping (instructions_of_expression context mapping_expr, instructions_of_expression context key_expr)" |
  "instructions_of_expression context (RequireExpression (condition_expression, pass_expression)) = 
    instructions_of_expression context condition_expression @ (
      branch_if (
        instructions_of_expression context pass_expression
      ) REVERT_WITH_NO_DATA
    )"

definition string_of_words :: "byte list \<Rightarrow> String.literal" where
  "string_of_words ws = String.implode (map (\<lambda>w. char_of_nat (unat w)) ws)"

definition words_of_string :: "String.literal \<Rightarrow> byte list" where
  "words_of_string str = map (\<lambda>c. word_of_int (nat_of_char c)) (String.explode str)"

(* We create this convoluted setup so we can replace this with an efficient implementation *)
definition keccak_of_string ::  "String.literal \<Rightarrow> String.literal" where
  "keccak_of_string i = string_of_words (word_rsplit (keccak (words_of_string i)))"

definition "contents_address_of_base_address = keccak_stack_value @ [
    Stack (PUSH_N (word_rsplit STORAGE_ADDRESS_MASK)),
    Bits inst_OR
  ]"

(* This function expects the top two values on the stack to be [address, value to save] *)
fun save_state_at_address :: "astType \<Rightarrow> inst list" where
  "save_state_at_address TInt = [Storage SSTORE]" |
  "save_state_at_address TBool = [Storage SSTORE]" |
  "save_state_at_address TAddress = [Storage SSTORE]" |
  "save_state_at_address (TRecord record_values) =
    ([Dup 0] @ contents_address_of_base_address @ [
      Dup 0,
      Stack (PUSH_N (number_to_words_minimum TEMP_VARIABLE_ADDRESS)),
      Memory MSTORE,
      Swap 0,
      Storage SSTORE
    ] @ List.concat (List.map 
    (\<lambda>(index, (_, type)). (
      (access_record_raw
        [Dup 0]
        index
        (
          [
            Stack (PUSH_N [1])
          ],
          [
            Stack (PUSH_N (number_to_words_minimum (32 + index * 40 + 8))),
            Arith ADD,
            Memory MLOAD,
            Stack (PUSH_N [1])
          ],
          ([
            Dup 0,
            Stack (PUSH_N (number_to_words_minimum TEMP_VARIABLE_ADDRESS)),
            Memory MLOAD,
            Arith inst_EQ
          ] @ branch_if [
              Stack POP,
              Stack (PUSH_N [0])
            ] [
              Stack (PUSH_N (number_to_words_minimum (index))),
              Arith ADD,
              Storage SLOAD,
              Stack (PUSH_N [1])
            ]
        ))) @
      (branch_if
        ([
          Stack (PUSH_N (number_to_words_minimum TEMP_VARIABLE_ADDRESS)),
          Memory MLOAD,
          Swap 0,
          Dup 1,
          Stack (PUSH_N (number_to_words_minimum index)),
          Arith ADD
        ] @ save_state_at_address type @ [
          Stack (PUSH_N (number_to_words_minimum TEMP_VARIABLE_ADDRESS)),
          Memory MSTORE
        ])
        [])
    )) record_values)
  ) @ [
    Stack POP
  ]" |
  "save_state_at_address (TMapping (_, value_type)) =
    [Dup 0] @ contents_address_of_base_address @ [
      Dup 0,
      Stack (PUSH_N (number_to_words_minimum TEMP_VARIABLE_ADDRESS)),
      Memory MSTORE,
      Swap 0,
      Storage SSTORE
    ] @ iterate_memory_mapping_entries (
      [],
      [
        Stack (PUSH_N (number_to_words_minimum TEMP_VARIABLE_ADDRESS)),
        (* [mapping address, key, value] *)
        Memory MLOAD,
        (* [value, key, mapping address] *)
        Swap 1,
        (* [key, value, mapping address] *)
        Swap 0,
        (* [mapping address, key, value, mapping address] *)
        Dup 2
      ] @ create_mapping_storage_address @ save_state_at_address (value_type) @ [
        Stack (PUSH_N (number_to_words_minimum TEMP_VARIABLE_ADDRESS)),
        Memory MSTORE
      ],
      [
        Stack (PUSH_N (number_to_words_minimum TEMP_VARIABLE_ADDRESS)),
        Memory MLOAD,
        Arith inst_EQ
      ] @ branch_if
        []
        (* We have just tried to move a mapping from one location in storage to another.
           Currently not possible so we abort *)
        REVERT_WITH_NO_DATA
      )
  " |
  "save_state_at_address _ = REVERT_WITH_NO_DATA"

(* This assumes the argument is at the top of the stack,
   and finishes with the result on the top of the stack.
   Leaving everything else unaffected. *)
fun function_body_to_instructions :: "codegen_context \<Rightarrow> ast_function_body \<Rightarrow> String.literal \<Rightarrow> inst list" where
  "function_body_to_instructions context (FunctionExpression expression) arg_name = (
    let argument_offset = (count_max_let_binding (expression)) * 32 in (
      (* Make room for all let bindings in this function *)
      place_offset_from_frame_pointer (argument_offset + 32) @ 
    [
      Stack (PUSH_N (number_to_words_minimum (NEXT_FREE_MEMORY_ADDRESS))),
      Memory MSTORE
    ] @
      (* Save argument as a variable *)
      place_offset_from_frame_pointer argument_offset @ [
      Memory MSTORE
    ] @
      (* Execute function body *)
      instructions_of_expression (context\<lparr>r_variable_locations := 
        (arg_name, argument_offset) # (r_variable_locations context)
      \<rparr>) (expression)
    )
    )" |
  "function_body_to_instructions context (FunctionModifier (modifiee_name, WithState)) _ = check_reentrancy (create_full_record [
    (0, fetch_state (r_codegen_state_type context)),
    (1, [Swap 0] (* argument is second element on the stack at point of execution *))
  ] @ instructions_to_call_function_of_name context modifiee_name)" |
  "function_body_to_instructions context (FunctionModifier (modifiee_name, UpdatingState)) _ = wrap_reentrancy (create_full_record [
    (0, fetch_state (r_codegen_state_type context)),
    (1, [Swap 0] (* argument is second element on the stack at point of execution *))
  ] @ instructions_to_call_function_of_name context modifiee_name @
      (* The first element of the result record is the new state *)
      access_record [Dup 0] 0 @
      [Stack (PUSH_N (number_to_words_minimum STORAGE_STATE_ADDRESS))] @
      save_state_at_address (r_codegen_state_type context) @
      (* The second element is the data to return *)
      access_record [] 1)" 
    

fun populate_function_with_instructions :: "codegen_context \<Rightarrow> program_function \<Rightarrow> program_function" where
  "populate_function_with_instructions context function = function\<lparr>
    r_instructions := [Pc JUMPDEST] @ (function_body_to_instructions context (r_body function) (r_argument_name function)) @ [Swap 0, Pc JUMP]
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
        Stack (PUSH_N (number_to_words_padded (function_jump_location function_instructions))),
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

fun name_of_type_string :: "astType \<Rightarrow> string" where
  "name_of_type_string TInt = ''int256''" |
  "name_of_type_string TBool = ''bool''" |
  "name_of_type_string TAddress = ''uint160''" |
  "name_of_type_string (TRecord []) = ''''" |
  "name_of_type_string (TRecord [(_, (_, record_type))]) = name_of_type_string record_type" |
  "name_of_type_string (TRecord ((_, (_, record_type)) # vals)) = List.concat [name_of_type_string record_type, '','',  name_of_type_string (TRecord vals)]" |
  "name_of_type_string (TEffect (_, inner_type)) = name_of_type_string inner_type" |
  "name_of_type_string _ = ''''"

fun name_of_type :: "astType \<Rightarrow> String.literal" where
  "name_of_type type = String.implode (name_of_type_string type)"

fun function_string_representation :: "String.literal \<Rightarrow> astType \<Rightarrow> astType \<Rightarrow> String.literal" where
  "function_string_representation name input_type output_type = String.implode (List.concat [
    String.explode name,
    ''('',
    (name_of_type_string input_type),
    '')''
  ])"

fun function_signature :: "String.literal \<Rightarrow> astType \<Rightarrow> astType \<Rightarrow> byte list" where
  "function_signature name input_type output_type = take 4 (map (\<lambda>c. word_of_int (nat_of_char c)) (String.explode (keccak_of_string (function_string_representation name input_type output_type))))"
    

(* This function returns instructions that assumes there is a value
   of the given type at the top of the stack. These instructions take
   this value and returns it *)
fun return_instructions_for_type :: "astType \<Rightarrow> inst list" where
  "return_instructions_for_type TInt = return_32_byte_value_on_stack ()" |
  "return_instructions_for_type TBool = return_32_byte_value_on_stack ()" |
  "return_instructions_for_type TAddress = return_32_byte_value_on_stack ()" |
  "return_instructions_for_type (TRecord values) = [
    (* [free address pointer, return record] *)
    Stack (PUSH_N (number_to_words_minimum NEXT_FREE_MEMORY_ADDRESS)),
    (* [free address, record] *)
    Memory MLOAD
  ] @ List.concat (map (\<lambda>(i, _).
    (* [value, free address, record] *)
    (access_record [Dup 1] i) @  [
      (* [free address, value, free address, record] *)
      Dup 1,
      (* [return offset, free address, value, free address, record] *)
      Stack (PUSH_N (number_to_words_minimum (i * 32))),
      (* [return address, value, free address, record] *)
      Arith ADD,
      (* [free address, record] *)
      Memory MSTORE
    ]
  ) values) @ [
    Stack (PUSH_N (number_to_words_minimum ((length values) * 32))),
    Swap 0,
    Misc RETURN
  ]" |
  "return_instructions_for_type (TEffect (_, inner_type)) = return_instructions_for_type inner_type" |
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
  "extract_type_from_call_data TAddress = [
    Stack (PUSH_N [4]),
    Stack CALLDATALOAD
  ]" |
  "extract_type_from_call_data (TRecord values) =
    create_full_record (
      map
        (\<lambda>(i, (_, expression)). (i, [Stack (PUSH_N (number_to_words_minimum (i * 32 + 4))), Stack CALLDATALOAD])) 
        values
    )" |
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
      Stack (PUSH_N (number_to_words_minimum (size (bytes_of_instructions call_instructions))))
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

fun program_defined_init :: "codegen_context \<Rightarrow> inst list" where
  "program_defined_init context =  [
    Stack (PUSH_N [0]) (* Push unit as input to the function no arg *)
  ] @ instructions_to_call_function_of_name context AST_INIT_NAME @ [
    Stack (PUSH_N (number_to_words_minimum STORAGE_STATE_ADDRESS))
  ] @ save_state_at_address  (r_codegen_state_type context)"

fun init_instructions_naive :: "codegen_context \<Rightarrow> nat \<Rightarrow> inst list" where
  "init_instructions_naive context offset = [
    (* [pre-marker pc] *)
    Pc PC,
    (* Push a marker to the stack, this will be overwritten in memory
       [marker, pre-marker pc]*)
    Stack (PUSH_N (number_to_words_padded 0)),
    (* [post-marker pc, marker, pre-marker pc] *)
    Pc PC,
    (* [destination - (post-marker pc), (post-marker pc), marker, pre-marker pc] *)
    Stack (PUSH_N (number_to_words_padded (offset - 36))),
    (* [destination, marker, pre-marker pc] *)
    Arith ADD,
    (* Jump to JUMPDEST at the bottom of this section if marker is 1
       [pre-marker pc] *)
    Pc JUMPI,
    Stack (PUSH_N [1]),
    Stack (PUSH_N (number_to_words_padded STORAGE_RE_ENTRANCY_FLAG_STATE_ADDRESS)),
    Storage SSTORE] @
    (* Run user-defined init *)
    program_defined_init context @
    (* Put existing code into memory  *)
    [Info CODESIZE,
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

fun instructions_of_program :: "typed_program \<Rightarrow> inst list" where
  "instructions_of_program program = (
    let (instructions, context) = instructions_of_ast_functions \<lparr>r_variable_locations = [], r_program_functions = [], r_codegen_state_type = r_program_state_type program\<rparr> (r_defined_functions program) in
      instructions @ [
        Stack (PUSH_N [128]),
        Stack (PUSH_N [128]),
        Stack (PUSH_N (number_to_words_minimum (NEXT_FREE_MEMORY_ADDRESS))),
        Memory MSTORE,
        Stack (PUSH_N (number_to_words_minimum (FRAME_POINTER_ADDRESS))),
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
