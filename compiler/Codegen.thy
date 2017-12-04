theory Codegen
  imports Main List "./Ast" "~~/src/HOL/Library/Code_Target_Int" "./eth-isabelle/ContractSem" "./eth-isabelle/RelationalSem" "./eth-isabelle/ProgramInAvl"
begin

type_synonym codegen_context = "(String.literal  * nat) list"

fun number_to_words :: "int \<Rightarrow> 8 word list" where
  "number_to_words i = (word_rsplit (word_of_int i::256 word))"

fun offset_for_name_in_context :: "codegen_context \<Rightarrow> String.literal \<Rightarrow> nat" where
  "offset_for_name_in_context [] _ = 0" |
  "offset_for_name_in_context (Cons (name, location) rest) required_name = (
    if (name = required_name) then location
    else offset_for_name_in_context rest required_name
  )"

fun bytes_of_value :: "astValue \<Rightarrow> 8 word list" where
  "bytes_of_value (Integer i) = number_to_words i" |
  "bytes_of_value (Bool b) = (if b then [1] else [0])"

fun instructions_of_binary_operator :: "astBinaryOperator \<Rightarrow> inst list" where
  "instructions_of_binary_operator Plus = [Arith ADD]" |
  "instructions_of_binary_operator Minus = [Arith SUB]" |
  "instructions_of_binary_operator Or = [Bits inst_OR]" |
  "instructions_of_binary_operator And = [Bits inst_AND]"

definition NULL_POINTER_ADDRESS :: "nat" where 
  "NULL_POINTER_ADDRESS = 0"

definition RE_ENTRANCY_FLAG_ADDRESS :: "nat" where 
  "RE_ENTRANCY_FLAG_ADDRESS = 16"

definition NEXT_FREE_MEMORY_ADDRESS :: "nat" where 
  "NEXT_FREE_MEMORY_ADDRESS = 32"

definition FRAME_POINTER_ADDRESS :: "nat" where 
  "FRAME_POINTER_ADDRESS = 48"

(*
  Layout of memory during execution:
  
  0: Null pointer
  1: Re-entrancy flag
  2: Next available memory address (initialised to 4) 
  3: Frame pointer
*)

fun count_max_let_binding :: "astExpression \<Rightarrow> nat" where
  "count_max_let_binding (BinaryOperator (_, e1, e2)) = max (count_max_let_binding e1) (count_max_let_binding e2)" |
  "count_max_let_binding (Value _) = 0" |
  "count_max_let_binding (Variable _) = 0" |
  "count_max_let_binding (LetBinding ((name, assignment), inner_expression)) = 1 + max (count_max_let_binding assignment) (count_max_let_binding inner_expression)"

fun place_offset_from_frame_pointer :: "nat \<Rightarrow> inst list" where
  "place_offset_from_frame_pointer n = [
    Stack (PUSH_N (number_to_words FRAME_POINTER_ADDRESS)),
    Memory MLOAD,
    Stack (PUSH_N (number_to_words n)),
    Arith ADD
  ]"

fun instructions_of_expression :: "codegen_context \<Rightarrow> astExpression => inst list" where
  "instructions_of_expression context (BinaryOperator (operator, e1, e2)) =
    (instructions_of_expression context e1) @ (instructions_of_expression context e2) @ instructions_of_binary_operator operator" |
  "instructions_of_expression _ (Value v) = [Stack (PUSH_N (bytes_of_value v))]" |
  "instructions_of_expression context (LetBinding ((name, assignment), inner_expression)) = (
    let variable_offset = (max (count_max_let_binding assignment) (count_max_let_binding inner_expression)) * 16 in (
      (* Place value on top of stack *)
      (instructions_of_expression context assignment) @
      (* Place address on top of stack *)
      (place_offset_from_frame_pointer variable_offset) @
      (* Save the value to the address *)
      [Memory MSTORE] @
      (* Continue with execution *)
      (instructions_of_expression (Cons (name, variable_offset) context) inner_expression) 
    )
  )" |
  "instructions_of_expression context (Variable name) = (
    (* Place address on top of stack *)
    (place_offset_from_frame_pointer (offset_for_name_in_context context name)) @
    (* Load from address *)
    [Memory MLOAD]
  )"

fun instructions_of_program :: "astExpression \<Rightarrow> inst list" where
  "instructions_of_program expression = [
    Stack (PUSH_N [(10 * 16)]),
    Stack (PUSH_N (number_to_words (FRAME_POINTER_ADDRESS))),
    Memory MSTORE
  ] @ instructions_of_expression []  expression"
    

fun integers_of_instruction :: "inst \<Rightarrow> int list" where
  "integers_of_instruction inst = map (\<lambda> w. unat w) (inst_code inst)"

end
