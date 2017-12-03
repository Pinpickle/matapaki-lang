theory Codegen
  imports Main List "./Ast" "~~/src/HOL/Library/Code_Target_Int" "./eth-isabelle/ContractSem" "./eth-isabelle/RelationalSem" "./eth-isabelle/ProgramInAvl"
begin

fun bytes_of_value :: "astValue \<Rightarrow> 8 word list" where
  "bytes_of_value (Integer i) = (word_rsplit (word_of_int i::256 word))" |
  "bytes_of_value (Bool b) = (if b then [1] else [0])"

fun instructions_of_binary_operator :: "astBinaryOperator \<Rightarrow> inst list" where
  "instructions_of_binary_operator Plus = [Arith ADD]" |
  "instructions_of_binary_operator Minus = [Arith SUB]" |
  "instructions_of_binary_operator Or = [Bits inst_OR]" |
  "instructions_of_binary_operator And = [Bits inst_AND]"

fun instructions_of_expression :: "astExpression => inst list" where
  "instructions_of_expression (BinaryOperator (operator, e1, e2)) =
    (instructions_of_expression e1) @ (instructions_of_expression e2) @ instructions_of_binary_operator operator" |
  "instructions_of_expression (Value v) = [Stack (PUSH_N (bytes_of_value v))]"

fun integers_of_instruction :: "inst \<Rightarrow> int list" where
  "integers_of_instruction inst = map (\<lambda> w. unat w) (inst_code inst)"

end
