theory Compiler
  imports Main List "~~/src/HOL/Library/Code_Target_Int" "./eth-isabelle/ContractSem" "./eth-isabelle/RelationalSem" "./eth-isabelle/ProgramInAvl"
begin

datatype astValue = Integer "int" | Bool "bool"

datatype astExpression = Plus "astExpression * astExpression" | Or "astExpression * astExpression" | Value "astValue"

datatype astType = TInt | TBool

datatype ('a, 'b) either = Right 'a | Left 'b

fun findType :: "astExpression => (astType, unit) either" where
  "findType (Plus (expression1, expression2)) = (
    let type1 = findType(expression1) in
    let type2 = findType(expression2) in
    if (type1 = Right TInt) & (type2 = Right TInt) then
      Right(TInt)
    else
      Left(())
  )" |
  "findType (Value (Integer _)) = Right TInt" |
  "findType (Value (Bool _)) = Right TBool" |
  "findType (Or (expression1, expression2)) = (
    let type1 = findType(expression1) in
    let type2 = findType(expression2) in
    if type1 = Right TBool & type2 = Right TBool then
      Right TBool
    else
      Left ()
  )"

fun intToBytes :: "int \<Rightarrow> 8 word list" where
  "intToBytes n = (word_rsplit (word_of_int n::256 word))"

fun boolToWord :: "bool \<Rightarrow> 8 word" where
  "boolToWord b = (if b then 1 else 0)"

fun compileToBytecode :: "astExpression => inst list" where
  "compileToBytecode (Plus (e1, e2)) =
    (compileToBytecode e1) @ (compileToBytecode e2) @ [Arith ADD]" |
  "compileToBytecode (Or (e1, e2)) =
    (compileToBytecode e1) @ (compileToBytecode e2) @ [Bits inst_AND]" |
  "compileToBytecode (Value (Integer n)) = [Stack (PUSH_N (intToBytes n))]" |
  "compileToBytecode (Value (Bool b)) = [Stack (PUSH_N [boolToWord b])]"

fun instToInts :: "inst \<Rightarrow> int list" where
  "instToInts inst = map (\<lambda> w. unat w) (inst_code inst)"

export_code findType instToInts compileToBytecode Plus TInt Integer int_of_integer integer_of_int Right inst_code in OCaml
  file "src/compiler_theory.ml"
