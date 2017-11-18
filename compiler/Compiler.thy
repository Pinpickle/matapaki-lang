theory Compiler
  imports Main "~~/src/HOL/Library/Code_Target_Int" "./eth-isabelle/ContractSem" "./eth-isabelle/RelationalSem" "./eth-isabelle/ProgramInAvl"
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

fun step :: "astExpression => (astExpression, unit) either" where
  "step (Plus ((Value (Integer n1)), (Value (Integer n2)))) = Right (Value (Integer (n1 + n2)))" |
  "step (Plus ((Value v), expression)) = (
    case (step expression) of
      Right result => Right (Plus (Value v, result)) |
      error => error
    )" |
  "step (Plus (expression1, expression2)) = (
    case (step expression1) of
      Right result => Right (Plus (result, expression2)) |
      error => error
    )" |
  "step (Or ((Value (Bool b1)), (Value (Bool b2)))) = Right (Value (Bool (b1 | b2)))" |
  "step _ = Left ()"

export_code findType step Plus TInt Integer int_of_integer integer_of_int Right in OCaml
  file "src/compiler_theory.ml"
