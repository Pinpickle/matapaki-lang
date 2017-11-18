module HOL : sig
  type 'a equal = {equal : 'a -> 'a -> bool}
  val equal : 'a equal -> 'a -> 'a -> bool
  val eq : 'a equal -> 'a -> 'a -> bool
end = struct

type 'a equal = {equal : 'a -> 'a -> bool};;
let equal _A = _A.equal;;

let rec eq _A a b = equal _A a b;;

end;; (*struct HOL*)

module Arith : sig
  type int = Int_of_integer of Big_int.big_int
  val integer_of_int : int -> Big_int.big_int
  val plus_int : int -> int -> int
end = struct

type int = Int_of_integer of Big_int.big_int;;

let rec integer_of_int (Int_of_integer k) = k;;

let rec plus_int
  k l = Int_of_integer
          (Big_int.add_big_int (integer_of_int k) (integer_of_int l));;

end;; (*struct Arith*)

module Product_Type : sig
  val equal_unit : unit HOL.equal
end = struct

let rec equal_unita u v = true;;

let equal_unit = ({HOL.equal = equal_unita} : unit HOL.equal);;

end;; (*struct Product_Type*)

module Compiler : sig
  type astType = TInt | TBool
  type ('a, 'b) either = Right of 'a | Left of 'b
  type astValue = Integer of Arith.int | Bool of bool
  type astExpression = Plus of (astExpression * astExpression) |
    Or of (astExpression * astExpression) | Value of astValue
  val step : astExpression -> (astExpression, unit) either
  val findType : astExpression -> (astType, unit) either
end = struct

type astType = TInt | TBool;;

let rec equal_astTypea x0 x1 = match x0, x1 with TInt, TBool -> false
                         | TBool, TInt -> false
                         | TBool, TBool -> true
                         | TInt, TInt -> true;;

let equal_astType = ({HOL.equal = equal_astTypea} : astType HOL.equal);;

type ('a, 'b) either = Right of 'a | Left of 'b;;

type astValue = Integer of Arith.int | Bool of bool;;

type astExpression = Plus of (astExpression * astExpression) |
  Or of (astExpression * astExpression) | Value of astValue;;

let rec step
  = function
    Plus (Value (Integer n1), Value (Integer n2)) ->
      Right (Value (Integer (Arith.plus_int n1 n2)))
    | Plus (Value (Bool va), expression) ->
        (match step expression
          with Right result -> Right (Plus (Value (Bool va), result))
          | Left a -> Left a)
    | Plus (Value v, Plus va) ->
        (match step (Plus va)
          with Right result -> Right (Plus (Value v, result))
          | Left a -> Left a)
    | Plus (Value v, Or va) ->
        (match step (Or va) with Right result -> Right (Plus (Value v, result))
          | Left a -> Left a)
    | Plus (Value v, Value (Bool vb)) ->
        (match step (Value (Bool vb))
          with Right result -> Right (Plus (Value v, result))
          | Left a -> Left a)
    | Plus (Plus v, expression2) ->
        (match step (Plus v)
          with Right result -> Right (Plus (result, expression2))
          | Left a -> Left a)
    | Plus (Or v, expression2) ->
        (match step (Or v)
          with Right result -> Right (Plus (result, expression2))
          | Left a -> Left a)
    | Or (Value (Bool b1), Value (Bool b2)) -> Right (Value (Bool (b1 || b2)))
    | Or (Plus vc, vb) -> Left ()
    | Or (Or vc, vb) -> Left ()
    | Or (Value (Integer vd), vb) -> Left ()
    | Or (va, Plus vc) -> Left ()
    | Or (va, Or vc) -> Left ()
    | Or (va, Value (Integer vd)) -> Left ()
    | Value v -> Left ();;

let rec equal_either _A _B
  x0 x1 = match x0, x1 with Right x1, Left x2 -> false
    | Left x2, Right x1 -> false
    | Left x2, Left y2 -> HOL.eq _B x2 y2
    | Right x1, Right y1 -> HOL.eq _A x1 y1;;

let rec findType
  = function
    Plus (expression1, expression2) ->
      (let type1 = findType expression1 in
       let type2 = findType expression2 in
        (if equal_either equal_astType Product_Type.equal_unit type1
              (Right TInt) &&
              equal_either equal_astType Product_Type.equal_unit type2
                (Right TInt)
          then Right TInt else Left ()))
    | Value (Integer uu) -> Right TInt
    | Value (Bool uv) -> Right TBool
    | Or (expression1, expression2) ->
        (let type1 = findType expression1 in
         let type2 = findType expression2 in
          (if equal_either equal_astType Product_Type.equal_unit type1
                (Right TBool) &&
                equal_either equal_astType Product_Type.equal_unit type2
                  (Right TBool)
            then Right TBool else Left ()));;

end;; (*struct Compiler*)
