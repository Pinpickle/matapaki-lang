module Finite_Set : sig
  type 'a finite = unit
end = struct

type 'a finite = unit;;

end;; (*struct Finite_Set*)

module HOL : sig
  type 'a equal = {equal : 'a -> 'a -> bool}
  val equal : 'a equal -> 'a -> 'a -> bool
  type 'a itself = Type
  val eq : 'a equal -> 'a -> 'a -> bool
end = struct

type 'a equal = {equal : 'a -> 'a -> bool};;
let equal _A = _A.equal;;

type 'a itself = Type;;

let rec eq _A a b = equal _A a b;;

end;; (*struct HOL*)

module Product_Type : sig
  val equal_unit : unit HOL.equal
  val apsnd : ('a -> 'b) -> 'c * 'a -> 'c * 'b
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
end = struct

let rec equal_unita u v = true;;

let equal_unit = ({HOL.equal = equal_unita} : unit HOL.equal);;

let rec apsnd f (x, y) = (x, f y);;

let rec fst (x1, x2) = x1;;

let rec snd (x1, x2) = x2;;

end;; (*struct Product_Type*)

module Orderings : sig
  type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool}
  val less_eq : 'a ord -> 'a -> 'a -> bool
  val less : 'a ord -> 'a -> 'a -> bool
  val max : 'a ord -> 'a -> 'a -> 'a
end = struct

type 'a ord = {less_eq : 'a -> 'a -> bool; less : 'a -> 'a -> bool};;
let less_eq _A = _A.less_eq;;
let less _A = _A.less;;

let rec max _A a b = (if less_eq _A a b then b else a);;

end;; (*struct Orderings*)

module Fun : sig
  val comp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
end = struct

let rec comp f g = (fun x -> f (g x));;

end;; (*struct Fun*)

module Arith : sig
  type int = Int_of_integer of Big_int.big_int
  type num = One | Bit0 of num | Bit1 of num
  val one_inta : int
  type 'a one
  val integer_of_int : int -> Big_int.big_int
  type 'a times
  type 'a power
  val power_int : int power
  type nat
  val nat : int -> nat
  val one_nat : nat
  val suc : nat -> nat
  val minus_nat : nat -> nat -> nat
  val equal_nat : nat -> nat -> bool
  val zero_nat : nat
  val power : 'a power -> 'a -> nat -> 'a
  val less_nat : nat -> nat -> bool
  val int_of_nat : nat -> int
  val plus_int : int -> int -> int
  val zero_int : int
  val nat_of_integer : Big_int.big_int -> nat
  val equal_int : int -> int -> bool
  val times_nat : nat -> nat -> nat
  val divide_int : int -> int -> int
  val modulo_int : int -> int -> int
end = struct

type int = Int_of_integer of Big_int.big_int;;

type num = One | Bit0 of num | Bit1 of num;;

let one_inta : int = Int_of_integer (Big_int.big_int_of_int 1);;

type 'a one = {one : 'a};;
let one _A = _A.one;;

let one_int = ({one = one_inta} : int one);;

let rec integer_of_int (Int_of_integer k) = k;;

let rec times_inta
  k l = Int_of_integer
          (Big_int.mult_big_int (integer_of_int k) (integer_of_int l));;

type 'a times = {times : 'a -> 'a -> 'a};;
let times _A = _A.times;;

type 'a power = {one_power : 'a one; times_power : 'a times};;

let times_int = ({times = times_inta} : int times);;

let power_int = ({one_power = one_int; times_power = times_int} : int power);;

let ord_integer =
  ({Orderings.less_eq = Big_int.le_big_int; Orderings.less = Big_int.lt_big_int}
    : Big_int.big_int Orderings.ord);;

type nat = Nat of Big_int.big_int;;

let rec nat
  k = Nat (Orderings.max ord_integer Big_int.zero_big_int (integer_of_int k));;

let rec integer_of_nat (Nat x) = x;;

let rec plus_nat
  m n = Nat (Big_int.add_big_int (integer_of_nat m) (integer_of_nat n));;

let one_nat : nat = Nat (Big_int.big_int_of_int 1);;

let rec suc n = plus_nat n one_nat;;

let rec minus_nat
  m n = Nat (Orderings.max ord_integer Big_int.zero_big_int
              (Big_int.sub_big_int (integer_of_nat m) (integer_of_nat n)));;

let rec equal_nat
  m n = Big_int.eq_big_int (integer_of_nat m) (integer_of_nat n);;

let zero_nat : nat = Nat Big_int.zero_big_int;;

let rec power _A
  a n = (if equal_nat n zero_nat then one _A.one_power
          else times _A.times_power a (power _A a (minus_nat n one_nat)));;

let rec less_nat
  m n = Big_int.lt_big_int (integer_of_nat m) (integer_of_nat n);;

let rec int_of_nat n = Int_of_integer (integer_of_nat n);;

let rec plus_int
  k l = Int_of_integer
          (Big_int.add_big_int (integer_of_int k) (integer_of_int l));;

let zero_int : int = Int_of_integer Big_int.zero_big_int;;

let rec sgn_integer
  k = (if Big_int.eq_big_int k Big_int.zero_big_int then Big_int.zero_big_int
        else (if Big_int.lt_big_int k Big_int.zero_big_int
               then (Big_int.minus_big_int (Big_int.big_int_of_int 1))
               else (Big_int.big_int_of_int 1)));;

let rec divmod_integer
  k l = (if Big_int.eq_big_int k Big_int.zero_big_int
          then (Big_int.zero_big_int, Big_int.zero_big_int)
          else (if Big_int.eq_big_int l Big_int.zero_big_int
                 then (Big_int.zero_big_int, k)
                 else Fun.comp
                        (Fun.comp Product_Type.apsnd Big_int.mult_big_int)
                        sgn_integer l
                        (if Big_int.eq_big_int (sgn_integer k) (sgn_integer l)
                          then Big_int.quomod_big_int (Big_int.abs_big_int k)
                                 (Big_int.abs_big_int l)
                          else (let (r, s) =
                                  Big_int.quomod_big_int (Big_int.abs_big_int k)
                                    (Big_int.abs_big_int l)
                                  in
                                 (if Big_int.eq_big_int s Big_int.zero_big_int
                                   then (Big_int.minus_big_int r,
  Big_int.zero_big_int)
                                   else (Big_int.sub_big_int
   (Big_int.minus_big_int r) (Big_int.big_int_of_int 1),
  Big_int.sub_big_int (Big_int.abs_big_int l) s))))));;

let rec nat_of_integer
  k = Nat (Orderings.max ord_integer Big_int.zero_big_int k);;

let rec equal_int
  k l = Big_int.eq_big_int (integer_of_int k) (integer_of_int l);;

let rec times_nat
  m n = Nat (Big_int.mult_big_int (integer_of_nat m) (integer_of_nat n));;

let rec divide_integer k l = Product_Type.fst (divmod_integer k l);;

let rec divide_int
  k l = Int_of_integer (divide_integer (integer_of_int k) (integer_of_int l));;

let rec modulo_integer k l = Product_Type.snd (divmod_integer k l);;

let rec modulo_int
  k l = Int_of_integer (modulo_integer (integer_of_int k) (integer_of_int l));;

end;; (*struct Arith*)

module Numeral_Type : sig
  type 'a bit0
  type num1
end = struct

type 'a bit0 = Abs_bit0 of Arith.int;;

type num1 = One_num1;;

end;; (*struct Numeral_Type*)

module Type_Length : sig
  type 'a len0 = {len_of : 'a HOL.itself -> Arith.nat}
  val len_of : 'a len0 -> 'a HOL.itself -> Arith.nat
  type 'a len = {len0_len : 'a len0}
  val len0_bit0 : 'a len0 -> 'a Numeral_Type.bit0 len0
  val len_bit0 : 'a len -> 'a Numeral_Type.bit0 len
  val len0_num1 : Numeral_Type.num1 len0
  val len_num1 : Numeral_Type.num1 len
end = struct

type 'a len0 = {len_of : 'a HOL.itself -> Arith.nat};;
let len_of _A = _A.len_of;;

let rec len_of_bit0 _A
  uu = Arith.times_nat (Arith.nat_of_integer (Big_int.big_int_of_int 2))
         (len_of _A HOL.Type);;

type 'a len = {len0_len : 'a len0};;

let rec len0_bit0 _A = ({len_of = len_of_bit0 _A} : 'a Numeral_Type.bit0 len0);;

let rec len_bit0 _A =
  ({len0_len = (len0_bit0 _A.len0_len)} : 'a Numeral_Type.bit0 len);;

let rec len_of_num1 uu = Arith.one_nat;;

let len0_num1 = ({len_of = len_of_num1} : Numeral_Type.num1 len0);;

let len_num1 = ({len0_len = len0_num1} : Numeral_Type.num1 len);;

end;; (*struct Type_Length*)

module Bit_Representation : sig
  val bin_split : Arith.nat -> Arith.int -> Arith.int * Arith.int
end = struct

let rec bit
  k b = Arith.plus_int
          (Arith.plus_int (if b then Arith.one_inta else Arith.zero_int) k) k;;

let rec bin_last
  w = Arith.equal_int
        (Arith.modulo_int w (Arith.Int_of_integer (Big_int.big_int_of_int 2)))
        Arith.one_inta;;

let rec bin_rest
  w = Arith.divide_int w (Arith.Int_of_integer (Big_int.big_int_of_int 2));;

let rec bin_split
  n w = (if Arith.equal_nat n Arith.zero_nat then (w, Arith.zero_int)
          else (let (w1, w2) =
                  bin_split (Arith.minus_nat n Arith.one_nat) (bin_rest w) in
                 (w1, bit w2 (bin_last w))));;

end;; (*struct Bit_Representation*)

module Bits_Int : sig
  val bin_rsplit : Arith.nat -> Arith.nat * Arith.int -> Arith.int list
end = struct

let rec bin_rsplit_aux
  n m c bs =
    (if Arith.equal_nat m Arith.zero_nat || Arith.equal_nat n Arith.zero_nat
      then bs
      else (let a = Bit_Representation.bin_split n c in
            let (aa, b) = a in
             bin_rsplit_aux n (Arith.minus_nat m n) aa (b :: bs)));;

let rec bin_rsplit
  n w = bin_rsplit_aux n (Product_Type.fst w) (Product_Type.snd w) [];;

end;; (*struct Bits_Int*)

module List : sig
  val map : ('a -> 'b) -> 'a list -> 'b list
  val size_list : 'a list -> Arith.nat
end = struct

let rec map f x1 = match f, x1 with f, [] -> []
              | f, x21 :: x22 -> f x21 :: map f x22;;

let rec gen_length
  n x1 = match n, x1 with n, x :: xs -> gen_length (Arith.suc n) xs
    | n, [] -> n;;

let rec size_list x = gen_length Arith.zero_nat x;;

end;; (*struct List*)

module Word : sig
  type 'a word
  val uint : 'a Type_Length.len0 -> 'a word -> Arith.int
  val unat : 'a Type_Length.len0 -> 'a word -> Arith.nat
  val word_of_int : 'a Type_Length.len0 -> Arith.int -> 'a word
  val word_rsplit :
    'a Type_Length.len0 -> 'b Type_Length.len -> 'a word -> 'b word list
  val one_word : 'a Type_Length.len0 -> 'a word
  val plus_word : 'a Type_Length.len0 -> 'a word -> 'a word -> 'a word
  val zero_word : 'a Type_Length.len0 -> 'a word
end = struct

type 'a word = Word of Arith.int;;

let rec uint _A (Word x) = x;;

let rec unat _A w = Arith.nat (uint _A w);;

let rec word_of_int _A
  k = Word (Arith.modulo_int k
             (Arith.power Arith.power_int
               (Arith.Int_of_integer (Big_int.big_int_of_int 2))
               (Type_Length.len_of _A HOL.Type)));;

let rec word_rsplit _A _B
  w = List.map (word_of_int _B.Type_Length.len0_len)
        (Bits_Int.bin_rsplit
          (Type_Length.len_of _B.Type_Length.len0_len HOL.Type)
          (Type_Length.len_of _A HOL.Type, uint _A w));;

let rec one_word _A = word_of_int _A Arith.one_inta;;

let rec plus_word _A
  a b = word_of_int _A (Arith.plus_int (uint _A a) (uint _A b));;

let rec zero_word _A = word_of_int _A Arith.zero_int;;

end;; (*struct Word*)

module Word8 : sig
  val word8FromNat :
    Arith.nat ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Word.word
end = struct

let rec word8FromNat
  i = Word.word_of_int
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        (Arith.int_of_nat i);;

end;; (*struct Word8*)

module Evm : sig
  type storage_inst
  type sarith_inst
  type memory_inst
  type stack_inst = POP |
    PUSH_N of
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Word.word list
    | CALLDATALOAD
  type arith_inst = ADD | MUL | SUB | DIV | MOD | ADDMOD | MULMOD | EXP |
    Inst_GT | Inst_EQ | Inst_LT | ISZERO | SHA3
  type misc_inst
  type info_inst
  type bits_inst = Inst_AND | Inst_OR | Inst_XOR | Inst_NOT | BYTE
  type log_inst
  type pc_inst
  type inst =
    Unknown of
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Word.word
    | Bits of bits_inst | Sarith of sarith_inst | Arith of arith_inst |
    Info of info_inst |
    Dup of Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Word.word |
    Memory of memory_inst | Storage of storage_inst | Pc of pc_inst |
    Stack of stack_inst |
    Swap of Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Word.word |
    Log of log_inst | Misc of misc_inst
  val inst_code :
    inst ->
      Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
        Word.word list
end = struct

type storage_inst = SLOAD | SSTORE;;

type sarith_inst = SDIV | SMOD | SGT | SLT | SIGNEXTEND;;

type memory_inst = MLOAD | MSTORE | MSTORE8 | CALLDATACOPY | CODECOPY |
  EXTCODECOPY | MSIZE;;

type stack_inst = POP |
  PUSH_N of
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Word.word list
  | CALLDATALOAD;;

type arith_inst = ADD | MUL | SUB | DIV | MOD | ADDMOD | MULMOD | EXP | Inst_GT
  | Inst_EQ | Inst_LT | ISZERO | SHA3;;

type misc_inst = STOP | CREATE | CALL | CALLCODE | DELEGATECALL | RETURN |
  SUICIDE;;

type info_inst = ADDRESS | BALANCE | ORIGIN | CALLER | CALLVALUE | CALLDATASIZE
  | CODESIZE | GASPRICE | EXTCODESIZE | BLOCKHASH | COINBASE | TIMESTAMP |
  NUMBER | DIFFICULTY | GASLIMIT | GAS;;

type bits_inst = Inst_AND | Inst_OR | Inst_XOR | Inst_NOT | BYTE;;

type log_inst = LOG0 | LOG1 | LOG2 | LOG3 | LOG4;;

type pc_inst = JUMP | JUMPI | PC | JUMPDEST;;

type inst =
  Unknown of
    Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Numeral_Type.bit0
      Word.word
  | Bits of bits_inst | Sarith of sarith_inst | Arith of arith_inst |
  Info of info_inst |
  Dup of Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Word.word |
  Memory of memory_inst | Storage of storage_inst | Pc of pc_inst |
  Stack of stack_inst |
  Swap of Numeral_Type.num1 Numeral_Type.bit0 Numeral_Type.bit0 Word.word |
  Log of log_inst | Misc of misc_inst;;

let rec storage_inst_code
  = function
    SLOAD ->
      Word.word_of_int
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        (Arith.Int_of_integer (Big_int.big_int_of_int 84))
    | SSTORE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 85));;

let rec sarith_inst_code
  = function
    SDIV ->
      Word.word_of_int
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        (Arith.Int_of_integer (Big_int.big_int_of_int 5))
    | SMOD ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 7))
    | SGT ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 19))
    | SLT ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 18))
    | SIGNEXTEND ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 11));;

let rec memory_inst_code
  = function
    MLOAD ->
      Word.word_of_int
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        (Arith.Int_of_integer (Big_int.big_int_of_int 81))
    | MSTORE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 82))
    | MSTORE8 ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 83))
    | CALLDATACOPY ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 55))
    | CODECOPY ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 57))
    | EXTCODECOPY ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 60))
    | MSIZE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 89));;

let rec byteFromNat x = Word8.word8FromNat x;;

let rec stack_inst_code
  = function
    POP ->
      [Word.word_of_int
         (Type_Length.len0_bit0
           (Type_Length.len0_bit0
             (Type_Length.len0_bit0 Type_Length.len0_num1)))
         (Arith.Int_of_integer (Big_int.big_int_of_int 80))]
    | PUSH_N lst ->
        (if Arith.less_nat (List.size_list lst) Arith.one_nat
          then [Word.word_of_int
                  (Type_Length.len0_bit0
                    (Type_Length.len0_bit0
                      (Type_Length.len0_bit0 Type_Length.len0_num1)))
                  (Arith.Int_of_integer (Big_int.big_int_of_int 96));
                 Word.word_of_int
                   (Type_Length.len0_bit0
                     (Type_Length.len0_bit0
                       (Type_Length.len0_bit0 Type_Length.len0_num1)))
                   Arith.zero_int]
          else (if Arith.less_nat
                     (Arith.nat_of_integer (Big_int.big_int_of_int 32))
                     (List.size_list lst)
                 then [Word.word_of_int
                         (Type_Length.len0_bit0
                           (Type_Length.len0_bit0
                             (Type_Length.len0_bit0 Type_Length.len0_num1)))
                         (Arith.Int_of_integer (Big_int.big_int_of_int 96));
                        Word.word_of_int
                          (Type_Length.len0_bit0
                            (Type_Length.len0_bit0
                              (Type_Length.len0_bit0 Type_Length.len0_num1)))
                          Arith.zero_int]
                 else [Word.plus_word
                         (Type_Length.len0_bit0
                           (Type_Length.len0_bit0
                             (Type_Length.len0_bit0 Type_Length.len0_num1)))
                         (byteFromNat (List.size_list lst))
                         (Word.word_of_int
                           (Type_Length.len0_bit0
                             (Type_Length.len0_bit0
                               (Type_Length.len0_bit0 Type_Length.len0_num1)))
                           (Arith.Int_of_integer
                             (Big_int.big_int_of_int 95)))] @
                        lst))
    | CALLDATALOAD ->
        [Word.word_of_int
           (Type_Length.len0_bit0
             (Type_Length.len0_bit0
               (Type_Length.len0_bit0 Type_Length.len0_num1)))
           (Arith.Int_of_integer (Big_int.big_int_of_int 53))];;

let rec arith_inst_code
  = function
    ADD ->
      Word.word_of_int
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        Arith.one_inta
    | MUL ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 2))
    | SUB ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 3))
    | DIV ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 4))
    | MOD ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 6))
    | ADDMOD ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 8))
    | MULMOD ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 9))
    | EXP ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 10))
    | Inst_GT ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 17))
    | Inst_LT ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 16))
    | Inst_EQ ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 20))
    | ISZERO ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 21))
    | SHA3 ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 32));;

let rec swap_inst_code
  m = Word.plus_word
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        (Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Word.uint
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1))
            m))
        (Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 144)));;

let rec misc_inst_code
  = function
    STOP ->
      Word.word_of_int
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        Arith.zero_int
    | CREATE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 240))
    | CALL ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 241))
    | CALLCODE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 242))
    | RETURN ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 243))
    | DELEGATECALL ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 244))
    | SUICIDE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 255));;

let rec info_inst_code
  = function
    ADDRESS ->
      Word.word_of_int
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        (Arith.Int_of_integer (Big_int.big_int_of_int 48))
    | BALANCE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 49))
    | ORIGIN ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 50))
    | CALLVALUE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 52))
    | CALLDATASIZE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 54))
    | CALLER ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 51))
    | CODESIZE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 56))
    | GASPRICE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 58))
    | EXTCODESIZE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 59))
    | BLOCKHASH ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 64))
    | COINBASE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 65))
    | TIMESTAMP ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 66))
    | NUMBER ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 67))
    | DIFFICULTY ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 68))
    | GASLIMIT ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 69))
    | GAS ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 90));;

let rec bits_inst_code
  = function
    Inst_AND ->
      Word.word_of_int
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        (Arith.Int_of_integer (Big_int.big_int_of_int 22))
    | Inst_OR ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 23))
    | Inst_XOR ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 24))
    | Inst_NOT ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 25))
    | BYTE ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 26));;

let rec log_inst_code
  = function
    LOG0 ->
      Word.word_of_int
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        (Arith.Int_of_integer (Big_int.big_int_of_int 160))
    | LOG1 ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 161))
    | LOG2 ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 162))
    | LOG3 ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 163))
    | LOG4 ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 164));;

let rec dup_inst_code
  m = Word.plus_word
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        (Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Word.uint
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1))
            m))
        (Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 128)));;

let rec pc_inst_code
  = function
    JUMP ->
      Word.word_of_int
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0 (Type_Length.len0_bit0 Type_Length.len0_num1)))
        (Arith.Int_of_integer (Big_int.big_int_of_int 86))
    | JUMPI ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 87))
    | PC -> Word.word_of_int
              (Type_Length.len0_bit0
                (Type_Length.len0_bit0
                  (Type_Length.len0_bit0 Type_Length.len0_num1)))
              (Arith.Int_of_integer (Big_int.big_int_of_int 88))
    | JUMPDEST ->
        Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1)))
          (Arith.Int_of_integer (Big_int.big_int_of_int 91));;

let rec inst_code = function Unknown byte -> [byte]
                    | Bits b -> [bits_inst_code b]
                    | Sarith s -> [sarith_inst_code s]
                    | Arith a -> [arith_inst_code a]
                    | Info i -> [info_inst_code i]
                    | Dup d -> [dup_inst_code d]
                    | Memory m -> [memory_inst_code m]
                    | Storage s -> [storage_inst_code s]
                    | Pc p -> [pc_inst_code p]
                    | Stack s -> stack_inst_code s
                    | Swap s -> [swap_inst_code s]
                    | Log l -> [log_inst_code l]
                    | Misc m -> [misc_inst_code m];;

end;; (*struct Evm*)

module Compiler : sig
  type astType = TInt | TBool
  type ('a, 'b) either = Right of 'a | Left of 'b
  type astValue = Integer of Arith.int | Bool of bool
  type astExpression = Plus of (astExpression * astExpression) |
    Or of (astExpression * astExpression) | Value of astValue
  val findType : astExpression -> (astType, unit) either
  val instToInts : Evm.inst -> Arith.int list
  val compileToBytecode : astExpression -> Evm.inst list
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

let rec boolToWord
  b = (if b then Word.one_word
                   (Type_Length.len0_bit0
                     (Type_Length.len0_bit0
                       (Type_Length.len0_bit0 Type_Length.len0_num1)))
        else Word.zero_word
               (Type_Length.len0_bit0
                 (Type_Length.len0_bit0
                   (Type_Length.len0_bit0 Type_Length.len0_num1))));;

let rec instToInts
  inst =
    List.map Arith.int_of_nat
      (List.map
        (Word.unat
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0 Type_Length.len0_num1))))
        (Evm.inst_code inst));;

let rec intToBytes
  n = Word.word_rsplit
        (Type_Length.len0_bit0
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0
                (Type_Length.len0_bit0
                  (Type_Length.len0_bit0
                    (Type_Length.len0_bit0
                      (Type_Length.len0_bit0 Type_Length.len0_num1))))))))
        (Type_Length.len_bit0
          (Type_Length.len_bit0 (Type_Length.len_bit0 Type_Length.len_num1)))
        (Word.word_of_int
          (Type_Length.len0_bit0
            (Type_Length.len0_bit0
              (Type_Length.len0_bit0
                (Type_Length.len0_bit0
                  (Type_Length.len0_bit0
                    (Type_Length.len0_bit0
                      (Type_Length.len0_bit0
                        (Type_Length.len0_bit0 Type_Length.len0_num1))))))))
          n);;

let rec compileToBytecode
  = function
    Plus (e1, e2) ->
      compileToBytecode e1 @ compileToBytecode e2 @ [Evm.Arith Evm.ADD]
    | Or (e1, e2) ->
        compileToBytecode e1 @ compileToBytecode e2 @ [Evm.Bits Evm.Inst_AND]
    | Value (Integer n) -> [Evm.Stack (Evm.PUSH_N (intToBytes n))]
    | Value (Bool b) -> [Evm.Stack (Evm.PUSH_N [boolToWord b])];;

end;; (*struct Compiler*)
