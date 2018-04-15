{
  open Lexing
  open Parser
  open Compiler_theory

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { pos with pos_bol = lexbuf.lex_curr_pos;
                  pos_lnum = pos.pos_lnum + 1
        }
}

let int = ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int { INT (Big_int.big_int_of_string (Lexing.lexeme lexbuf)) }
  | "let" { LET }
  | ";" { SEMICOLON }
  | "+" { PLUS }
  | "-" { MINUS }
  | "&&" { AND }
  | "||" { OR }
  | "Write" { WRITE }
  | "Read" { READ }
  | "Paying" { PAYING }
  | "ReadEnv" { READ_ENVIRONMENT }
  | "Payable" { PAYABLE }
  | "Effect" { TEFFECT }
  | "=" { EQUALS }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | "export" { EXPORT }
  | "this.address" { THIS_ADDRESS }
  | "this.balance" { THIS_BALANCE }
  | "message.sender" { MESSAGE_SENDER }
  | "message.value" { MESSAGE_VALUE }
  | "{" { RECORD_OPEN }
  | "}" { RECORD_CLOSE }
  | "[" { SQUARE_OPEN }
  | "]" { SQUARE_CLOSE }
  | "(" { LEFT_PAREN }
  | ")" { RIGHT_PAREN }
  | "," { COMMA }
  | "." { PERIOD }
  | "*" { MULTIPLY }
  | "/" { DIVIDE }
  | "%" { MOD }
  | ">" { GREATER }
  | ">=" { GREATER_EQUAL }
  | "<" { LESSER }
  | "<=" { LESSER_EQUAL }
  | "Uint" { TUINT }
  | "Bool" { TBOOL }
  | "Address" { TADDRESS }
  | ":" { COLON }
  | "->" { ARROW }
  | "fun" { FUN }
  | "!" { EXCLAMATION }
  | "State" { STATE }
  | "as" { AS }
  | "with" { WITH }
  | "@" { AT }
  | "send" { SEND }
  | "to" { TO }
  | "if" { IF }
  | "require" { REQUIRE }
  | "then" { THEN }
  | "else" { ELSE }
  | "with_state" { WITH_STATE }
  | "updating_state" { UPDATING_STATE }
  | identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
