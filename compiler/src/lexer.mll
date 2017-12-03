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

let int = '-' ? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "+" { BINARY_OPERATOR Ast.Plus }
  | "-" { BINARY_OPERATOR Ast.Minus }
  | "&&" { BINARY_OPERATOR Ast.And }
  | "||" { BINARY_OPERATOR Ast.Or }
  | "true" { BOOL true }
  | "false" { BOOL false } 
  | "(" { LEFT_PAREN }
  | ")" { RIGHT_PAREN }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
