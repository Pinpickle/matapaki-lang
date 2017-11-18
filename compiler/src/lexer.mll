{
  open Lexing
  open Parser

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
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LEFT_PAREN }
  | ")" { RIGHT_PAREN }
  | "+" { PLUS }
  | "|" { OR }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
