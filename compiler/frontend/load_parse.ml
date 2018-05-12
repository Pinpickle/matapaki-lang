open Core.Std
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let parse_and_print verbose lexbuf =
  match parse_with_error lexbuf with
  | Some ast -> (
    if verbose then (
      print_endline "Parsed AST";
      print_endline (Pretty_print.pretty_print_ast_program ast)
    );
    match Compile.either_type_of_program ast with
      | Right typed_ast -> print_endline (Yojson.to_string (Output.program_and_type_to_json typed_ast))
      | Left () -> fprintf stderr "Type error")
  | None -> ()

let loop verbose filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print verbose lexbuf;
  In_channel.close inx
