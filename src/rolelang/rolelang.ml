module Ast = Ast

let parse message =
  let lexbuf = Lexing.from_string message in
  try Ok (Parser.ast Lexer.token lexbuf) with
  | Lexer.LexicalError (pos1, pos2) ->
      Error ("Lexical error", pos1, pos2)
  | Parser_aux.SyntaxError (pos1, pos2) ->
      Error ("Syntax error", pos1, pos2)
