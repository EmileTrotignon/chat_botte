{ open Parser

exception LexicalError of string * Lexing.position * Lexing.position

}

let and_regexp = "&" | "/\\"

let or_regexp = "|" | "\\/" | "\\\\/"

let number = ['0' - '9']

let whitespace = [' ' '\n' '\t' '\r']*

rule token  =
  parse
  whitespace {
      token lexbuf }
  | "@everyone" {
      TId (Ast.Everyone)}
| "<@!" (number*) ">" {
      let matched = Lexing.lexeme lexbuf in
      let n = String.length matched in
      let id = String.sub matched 2 (n - 3) in
      let id = int_of_string id in
      TId (Ast.User id) }
| "<@&" (number*) ">" {
      let matched = Lexing.lexeme lexbuf in
      let n = String.length matched in
      let id = String.sub matched 3 (n - 4) in
      let id = int_of_string id in
      TId (Ast.Role id) }

  | and_regexp {
      TAnd }
  | or_regexp {
      TOr }
  | "(" {
      LPar }
  | ")" {
      RPar }
  | eof {
      EOM }
  | _ {
      let string = Lexing.lexeme lexbuf
      and pos_start = Lexing.lexeme_start_p lexbuf
      and pos_end = Lexing.lexeme_end_p lexbuf in
      raise (LexicalError (string, pos_start, pos_end)) }

{
}
