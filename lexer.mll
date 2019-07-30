{
  open Parser
  open Lexing
  exception Lexing_error of string

  let kw = [
    "break", BREAK ;
    "continue", CONTINUE ;
    "def", DEF ;
    "effect", EFFECT ;
    "else", ELSE ;
    "exception", EXCEPTION ;
    "for", FOR ;
    "if", IF ;
    "in", IN ;
    "match", MATCH ;
    "of", OF ;
    "perform", PERFORM ;
    "rec", REC ;
    "return", RETURN ;
    "type", TYPE ;
    "val", VAL ;
    "while", WHILE ;
  ]

  let keywords = Hashtbl.create (List.length kw)
  let () = List.iter (fun (name, token) -> Hashtbl.add keywords name token) kw

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1 ; pos_bol = pos.pos_cnum }
}

let digits = ['0'-'9']
let hex_digits = ['0'-'9' 'a'-'f' 'A'-'F']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let alpha = lowercase | uppercase | digits | '_'
let lident = lowercase alpha*
let uident = uppercase alpha*
let integer = digits+ | "0x" hex_digits+
let whitespace = [ ' ' '\t' ]

rule token = parse
  | whitespace   { token lexbuf }
  | "//"         { comment_line lexbuf }
  | "/*"         { comment_region lexbuf }
  | "\n"         { newline lexbuf; token lexbuf }
  | eof          { EOF }
  | integer as s { INT s }
  | lident as s  { try Hashtbl.find keywords s with Not_found -> LIDENT s }
  | uident as s  { UIDENT s }
  | "("          { LPAREN }
  | ")"          { RPAREN }
  | "["          { LBRACK }
  | "]"          { RBRACK }
  | "{"          { LBRACE }
  | "}"          { RBRACE }
  | "[|"         { LBRACKBAR }
  | "|]"         { BARRBRACK }
  | "{|"         { LBRACEBAR }
  | "|}"         { BARRBRACE }
  | "|"          { BAR }
  | ","          { COMMA }
  | ":"          { COLON }
  | "::"         { COLONCOLON }
  | "."          { DOT }
  | "="          { EQ }
  | "=>"         { EQGT }
  | "=="         { EQEQ }
  | "?"          { INTERRO }
  | ";"          { SEMI }
  | "*"          { STAR }
  | _            { raise (Lexing_error "Illegal character") }

and comment_line = parse
  | "\n" { newline lexbuf; token lexbuf }
  | _    { comment_line lexbuf }
  | eof  { EOF }

and comment_region = parse
  | "\n" { newline lexbuf; comment_region lexbuf }
  | "*/" { token lexbuf }
  | _    { comment_region lexbuf }
  | eof  { raise (Lexing_error "Unterminated comment") }
