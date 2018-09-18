(* Copyright Per Lindgren 2018, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* cimp/Lexer.mll *)

{
 open Parser
 open Lexing
 open Common
 open Error
 open State__State
 open Env

 exception SyntaxError of string

 (* helpers *)
 let badd buf lexbuf = Buffer.add_string buf (Lexing.lexeme lexbuf)
}

(* regular expressions (regexps) *)
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id      = ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']*
let digits  = ['0'-'9']+
let strings = ['"']_*?['"']
let hex     = ['0']['x']['0'-'9' 'a'-'f']+
let bin     = ['0']['b']['0'-'1']+


(* lexing rules *)
rule lex = parse
  | "IF"    | "if"       { IF }
  | "ELSE"  | "else"     { ELSE }
  | "WHILE" | "while"    { WHILE }

  | '{'                  { LC }
  | '}'                  { RC }

  | "TRUE"  | "true"     { TRUE }
  | "FALSE" | "false"    { FALSE }
  | "&&"                 { AND }
  | "NOT"   | '!'        { NOT }
  | "="                  { BEQ }
  | "<"                  { BLT }
  | ">"                  { BGT }

  | "SINT"               { SINT }
  | "UINT32"             { UINT32 }

  | ';'                  { SC }
  | ':'                  { C }
  | ":="                 { ASSIGN }

  | '+'                  { PLUS }
  | "+u"                 { PLUSU }
  | '-'                  { MINUS }

  | "++"                 { INC }
  | "--"                 { DEC }

  | digits as i          { INTVAL (int_of_string i) }           (* literals/values *)
  | strings as s         { STRINGVAL (s) }                      (* strings *)
  | hex as x             { HEXVAL (int_of_string x) }           (* hex literals *)
  | bin as b             { BINVAL (int_of_string b) }           (* binary literals *)

  | id as s              { ID (add_id s) }
  | white                { lex lexbuf }                         (* white space *)
  | newline              { next_line lexbuf; lex lexbuf }       (* new line *)
  | "//"                 { set_info lexbuf; comment lexbuf }    (* single line comment *)
  | "(*"                 { set_info lexbuf; comments 0 lexbuf } (* nested comment *)
  | '('                  { LP }                                 (* must come after comment *)
  | ')'                  { RP }

  | eof                  { EOF }
  | _                    { raise (SyntaxError("Unknown Symbol.")) }

and comment = parse
  | newline              { next_line lexbuf; lex lexbuf }
  | eof                  { EOF }                                (* // at last line *)
  | _                    { comment lexbuf; }

and comments level = parse
  | "*)"                 { if level = 0 then lex lexbuf else comments (level-1) lexbuf }
  | "(*"                 { comments (level+1) lexbuf }
  | newline              { next_line lexbuf; comments level lexbuf }
  | _                    { comments level lexbuf }
  | eof                  { bol lexbuf; raise (SyntaxError("Comment not closed.")) }
