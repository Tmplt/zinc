(* Copyright Per Lindgren 2016-2018, see the file "LICENSE" *)
(* for the full license governing this code.                *)

(* cimp/Parser.mly *)

%token <State__State.id> ID
%token <int> INTVAL
%token <string> STRINGVAL
%token <int> HEXVAL
%token <int> BINVAL
%token IF ELSE WHILE
%token CURLO CURLC
%token TRUE FALSE AND NOT BEQ BLT BGT
%token SC LP RP ASSIGN PLUS PLUSU MINUS
%token INC DEC
%token EOF

%left CURLO CURLC
%left NOT
%left PLUS PLUSU MINUS
%left AND
%left SC

%{
  open Imp__Imp
  open Common
  open Env
  open State__State

  let _zero = Anum (Z.of_int 0)
  let _one  = Anum (Z.of_int 1)
%}

%start prog

%type <Imp__Imp.com> prog

%%

prog:
  | com EOF                                           { $1 }

com:
  | com SC com                                        { Cseq ($1, $3) }

  (* Assigning new variables *)
  | ID ASSIGN aexpr SC                                { Cassign ($1, $3) }
  | ID ASSIGN aexpr SC com                            { Cseq (Cassign ($1, $3), $5) }

  (* Incrementing/decrementing a variable *)
  (* TODO: consider if this is worth keeping *)
  | ID INC SC                                         { Cassign ($1, Aadd (Avar $1, _one)) }
  | ID INC SC com                                     { Cseq (Cassign ($1, Aadd (Avar $1, _one)), $4) }
  | ID DEC SC                                         { Cassign ($1, Asub (Avar $1, _one)) }
  | ID DEC SC com                                     { Cseq (Cassign ($1, Asub (Avar $1, _one)), $4) }

  (* if-else statements *)
  | IF bexpr CURLO com CURLC                          { Cif ($2, $4, Cskip) }
  | IF bexpr CURLO com CURLC com                      { Cseq (Cif ($2, $4, Cskip), $6) }
  | IF bexpr CURLO com CURLC ELSE CURLO com CURLC     { Cif ($2, $4, $8) }
  | IF bexpr CURLO com CURLC ELSE CURLO com CURLC com { Cseq (Cif ($2, $4, $8), $10) }

  (* while loops *)
  | WHILE bexpr CURLO com CURLC                       { Cwhile ($2, $4) }
  | WHILE bexpr CURLO com CURLC com                   { Cseq (Cwhile ($2, $4), $6) }

  | com SC                                            { $1 }

bexpr:
  | LP bexpr RP                                       { $2 }
  | TRUE                                              { Btrue }
  | FALSE                                             { Bfalse }
  | bexpr AND bexpr                                   { Band ($1, $3) }
  | NOT bexpr                                         { Bnot ($2) }
  | aexpr BEQ aexpr                                   { Beq ($1, $3) }
  | aexpr NOT BEQ aexpr                               { Bnot (Beq ($1, $4)) }
  | aexpr BLT aexpr                                   { let sub = Asub ($1, $3) in
                                                        let leq = Ble (sub, _zero) in
                                                        let neq = Bnot (Beq (sub, _zero)) in
                                                        Band (leq, neq)
                                                      }
  | aexpr BGT aexpr                                   { Bnot (Ble ($1, $3)) }
  | aexpr BLT BEQ aexpr                               { Ble ($1, $4) }
  | aexpr BGT BEQ aexpr                               { Ble ($4, $1) }

aexpr:
  | LP aexpr RP                                       { $2 }
  | INTVAL | HEXVAL | BINVAL                          { Anum (Z.of_int $1) }
  | ID                                                { Avar $1 }
  | aexpr PLUS aexpr                                  { Aadd ($1, $3) }
  | aexpr PLUSU aexpr                                 { Aaddu ($1, $3) }
  | aexpr MINUS aexpr                                 { Asub ($1, $3) }
  | MINUS aexpr                                       { Asub (_zero, $2) }

