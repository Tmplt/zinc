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
%token SC C LP RP ASSIGN PLUS PLUSU MINUS
%token INC DEC
%token EOF

%token SINT UINT32

%left CURLO CURLC
%left NOT
%left PLUS PLUSU MINUS
%left AND
%left SC

%{
  open T_Imp
  open Common
  open Env
  open State__State

  let _zero = Anum (Z.of_int 0)
  let _one  = Anum (Z.of_int 1)
%}

%start prog

%type <T_Imp.prog> prog

%%
prog:
  | decl_span  SC com_span EOF                                            { Prog ($1, $3) }

decl_span:
  | decl                                                                  { ($1, ($startofs, $endofs)) }
decl:
  | decl_span SC decl_span                                                { Dseq ($1, $3) }
  | ID C primtype                                                         { Ddecl ($1, $3) }

primtype:
  | SINT                                                                  { Tsint }
  | UINT32                                                                { Tuint32 }

com_span:
  | com                                                                   { ($1, ($startofs, $endofs)) }
com:
  | com_span SC com_span                                                  { Cseq ($1, $3) }

  (* Assigning new variables *)
  | ID ASSIGN aexpr_span SC                                               { Cassign ($1, $3) }
  | ID ASSIGN aexpr_span SC com_span                                      { Cseq (Cassign ($1, $3), $5) }

  (* Incrementing/decrementing a variable *)
  (* TODO: consider if this is worth keeping *)
  | ID INC SC                                                             { Cassign ($1, Aadd (Avar $1, _one)) }
  | ID INC SC com_span                                                    { Cseq (Cassign ($1, Aadd (Avar $1, _one)), $4) }
  | ID DEC SC                                                             { Cassign ($1, Asub (Avar $1, _one)) }
  | ID DEC SC com_span                                                    { Cseq (Cassign ($1, Asub (Avar $1, _one)), $4) }

  (* if-else statements *)
  | IF bexpr_span CURLO com_span CURLC                                    { Cif ($2, $4, Cskip) }
  | IF bexpr_span CURLO com_span CURLC com_span                           { Cseq (Cif ($2, $4, Cskip), $6) }
  | IF bexpr_span CURLO com_span CURLC ELSE CURLO com_span CURLC          { Cif ($2, $4, $8) }
  | IF bexpr_span CURLO com_span CURLC ELSE CURLO com_span CURLC com_span { Cseq (Cif ($2, $4, $8), $10) }

  (* while loops *)
  | WHILE bexpr_span CURLO com_span CURLC                                 { Cwhile ($2, $4) }
  | WHILE bexpr_span CURLO com_span CURLC com_span                        { Cseq (Cwhile ($2, $4), $6) }

  | com SC                                                                { $1 }

bexpr_span:
  | bexpr                                                                 { ($1, ($startofs, $endofs)) }
bexpr:
  | LP bexpr RP                                                           { $2 }
  | TRUE                                                                  { Btrue }
  | FALSE                                                                 { Bfalse }
  | bexpr_span AND bexpr_span                                             { Band ($1, $3) }
  | NOT bexpr_span                                                        { Bnot ($2) }
  | aexpr_span BEQ aexpr_span                                             { Beq ($1, $3) }
  | aexpr_span NOT BEQ aexpr_span                                         { Bnot (Beq ($1, $4)) }
  | aexpr_span BLT aexpr_span                                             { let sub = Asub ($1, $3) in
                                                                            let leq = Ble (sub, _zero) in
                                                                            let neq = Bnot (Beq (sub, _zero)) in
                                                                            Band (leq, neq)
                                                                          }
  | aexpr BGT aexpr                                                       { Bnot (Ble ($1, $3)) }
  | aexpr BLT BEQ aexpr                                                   { Ble ($1, $4) }
  | aexpr BGT BEQ aexpr                                                   { Ble ($4, $1) }

aexpr_span:
  | aexpr                                                                 { ($1, ($startofs, $endofs)) }
aexpr:
  | LP aexpr RP                                                           { $2 }
  | INTVAL | HEXVAL | BINVAL                                              { Anum (Z.of_int $1) }
  | ID                                                                    { Avar $1 }
  | aexpr_span PLUS aexpr_span                                            { Aadd ($1, $3) }
  | aexpr_span PLUSU aexpr_span                                           { Aaddu ($1, $3) }
  | aexpr_span MINUS aexpr_span                                           { Asub ($1, $3) }
  | MINUS aexpr_span                                                      { Asub (_zero, $2) }
