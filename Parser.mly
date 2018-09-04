(* Copyright Per Lindgren 2016-2018, see the file "LICENSE" *)
(* for the full license governing this code.                *)

(* cimp/Parser.mly *)

%token <State__State.id> ID
%token <int> INTVAL
%token <string> STRINGVAL
%token IF THEN ELSE END WHILE DO DONE
%token TRUE FALSE AND NOT BEQ BLE
%token SC LP RP ASSIGN PLUS PLUSU MINUS
%token EOF

%left PLUS PLUSU MINUS  (* lowest precedence *)
%left AND NOT           (* medium precedence *)
%nonassoc SC            (* highest precedence *)

%{
  open Imp__Imp
  open Common
  open Env
  open State__State
%}

%start prog

%type <Imp__Imp.com> prog

%%

prog:
  | com EOF                        { $1 }

com:
  | com SC com                     { Cseq ($1, $3) }
  | ID ASSIGN aexpr                { Cassign ($1, $3) }
  | IF bexpr THEN com ELSE com END { Cif ($2, $4, $6) }
  | IF bexpr THEN com END          { Cif ($2, $4, Cskip) }
  | WHILE bexpr DO com DONE        { Cwhile ($2, $4) }

bexpr:
  | LP bexpr RP                    { $2 }
  | TRUE                           { Btrue }
  | FALSE                          { Bfalse }
  | bexpr AND bexpr                { Band ($1, $3) }
  | NOT bexpr                      { Bnot ($2) }
  | aexpr BEQ aexpr                { Beq ($1, $3) }
  | aexpr BLE aexpr                { Ble ($1, $3) }

aexpr:
  | LP aexpr RP                    { $2 }
  | INTVAL                         { Anum (Z.of_int $1) }
  | ID                             { Avar $1 }
  | aexpr PLUS aexpr               { Aadd ($1, $3) }
  | aexpr PLUSU aexpr              { Aaddu ($1, $3) }
  | aexpr MINUS aexpr              { Asub ($1, $3) }

