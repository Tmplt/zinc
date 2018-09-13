(* Copyright Per Lindgren 2016-2018, see the file "LICENSE" *)
(* for the full license governing this code.                *)

open State__State
open Vm__Vm
open Env
open Common
open T_Imp

(* AST dump of T_Imp *)

let of_types = function 
  | Tsint   -> "SINT"
  | Tuint32 -> "UINT32"
  | Tint    -> "INT"

let of_id = function
  | Id i -> "Id #" ^ Z.to_string i

let rec of_aexpr = function
  | Anum v          -> "Anum " ^ Z.to_string v
  | Avar id         -> "Avar " ^ of_id id
  | Aadd (e1, e2)   -> "Aadd (" ^ of_aexpr_span e1 ^ ") (" ^ of_aexpr_span e2 ^ ")"
  | Aaddu (e1, e2)  -> "Aaddu (" ^ of_aexpr_span e1 ^ ") (" ^ of_aexpr_span e2 ^")"
  | Asub (e1, e2)   -> "Asub (" ^ of_aexpr_span e1 ^ ") (" ^ of_aexpr_span e2 ^")"
and of_aexpr_span (e, (start, stop)) = 
  "< (" ^ string_of_int start ^ ", " ^ string_of_int stop ^ 
  ") " ^ of_aexpr e ^ ">"

let rec of_bexpr = function
  | Btrue           -> "Btrue"
  | Bfalse          -> "Bfalse"
  | Band (b1, b2)   -> "Band (" ^ of_bexpr_span b1 ^ ") (" ^ of_bexpr_span b2 ^ ")"
  | Bnot b          -> "Bnot (" ^ of_bexpr_span b ^ ")"
  | Beq (e1, e2)    -> "Beq (" ^ of_aexpr_span e1 ^ ") (" ^ of_aexpr_span e2 ^ ")"
  | Ble (e1, e2)    -> "Ble (" ^ of_aexpr_span e1 ^ ") (" ^ of_aexpr_span e2 ^ ")"
and of_bexpr_span (e, s) = of_bexpr e

let rec of_com = function
  | Cskip           -> "Cskip"
  | Cassign (id, a) -> "Cassign " ^ of_id id ^ " " ^ of_aexpr_span a
  | Cseq (c1, c2)   -> "Cseq (" ^ of_com_span c1 ^ ") (" ^ of_com_span c2 ^ ")"
  | Cif (b, c1, c2) -> "Cif (" ^ of_bexpr_span b ^ ") (" ^ of_com_span c1 ^ " " ^ of_com_span c2 ^ ")"
  | Cwhile (b, c)   -> "Cwhile (" ^ of_bexpr_span b ^ ") (" ^ of_com_span c ^ ")"
and of_com_span (c, (start, stop)) = 
  "< (" ^ string_of_int start ^ ", " ^ string_of_int stop ^ 
  ") " ^ of_com c ^ ">"

let rec of_decl = function
  | Dseq (d1, d2) -> "Dseq (" ^ of_decl_span d1 ^ ", " ^ of_decl_span d2 ^ ")"
  | Ddecl (id, t) -> "Ddecl (" ^ of_id id ^ ", " ^ of_types t ^ ")" 
and of_decl_span (d,_) = of_decl d

let of_prog = function
  | Prog (d, c) -> of_decl_span d ^ of_com_span c

