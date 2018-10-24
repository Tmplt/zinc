(* Copyright Per Lindgren 2016-2018, see the file "LICENSE" *)
(* for the full license governing this code.                *)

open Imp__Imp
open State__State
open Vm__Vm
open Env
open Common

(* AST dump of native data structures *)
let of_id = function
  | Id i -> "Id #" ^ Z.to_string i

let rec of_aexpr = function
  | Anum v          -> "Anum " ^ Z.to_string v
  | Avar id         -> "Avar " ^ of_id id
  | Aadd (e1, e2)   -> "Aadd (" ^ of_aexpr e1 ^ ") (" ^ of_aexpr e2 ^")"
  | Aaddu (e1, e2)  -> "Aaddu (" ^ of_aexpr e1 ^ ") (" ^ of_aexpr e2 ^")"
  | Asub (e1, e2)   -> "Asub (" ^ of_aexpr e1 ^ ") (" ^ of_aexpr e2 ^")"

let rec of_bexpr = function
  | Btrue           -> "Btrue"
  | Bfalse          -> "Bfalse"
  | Band (b1, b2)   -> "Band (" ^ of_bexpr b1 ^ ") (" ^ of_bexpr b2 ^ ")"
  | Bnot b          -> "Bnot (" ^ of_bexpr b ^ ")"
  | Beq (e1, e2)    -> "Beq (" ^ of_aexpr e1 ^ ") (" ^ of_aexpr e2 ^ ")"
  | Ble (e1, e2)    -> "Ble (" ^ of_aexpr e1 ^ ") (" ^ of_aexpr e2 ^ ")"

let rec of_com = function
  | Cskip           -> "Cskip"
  | Cassign (id, a) -> "Cassign " ^ of_id id ^ " " ^ of_aexpr a
  | Cseq (c1, c2)   -> "Cseq (" ^ of_com c1 ^ ") (" ^ of_com c2 ^ ")"
  | Cif (b, c1, c2) -> "Cif (" ^ of_bexpr b ^ ") (" ^ of_com c1 ^ ") (" ^ of_com c2 ^ ")"
  | Cwhile (b, c)   -> "Cwhile (" ^ of_bexpr b ^ ") (" ^ of_com c ^ ")"

(* AST dump for the IMP lanaguage *)
let pretty_of_id id = get_id id

let rec pretty_of_aexpr = function
  | Anum v          -> Z.to_string v
  | Avar id         -> pretty_of_id id
  | Aadd (e1, e2)   -> "(" ^ pretty_of_aexpr e1 ^ " + " ^ pretty_of_aexpr e2 ^")"
  | Aaddu (e1, e2)  -> "(" ^ pretty_of_aexpr e1 ^ " +u " ^ pretty_of_aexpr e2 ^")"
  | Asub (e1, e2)   -> "(" ^ pretty_of_aexpr e1 ^ " - " ^ pretty_of_aexpr e2 ^")"

let rec pretty_of_bexpr = function
  | Btrue           -> "TRUE"
  | Bfalse          -> "FALSE"
  | Band (b1, b2)   -> "(" ^ pretty_of_bexpr b1 ^ " && " ^ pretty_of_bexpr b2 ^ ")"
  | Bnot b          -> "NOT " ^ pretty_of_bexpr b
  | Beq (e1, e2)    -> "(" ^ pretty_of_aexpr e1 ^ " == " ^ pretty_of_aexpr e2 ^ ")"
  | Ble (e1, e2)    -> "(" ^ pretty_of_aexpr e1 ^ " <= " ^ pretty_of_aexpr e2 ^ ")"

let rec tab t =
  match t > 0 with
  | true -> "  " ^ tab (t - 1) (* indent two spaces *)
  | _ -> ""


let rec pretty_of_com t = function
  | Cskip           -> ""
  | Cassign (id, a) -> tab t ^ pretty_of_id id ^ " := " ^ pretty_of_aexpr a
  | Cseq (c1, c2)   -> pretty_of_com t c1 ^ ";\n" ^ pretty_of_com t c2
  | Cif (b, c1, c2) -> tab t ^ "IF " ^ pretty_of_bexpr b ^ " THEN\n" ^
                       pretty_of_com (t + 1) c1 ^ "\n" ^
                       tab t ^ "ELSE\n" ^
                       pretty_of_com (t + 1) c2 ^ "\n" ^
                       tab t ^ "END"
  | Cwhile (b, c)   -> tab t ^ "WHILE " ^ pretty_of_bexpr b ^ " DO\n" ^
                       pretty_of_com (t + 1) c ^ "\n" ^
                       tab t ^ "DONE"

let of_instr b = function
  | Iconst v        -> "Iconst " ^ Z.to_string v
  | Ivar id    -> "Ivar " ^ (
      match b with
      | false -> of_id id
      | true  -> get_id id
    )
  | Isetvar id -> "Isetvar " ^ (
      match b with
      | false -> of_id id
      | true  -> get_id id
    )
  | Ibranch n       -> "Ibranch " ^ Z.to_string n
  | Iadd            -> "Iadd"
  | Iaddu           -> "Iaddu"
  | Isub            -> "Isub"
  | Ibeq n          -> "Ibeq " ^ Z.to_string n
  | Ibne n          -> "Ibne " ^ Z.to_string n
  | Ible n          -> "Ible " ^ Z.to_string n
  | Ibgt n          -> "Ibgt " ^ Z.to_string n
  | Ihalt           -> "Ihalt"

let rec of_code b = function
  | [] -> ""
  | i :: il -> of_instr b i ^ ";\n" ^ of_code b il
