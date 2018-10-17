(* Copyright Viktor Sonesten 2018, see the file "LICENSE" *)
(* for the full license governing this code              *)

open Imp__Imp
open State__State
open Vm__Vm
open Common
open Z

(* TODO:
 *  - Add comment above every chunk explaining what it does
 *  - Format generated assembly to be more readable
 *      - Indent with 8 spaces, making space for eventual labels
 *      - tabulate the arguments so that they aling
 *)

let incstack = "addiu $sp, $sp, -4" ^ nl
let decstack = "addiu $sp, $sp, 4" ^ nl

(* Push n onto the stack via temporary registers *)
let pushi (n:Z.t) =
  "li $t0, " ^ Z.to_string n ^ nl ^
  incstack ^
  "sw $t0, 0($sp)"

(* pop value into passed register (temporary) *)
let pop (reg:int) =
  "lw $t" ^ string_of_int reg ^ ", 0($sp)" ^ nl ^
  decstack

(* pop value into passed register (without changing $sp) *)
let pop' (reg:int) =
  "lw $t" ^ string_of_int reg ^ ", 0($sp)" ^ nl

let ofs_of_id id =
  match id with
  | Id i -> Z.to_string (Z.mul i (Z.of_int 4))
  | _ -> assert false

(* Offset the register ID with 2, so that we do not use $zero/$at; they are problematic *)
let string_of_reg idr = "$" ^ Z.to_string (Z.add idr (Z.of_int 2))

let string_of_label label = "label_" ^ Z.to_string label ^ ":" ^ nl
let string_of_label' label = "label_" ^ Z.to_string (Z.add label (Z.of_int 1)) ^ nl

let of_instr currlabel = function
  (* new instructions, register based *)
  | Iload (idr, id) -> (* load register with variable *)
    "lw " ^ string_of_reg idr ^ ", " ^ ofs_of_id id ^ "($gp)"
  | Iimm (idr, n)   -> (* load register with value n *)
    "li " ^ string_of_reg idr ^ ", " ^ Z.to_string n
  | Istore (idr, id) -> (* store a register to variable *)
    "sw " ^ string_of_reg idr ^ ", " ^ ofs_of_id id ^ "($gp)"
  | Ipushr idr       -> (* push register on stack *)
    incstack ^
    "lw " ^ string_of_reg idr ^ ", 0($sp)"
  | Ipopr idr        -> (* pop from stack into register *)
    "lw " ^ string_of_reg idr ^ ", 0($sp)" ^
    decstack
  | Iaddr (lhs, rhs, res) -> (* add two registers, store result in third *)
    "add " ^ string_of_reg res ^ ", " ^ string_of_reg lhs ^ ", " ^ string_of_reg rhs
  | Iaddur (lhs, rhs, res) -> (* add two registers, store result in third (wrapping) *)
    "addu " ^ string_of_reg res ^ ", " ^ string_of_reg lhs ^ ", " ^ string_of_reg rhs
  | Isubr (lhs, rhs, res) -> (* subtract two registers, store result in third *)
    "sub " ^ string_of_reg res ^ ", " ^ string_of_reg lhs ^ ", " ^ string_of_reg rhs
  | Ibeqr (lhs, rhs, ofs) -> (* skip ofs forward if lhs = rhs *)
    "beq " ^ string_of_reg lhs ^ ", " ^ string_of_reg rhs ^ ", " ^ string_of_label' (currlabel + ofs)
  | Ibner (lhs, rhs, ofs) -> (* skip ofs forward if lhs != rhs *)
    "bne " ^ string_of_reg lhs ^ ", " ^ string_of_reg rhs ^ ", " ^ string_of_label' (currlabel + ofs)
  | Ibler (lhs, rhs, ofs) -> (* skip ofs forward if lhs != rhs *)
    (* lhs <= rhs -> rhs > lhs -> rhs - lhs > 0 *)
    "sub $t0, " ^ string_of_reg rhs ^ ", " ^ string_of_reg lhs ^ nl ^
    "bgtz $t0, " ^ string_of_label' (currlabel + ofs)
  | Ibgtr (lhs, rhs, ofs) -> (* skip ofs forward if lhs > rhs *)
    (* lhs > rhs -> lhs - rhs > 0 *)
    "sub $t0, " ^ string_of_reg lhs ^ ", " ^ string_of_reg rhs ^ nl ^
    "bgtz $t0, " ^ string_of_label' (currlabel + ofs)

  (* original/old instructions for stack machine *)
  | Iconst n        -> (* push n on stack *)
    "# Iconst" ^ nl ^
    pushi n
  | Ivar id         -> (* push the value of id onto stack *)
    "# Ivar" ^ nl ^
    "lw $t0, " ^ ofs_of_id id ^ "($gp)" ^ nl ^
    incstack ^
    "sw $t0, 0($sp)"
  | Isetvar id      -> (* pop an integer, assign it to a variable *)
    "# Isetvar" ^ nl ^
    pop 1 ^
    "sw $t1, " ^ ofs_of_id id ^ "($gp)"
  | Ibranch ofs     -> (* skip ofs instructions *)
    "# Ibranch" ^ nl ^
    "b " ^ string_of_label' (currlabel + ofs)
  | Iadd            -> (* pop two values, push their sum *)
    "# Iadd" ^ nl ^
    (* pop the values *)
    pop 1 ^
    pop' 2 ^
    (* push their sum *)
    "add $t1, $t1, $t2"    ^ nl ^
    "sw $t1, 0($sp)"
  | Iaddu           -> (* pop two values, push their sum (wrapping) *)
    "# Iaddu" ^ nl ^
    (* pop the values *)
    pop 1 ^
    pop' 2 ^
    (* push their sum (wrapping) *)
    "addu $t1, $t1, $t2"   ^ nl ^
    "sw $t1, 0($sp)"
  | Isub            -> (* pop two values, push their difference *)
    "# Isub" ^ nl ^
    pop 1 ^
    pop' 2 ^
    (* push their difference *)
    "sub $t1, $t2, $t1" ^ nl ^
    "sw $t1, 0($sp)"
  | Ibeq ofs        -> (* pop a, b, skip ofs forward if a = b *)
    "# Ibeq" ^ nl ^
    pop 1 ^
    pop 2 ^
    "beq $t1, $t2, " ^ string_of_label' (currlabel + ofs)
  | Ibne ofs        -> (* pop a, b, skip ofs forward if a != b *)
    "# Ibne" ^ nl ^
    pop 1 ^
    pop 2 ^
    "bne $t1, $t2, " ^ string_of_label' (currlabel + ofs)
  | Ible ofs        -> (* pop a, b, skip ofs forward if b <= a *)
    "# Ible" ^ nl ^
    pop 1 ^ (* a *)
    pop 2 ^ (* b *)
    (* b <= a -> a > b -> a - b > 0 *)
    "sub $t0, $t1, $t2" ^ nl ^
    "bgtz $t0, " ^ string_of_label' (currlabel + ofs)
  | Ibgt ofs        -> (* pop a, b, skip ofs forward if b > a *)
    "# Ibgt" ^ nl ^
    pop 1 ^ (* a *)
    pop 2 ^ (* b *)
    (* b > a -> b - a > 0 *)
    "sub $t0, $t2, $t1" ^ nl ^
    "bgtz $t0, " ^ string_of_label' (currlabel + ofs)
  | Ihalt           -> (* end of program, loop forevermore *)
    "# Ihalt" ^ nl ^
    "b " ^ string_of_label' (Z.sub currlabel (Z.of_int 1)) (* TODO: fix this decrement *)

let preamble =
  ".data\n" ^
  ".space 1024\n" ^
  ".set noreorder\n" ^ (* Avoid code optimisation *)
  ".set noat\n" ^ (* Disable warnings for accessing $at *)
  ".text\n" ^
  "la $gp, .data\n" (* Set $gp to accessible data area *)

(* TODO: final newline is applied here, strip it from of_instr above *)
let of_code code =
  let rec of_code' c labelid =
    match c with
    | [] -> ""
    | i :: il ->
        string_of_label labelid ^
        of_instr labelid i ^ nl ^ nl ^ of_code' il (labelid + (Z.of_int 1))
  in
  preamble ^ nl ^
  of_code' code (Z.of_int 0)

(* vim: shiftwidth=2:
*)
