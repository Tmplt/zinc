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
  "sw $1, 0($sp)"

(* pop value into passed register (temporary) *)
let pop (reg:int) =
  "lw $t" ^ string_of_int reg ^ ", 0($sp)" ^ nl ^
  decstack

(* pop value into passed register (without changing $sp) *)
let pop' (reg:int) =
  "lw $" ^ string_of_int reg ^ ", 0($sp)" ^ nl

let ofs_of_id id =
  match id with
  | Id i -> Z.to_string (Z.mul i (Z.of_int 4))
  | _ -> assert false

let string_of_reg idr = "$" ^ Z.to_string idr
let string_of_label label = "label_" ^ Z.to_string label ^ ":" ^ nl
let string_of_label' label = "label_" ^ Z.to_string label ^ nl

let of_instr currlabel = function
  (* new instructions, register based *)
  | Iload (idr, id) -> (* load register with variable *)
    "lw $" ^ Z.to_string idr ^ ", " ^ ofs_of_id id ^ "($gp)"
  | Iimm (idr, n)   -> (* load register with value n *)
    "li $" ^ Z.to_string idr ^ ", " ^ Z.to_string n
  | Istore (idr, id) -> (* store a register to variable *)
    "sw $" ^ Z.to_string idr ^ ", " ^ ofs_of_id id ^ "($gp)"
  | Ipushr idr       -> (* push register on stack *)
    incstack ^
    "lw $" ^ Z.to_string idr ^ ", 0($sp)"
  | Ipopr idr        -> (* pop from stack into register *)
    "lw $" ^ Z.to_string idr ^ ", 0($sp)" ^
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
    pushi n
  | Ivar id         -> (* push the value of id onto stack *)
    begin
      match id with
      | Id i -> pushi i
      | _ -> assert false
    end
  | Isetvar id      -> (* pop an integer, assign it to a variable *)
    pop 1 ^
    "sw $t1, " ^ ofs_of_id id ^ "($gp)"
  | Ibranch ofs     -> (* skip ofs instructions *)
    "b " ^ string_of_label' (currlabel + ofs)
  | Iadd            -> (* pop two values, push their sum *)
    (* pop the values *)
    pop 1 ^
    pop' 2 ^
    (* push their sum *)
    "add $t1, $1, $2"    ^ nl ^
    "sw $t1, 0($sp)"
  | Iaddu           -> (* pop two values, push their sum (wrapping) *)
    (* pop the values *)
    pop 1 ^
    pop' 2 ^
    (* push their sum (wrapping) *)
    "addu $t1, $t1, $t2"   ^ nl ^
    "sw $t1, 0($sp)"
  | Isub            -> (* pop two values, push their difference *)
    pop 1 ^
    pop' 2 ^
    (* push their difference *)
    "sub $t1, $t1, $t2" ^ nl ^
    "sw $t1, 0($sp)"
  | Ibeq ofs        -> (* pop a, b, skip ofs forward if a = b *)
    pop 1 ^
    pop 2 ^
    "beq $t1, $t2, " ^ string_of_label' (currlabel + ofs)
  | Ibne ofs        -> (* pop a, b, skip ofs forward if a != b *)
    pop 1 ^
    pop 2 ^
    "bne $t1, $t2, " ^ string_of_label' (currlabel + ofs)
  | Ible ofs        -> (* pop a, b, skip ofs forward if b <= a *)
    pop 1 ^ (* a *)
    pop 2 ^ (* b *)
    (* b <= a -> a > b -> a - b > 0 *)
    "sub $t0, $t1, $t2" ^ nl ^
    "bgtz $t0, " ^ string_of_label' (currlabel + ofs)
  | Ibgt ofs        -> (* pop a, b, skip ofs forward if b > a *)
    pop 1 ^ (* a *)
    pop 2 ^ (* b *)
    (* b > a -> b - a > 0 *)
    "sub $t0, $t2, $t1" ^ nl ^
    "bgtz $t0, " ^ string_of_label' (currlabel + ofs)
  | Ihalt           -> (* end of program, loop forevermore *)
    "b " ^ string_of_label' currlabel

(* TODO: final newline is applied here, strip it from of_instr above *)
let of_code code =
  let rec of_code' c labelid =
    match c with
    | [] -> ""
    | i :: il ->
        string_of_label labelid ^
        of_instr labelid i ^ nl ^ nl ^ of_code' il (labelid + (Z.of_int 1))
  in
  ".text" ^ nl ^
  ".set noreorder" ^ nl ^ nl ^
  of_code' code (Z.of_int 0)

(* vim: shiftwidth=2:
*)
