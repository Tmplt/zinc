(* Copyright Viktor Sonesten 2018, see the file "LICENSE" *)
(* for the full license governing this code              *)

open Imp__Imp
open State__State
open Vm__Vm
open Env
open Common

(* XXX: How should these be implemented? See why3/vm.mlw *)

let of_id = function
  | Id i -> "Id #" ^ Z.to_string i

(* Push n onto the stack via temporary registers *)
let pushi (n:Z.t) =
  "li $t0, " ^ Z.to_string n ^ nl ^
  "addiu $sp, $sp -4" ^ nl ^
  "sw $1, 0($sp)" ^ nl

(* pop value into passed register (temporary) *)
let pop (reg:int) =
  "lw $t" ^ string_of_int reg ^ ", 0($sp)" ^ nl ^
  "addiu $sp, $sp, 4" ^ nl

(* pop value into passed register (without changing $sp) *)
let pop' (reg:int) =
  "lw $" ^ string_of_int reg ^ ", 0($sp)" ^ nl

let of_instr b = function
  | Iconst n        -> (* push n on stack *)
    pushi n
  | Ivar id         -> (* push the value of id onto stack *)
    pushi (Z.of_string (get_id id))
  | Isetvar id      -> (* pop an integer, assign it to a variable *)
    let id_to_ofs id =
      match id with
      | Id i -> Z.to_string (Z.mul i (Z.of_int 4))
      | _ -> assert false
    in
    pop 1 ^
    "sw $t1, " ^ id_to_ofs id ^ "($gp)" ^ nl
  | Ibranch ofs     -> (* skip ofs instructions *)
    "b " ^ Z.to_string ofs ^ nl
  | Iadd            -> (* pop two values, push their sum *)
    (* pop the values *)
    pop 1 ^
    pop' 2 ^
    (* push their sum *)
    "add $t1, $1, $2"    ^ nl ^
    "sw $t1, 0($sp)"     ^ nl
  | Iaddu           -> (* pop two values, push their sum (wrapping) *)
    (* pop the values *)
    pop 1 ^
    pop' 2 ^
    (* push their sum (wrapping) *)
    "addu $t1, $t1, $t2"   ^ nl ^
    "sw $t1, 0($sp)"     ^ nl
  | Isub            -> (* pop two values, push their difference *)
    pop 1 ^
    pop' 2 ^
    (* push their difference *)
    "sub $t1, $t1, $t2" ^ nl ^
    "sw $t1, 0($sp)" ^ nl
  | Ibeq ofs        -> (* pop a, b, skip ofs forward if a = b *)
    pop 1 ^
    pop 2 ^
    "beq $t1, $t2, " ^ Z.to_string ofs ^ nl
  | Ibne ofs        -> (* pop a, b, skip ofs forward if a != b *)
    pop 1 ^
    pop 2 ^
    "bne $t1, $t2, " ^ Z.to_string ofs ^ nl
  | Ible ofs        -> (* pop a, b, skip ofs forward if b <= a *)
    pop 1 ^ (* a *)
    pop 2 ^ (* b *)
    (* b <= a -> a > b -> a - b > 0 *)
    "sub $t0, $t1, $t2" ^ nl ^
    "bgtz $t0, " ^ Z.to_string ofs ^ nl
  | Ibgt ofs        -> (* pop a, b, skip ofs forward if b > a *)
    pop 1 ^ (* a *)
    pop 2 ^ (* b *)
    (* b > a -> b - a > 0 *)
    "sub $t0, $t2, $t1" ^ nl ^
    "bgtz $t0, " ^ Z.to_string ofs ^ nl
  | Ihalt           -> (* end of program, loop forevermore *)
    "b 0"

let rec of_code b = function
  | [] -> ""
  | i :: il -> of_instr b i ^ nl ^ of_code b il
