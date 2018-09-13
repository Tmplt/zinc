(* Copyright Per Lindgren 2016-2018, see the file "LICENSE" *)
(* for the full license governing this code.                *)

(* type checker *)
open T_Imp
open T_Dump
open Common
open State__State


module Imp = Imp__Imp

let of_span inb (start, stop) = 
  let _ = seek_in inb start in
  let s = really_input_string inb (stop - start) in
  "<" ^ string_of_int start ^ ".." ^ string_of_int stop ^ "> " ^ s

let unique_id chan (id1, (t1, s1)) (id2, (t2, s2)) = 
  if id1 = id2 then 
    raise (CompilerError("Dupclicate variable definition: " ^
                         of_span chan s1 ^ " already declared at " ^  of_span chan s2)) 
  else ()

let rec idt_acc ch sp acc = function 
  | Dseq (d1, d2) ->  (idt_acc_span ch (idt_acc_span ch acc d1) d2) 
  | Ddecl (id, t) -> List.iter (unique_id ch (id, (t, sp))) acc;
    (id, (t, sp)) :: acc

and idt_acc_span c acc (d, s) = idt_acc c s acc d

let of_idt ch (id, (t,s)) =
  of_id id ^ ":" ^ of_types t ^ of_span ch s


let tc_unify ch t1 t2 s2 : types =
  match t1, t2 with
  | Tsint,    Tsint     -> Tsint
  | Tuint32,  Tuint32   -> Tuint32
  | Tint,     t         -> t
  | t,        Tint      -> t
  | _,        _         ->
    raise (TypeError(
        "Type error: Expected " ^  of_types t1 ^ 
        " got "  ^ of_types t2 ^ " in:" ^ of_span ch s2))

let tc_unify2 ch t1 s1 t2 s2 : types =
  match t1, t2 with
  | Tsint,    Tsint     -> Tsint
  | Tuint32,  Tuint32   -> Tuint32
  | Tint,     t         -> t
  | t,        Tint      -> t
  | _,        _         ->
    raise (TypeError(
        "Type error: " ^ of_types t1 ^ " in:" ^ of_span ch s1 ^
        " does not match " ^ of_types t2 ^ " in:" ^ of_span ch s2))

let get_id_type itl (id : id) : types * span =
  try
    List.assoc id itl
  with 
    _ -> raise (TypeError("Undeclared identifier: " ^ Dump.pretty_of_id id))

let rec tc_aexpr ch itl (a, span) : Imp.aexpr * types =  
  (* try *)
  match a with
  | Anum n -> (Imp.Anum n, Tint)
  | Avar id -> 
    let (t, _ ) = get_id_type itl id in
    (Imp.Avar id, t)
  | Aadd ((a1, a1_span), (a2, a2_span)) -> 
    (* type check a1 against Tsint *)
    let (ai1, t1) = tc_aexpr ch itl (a1, a1_span) in
    let _ = tc_unify ch Tsint t1 a1_span in

    (* type check a2 against Tsint *)
    let (ai2, t2) = tc_aexpr ch itl (a2, a2_span) in
    let _ = tc_unify ch Tsint t2 a2_span in

    (Imp.Aadd(ai1, ai2), Tsint) 
  | Aaddu ((a1, a1_span), (a2, a2_span)) -> 
    (* type check a1 against Tuint32 *)
    let (ai1, t1) = tc_aexpr ch itl (a1, a1_span) in
    let _ = tc_unify ch Tuint32 t1 a1_span in

    (* type check a2 against Tuint32 *)
    let (ai2, t2) = tc_aexpr ch itl (a2, a2_span) in
    let _ = tc_unify ch Tuint32 t2 a2_span in

    (Imp.Aaddu(ai1, ai2), t1)

  | Asub ((a1, a1_span), (a2, a2_span)) -> 
    (* type check a1 against Tsint *)
    let (ai1, t1) = tc_aexpr ch itl (a1, a1_span) in
    let _ = tc_unify ch Tsint t1 a1_span in

    (* type check a2 against Tsint *)
    let (ai2, t2) = tc_aexpr ch itl (a2, a2_span) in
    let _ = tc_unify ch Tsint t2 a2_span in

    (Imp.Asub(ai1, ai2), t1)  
(* with
   | TypeError msg -> raise (TypeError (msg ^ nl ^ "in expression:" ^ of_span ch span )) *)

let rec tc_bexpr ch itl (b, span) = 
  try
    match b with
    | Btrue -> Imp.Btrue
    | Bfalse -> Imp.Bfalse
    | Band (b1, b2) -> Imp.Band(tc_bexpr ch itl b1,tc_bexpr ch itl b2)
    | Bnot b -> Imp.Bnot(tc_bexpr ch itl b)
    | Beq ((a1, a1_span),(a2, a2_span)) ->
      let (a1, t1) = tc_aexpr ch itl (a1, a1_span) in
      let (a2, t2) = tc_aexpr ch itl (a2, a2_span) in
      let _ = tc_unify2 ch t1 a1_span t2 a2_span in
      Imp.Beq(a1, a2)
    | Ble ((a1,a1_span), (a2, a2_span)) -> 
      (* type check a1 against Tsint *) 
      let (a1, t1) = tc_aexpr ch itl (a1, a1_span) in
      let _ = tc_unify ch Tsint t1 a1_span in

      (* type check a2 against Tsint *) 
      let (a2, t2) = tc_aexpr ch itl (a2, a2_span) in
      let _ = tc_unify ch Tsint t2 a2_span in
      Imp.Ble(a1,  a2)
  with
  | TypeError msg -> raise (TypeError (msg ^ nl ^ "in expression:" ^ of_span ch span ))


let rec tc_com ch itl span com = 
  try 
    match com with
    | Cseq (c1, c2) -> Imp.Cseq(tc_com_span ch itl c1, tc_com_span ch itl c2)
    | Cassign (id, a) ->
      let (_, a_span) = a in
      let (a, ta) = tc_aexpr ch itl a in
      let (tid, tid_span) = get_id_type itl id in
      let _ = tc_unify2 ch ta a_span tid tid_span in 
      Imp.Cassign (id, a)
    | Cif (b, c1, c2)  -> Imp.Cif(tc_bexpr ch itl b, tc_com_span ch itl c1, tc_com_span ch itl c2)
    | Cwhile (b, c) -> Imp.Cwhile(tc_bexpr ch itl b, tc_com_span ch itl c)
    | Cskip -> Imp.Cskip
  with
  | TypeError msg -> raise (CompilerError (msg ^ nl ^ "in command: " ^ of_span ch span ))
and
  tc_com_span ch itl (com, span)  = tc_com ch itl span com

let tc_prog ch (Prog (decl, com)) = 
  let itl = idt_acc_span ch [] decl in
  tc_com_span ch itl com 

