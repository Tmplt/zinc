
module Imp = Imp__Imp 

type types =
  | Tsint
  | Tuint32
  | Tint (* int litterals can be coersed to both Tsint and Tuint32 *)

type span = int * int

type aexpr =
  | Anum of (Z.t)
  | Avar of State__State.id
  | Aadd of aexpr_span * aexpr_span
  | Aaddu of aexpr_span * aexpr_span
  | Asub of aexpr_span * aexpr_span
and
  aexpr_span = aexpr * span

type bexpr =
  | Btrue
  | Bfalse
  | Band of bexpr_span * bexpr_span
  | Bnot of bexpr_span
  | Beq of aexpr_span * aexpr_span
  | Ble of aexpr_span * aexpr_span
and bexpr_span = bexpr * span

type com =
  | Cskip
  | Cseq of com_span * com_span
  | Cassign of State__State.id * aexpr_span
  | Cif of bexpr_span * com_span * com_span
  | Cwhile of bexpr_span * com_span
and com_span = com * span

type decl =
  | Dseq of decl_span * decl_span
  | Ddecl of State__State.id * types 
and decl_span = decl * span

type prog = Prog of decl_span * com_span

(* Convert to Imp *)
let rec imp_of_aexpr = function
  | Anum n -> Imp.Anum n
  | Avar id -> Imp.Avar id
  | Aadd (a1, a2) -> Imp.Aadd(imp_of_aexpr_span a1, imp_of_aexpr_span a2) 
  | Aaddu (a1, a2) -> Imp.Aaddu(imp_of_aexpr_span a1, imp_of_aexpr_span a2) 
  | Asub (a1, a2) -> Imp.Asub(imp_of_aexpr_span a1, imp_of_aexpr_span a2) 
and
  imp_of_aexpr_span (a, _) = imp_of_aexpr a

let rec imp_of_bexpr = function
  | Btrue -> Imp.Btrue
  | Bfalse -> Imp.Bfalse
  | Band (b1, b2) -> Imp.Band(imp_of_bexpr_span b1,imp_of_bexpr_span b2)
  | Bnot b -> Imp.Bnot(imp_of_bexpr_span b)
  | Beq (a1, a2) -> Imp.Beq(imp_of_aexpr_span a1, imp_of_aexpr_span a2)
  | Ble (a1, a2) -> Imp.Ble(imp_of_aexpr_span a1, imp_of_aexpr_span a2)
and
  imp_of_bexpr_span (b, _) = imp_of_bexpr b

let rec imp_of_com = function
  | Cskip -> Imp.Cskip
  | Cseq (c1, c2) -> Imp.Cseq(imp_of_com_span c1,imp_of_com_span c2)
  | Cassign (id, a) -> Imp.Cassign (id, imp_of_aexpr_span a)
  | Cif (b, c1, c2)  -> Imp.Cif(imp_of_bexpr_span b, imp_of_com_span c1, imp_of_com_span c2)
  | Cwhile (b, c) -> Imp.Cwhile(imp_of_bexpr_span b, imp_of_com_span c)
and
  imp_of_com_span (c, _) = imp_of_com c





