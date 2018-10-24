type aexpr =
  | Anum of (Z.t)
  | Avar of State__State.id
  | Aadd of aexpr * aexpr
  | Asub of aexpr * aexpr
  | Aaddu of aexpr * aexpr
  | Asubu of aexpr * aexpr

type bexpr =
  | Btrue
  | Bfalse
  | Bnot of bexpr
  | Beq of aexpr * aexpr
  | Ble of aexpr * aexpr
  | Band of bexpr * bexpr

type com =
  | Cskip
  | Cassign of State__State.id * aexpr
  | Cseq of com * com
  | Cif of bexpr * com * com
  | Cwhile of bexpr * com

