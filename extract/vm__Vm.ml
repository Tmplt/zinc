type pos = Z.t

type stack = (Z.t) list

type machine_state =
  | VMS of (Z.t) * ((Z.t) -> (Z.t)) * ((Z.t) list) *
      (State__State.id -> (Z.t))

type ofs = Z.t

type instr =
  | Iload of (Z.t) * State__State.id
  | Iimm of (Z.t) * (Z.t)
  | Istore of (Z.t) * State__State.id
  | Ipushr of (Z.t)
  | Ipopr of (Z.t)
  | Iaddr of (Z.t) * (Z.t) * (Z.t)
  | Iaddur of (Z.t) * (Z.t) * (Z.t)
  | Isubr of (Z.t) * (Z.t) * (Z.t)
  | Isubur of (Z.t) * (Z.t) * (Z.t)
  | Ibeqr of (Z.t) * (Z.t) * (Z.t)
  | Ibner of (Z.t) * (Z.t) * (Z.t)
  | Ibler of (Z.t) * (Z.t) * (Z.t)
  | Ibgtr of (Z.t) * (Z.t) * (Z.t)
  | Iconst of (Z.t)
  | Ivar of State__State.id
  | Isetvar of State__State.id
  | Ibranch of (Z.t)
  | Iadd
  | Iaddu
  | Isub
  | Isubu
  | Ibeq of (Z.t)
  | Ibne of (Z.t)
  | Ible of (Z.t)
  | Ibgt of (Z.t)
  | Ihalt

type code = instr list

let push (n: Z.t) (s: (Z.t) list) : (Z.t) list = n :: s

let iimm (x: Z.t) (n: Z.t) : instr list = (Iimm (x, n)) :: []

let iload (x: Z.t) (n: State__State.id) : instr list = (Iload (x, n)) :: []

let istore (x: Z.t) (n: State__State.id) : instr list = (Istore (x, n)) :: []

let ipushr (x: Z.t) : instr list = (Ipushr x) :: []

let ipopr (x: Z.t) : instr list = (Ipopr x) :: []

let iaddr (x1: Z.t) (x2: Z.t) (x3: Z.t) : instr list =
  (Iaddr (x1, x2, x3)) :: []

let iaddur (x1: Z.t) (x2: Z.t) (x3: Z.t) : instr list =
  (Iaddur (x1, x2, x3)) :: []

let isubr (x1: Z.t) (x2: Z.t) (x3: Z.t) : instr list =
  (Isubr (x1, x2, x3)) :: []

let isubur (x1: Z.t) (x2: Z.t) (x3: Z.t) : instr list =
  (Isubur (x1, x2, x3)) :: []

let ibeqr (x1: Z.t) (x2: Z.t) (ofs: Z.t) : instr list =
  (Ibeqr (x1, x2, ofs)) :: []

let ibner (x1: Z.t) (x2: Z.t) (ofs: Z.t) : instr list =
  (Ibner (x1, x2, ofs)) :: []

let ibler (x1: Z.t) (x2: Z.t) (ofs: Z.t) : instr list =
  (Ibler (x1, x2, ofs)) :: []

let ibgtr (x1: Z.t) (x2: Z.t) (ofs: Z.t) : instr list =
  (Ibgtr (x1, x2, ofs)) :: []

let iconst (n: Z.t) : instr list = (Iconst n) :: []

let ivar (x: State__State.id) : instr list = (Ivar x) :: []

let isetvar (x: State__State.id) : instr list = (Isetvar x) :: []

let iadd  : instr list = Iadd :: []

let iaddu  : instr list = Iaddu :: []

let isub  : instr list = Isub :: []

let isubu  : instr list = Isubu :: []

let ibeq (ofs: Z.t) : instr list = (Ibeq ofs) :: []

let ible (ofs: Z.t) : instr list = (Ible ofs) :: []

let ibne (ofs: Z.t) : instr list = (Ibne ofs) :: []

let ibgt (ofs: Z.t) : instr list = (Ibgt ofs) :: []

let ibranch (ofs: Z.t) : instr list = (Ibranch ofs) :: []

let ihalt  : instr list = Ihalt :: []

