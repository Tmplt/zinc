let ifunf (code_f: Vm__Vm.instr list) : 'a Logic__Compiler_logic.hl = code_f

let iimmf (x: Z.t) (n: Z.t) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.iimm x
                                                                    n)))

let iloadf (x: Z.t) (n: State__State.id) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.iload x
                                                                    n)))

let istoref (x: Z.t) (n: State__State.id) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.istore x
                                                                    n)))

let ipushf (x: Z.t) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.ipushr x)))

let ipopf (x: Z.t) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.ipopr x)))

let iaddrf (x1: Z.t) (x2: Z.t) (x3: Z.t) : 'a Logic__Compiler_logic.hl =
  let c = Logic__Compiler_logic.prefix_dl (ifunf (Vm__Vm.iaddr x1 x2 x3)) in
  Logic__Compiler_logic.hoare c

let iaddurf (x1: Z.t) (x2: Z.t) (x3: Z.t) : 'a Logic__Compiler_logic.hl =
  let c = Logic__Compiler_logic.prefix_dl (ifunf (Vm__Vm.iaddur x1 x2 x3)) in
  Logic__Compiler_logic.hoare c

let isubrf (x1: Z.t) (x2: Z.t) (x3: Z.t) : 'a Logic__Compiler_logic.hl =
  let c = Logic__Compiler_logic.prefix_dl (ifunf (Vm__Vm.isubr x1 x2 x3)) in
  Logic__Compiler_logic.hoare c

let isuburf (x1: Z.t) (x2: Z.t) (x3: Z.t) : 'a Logic__Compiler_logic.hl =
  let c = Logic__Compiler_logic.prefix_dl (ifunf (Vm__Vm.isubur x1 x2 x3)) in
  Logic__Compiler_logic.hoare c

let ibeqrf (x1: Z.t) (x2: Z.t) (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.ibeqr x1
                                                                    x2 ofs)))

let ibnerf (x1: Z.t) (x2: Z.t) (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.ibner x1
                                                                    x2 ofs)))

let iblerf (x1: Z.t) (x2: Z.t) (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.ibler x1
                                                                    x2 ofs)))

let ibgtrf (x1: Z.t) (x2: Z.t) (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.ibgtr x1
                                                                    x2 ofs)))

let iconstf (n: Z.t) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.iconst n)))

let ivarf (x: State__State.id) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.ivar x)))

type binop = (Z.t) -> ((Z.t) -> (Z.t))

let create_binop (code_b: Vm__Vm.instr list) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf code_b))

let iaddf (_2: unit) : 'a Logic__Compiler_logic.hl = create_binop Vm__Vm.iadd

let iadduf (_2: unit) : 'a Logic__Compiler_logic.hl =
  create_binop Vm__Vm.iaddu

let isubf (_2: unit) : 'a Logic__Compiler_logic.hl = create_binop Vm__Vm.isub

let isubuf (_2: unit) : 'a Logic__Compiler_logic.hl =
  create_binop Vm__Vm.isubu

let inil (_2: unit) : 'a Logic__Compiler_logic.hl = []

let ibranchf (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  let cf = Logic__Compiler_logic.prefix_dl (ifunf (Vm__Vm.ibranch ofs)) in
  Logic__Compiler_logic.hoare cf

type cond = (Z.t) -> ((Z.t) -> (bool))

let create_cjump (code_cd: Vm__Vm.instr list) : 'a Logic__Compiler_logic.hl =
  let c = Logic__Compiler_logic.prefix_dl (ifunf code_cd) in
  Logic__Compiler_logic.hoare c

let ibeqf (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  create_cjump (Vm__Vm.ibeq ofs)

let ibnef (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  create_cjump (Vm__Vm.ibne ofs)

let iblef (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  create_cjump (Vm__Vm.ible ofs)

let ibgtf (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  create_cjump (Vm__Vm.ibgt ofs)

let isetvarf (x: State__State.id) : 'a Logic__Compiler_logic.hl =
  let c = Logic__Compiler_logic.prefix_dl (ifunf (Vm__Vm.isetvar x)) in
  Logic__Compiler_logic.hoare c

