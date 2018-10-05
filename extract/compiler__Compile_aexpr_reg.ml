let rec compile_aexpr :
  'a . Imp__Imp.aexpr -> (Z.t) -> ('a Logic__Compiler_logic.hl) =
  fun a idr ->
    let c =
      begin match a with
      | Imp__Imp.Anum n ->
        Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.iimmf idr n)
      | Imp__Imp.Avar x ->
        Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.iloadf idr x)
      | Imp__Imp.Aadd (a1, a2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              compile_aexpr a1 idr))
                                            (Logic__Compiler_logic.prefix_dl (
                                               compile_aexpr a2
                                                 (Z.add idr Z.one))))
          (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.iaddrf (Z.add idr Z.one)
                                              idr idr))
      | Imp__Imp.Aaddu (a1, a2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              compile_aexpr a1 idr))
                                            (Logic__Compiler_logic.prefix_dl (
                                               compile_aexpr a2
                                                 (Z.add idr Z.one))))
          (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.iaddurf (Z.add idr Z.one)
                                              idr idr))
      | Imp__Imp.Asub (a1, a2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              compile_aexpr a2 idr))
                                            (Logic__Compiler_logic.prefix_dl (
                                               compile_aexpr a1
                                                 (Z.add idr Z.one))))
          (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.isubrf (Z.add idr Z.one)
                                              idr idr))
      end in Logic__Compiler_logic.hoare c

let compile_aexpr_natural (a: Imp__Imp.aexpr) (idr: Z.t) : Vm__Vm.instr list
  = let res = compile_aexpr a idr in res

