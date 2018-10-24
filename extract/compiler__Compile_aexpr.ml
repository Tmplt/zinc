let rec compile_aexpr :
  'a . Imp__Imp.aexpr -> ('a Logic__Compiler_logic.hl) =
  fun a ->
    let c =
      begin match a with
      | Imp__Imp.Anum n ->
        Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.iconstf n)
      | Imp__Imp.Avar x ->
        Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.ivarf x)
      | Imp__Imp.Aadd (a1, a2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              compile_aexpr a1))
                                            (Logic__Compiler_logic.prefix_dl (
                                               compile_aexpr a2)))
          (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.iaddf ()))
      | Imp__Imp.Aaddu (a1, a2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              compile_aexpr a1))
                                            (Logic__Compiler_logic.prefix_dl (
                                               compile_aexpr a2)))
          (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.iadduf ()))
      | Imp__Imp.Asub (a1, a2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              compile_aexpr a1))
                                            (Logic__Compiler_logic.prefix_dl (
                                               compile_aexpr a2)))
          (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.isubf ()))
      | Imp__Imp.Asubu (a1, a2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              compile_aexpr a1))
                                            (Logic__Compiler_logic.prefix_dl (
                                               compile_aexpr a2)))
          (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.isubuf ()))
      end in Logic__Compiler_logic.hoare c

let compile_aexpr_natural (a: Imp__Imp.aexpr) : Vm__Vm.instr list =
  let res = compile_aexpr a in res

