let rec compile_bexpr :
  'a . Imp__Imp.bexpr -> (bool) -> (Z.t) -> (Z.t) -> ('a
       Logic__Compiler_logic.hl) =
  fun b cond ofs idr ->
    let c =
      begin match b with
      | Imp__Imp.Btrue ->
        Logic__Compiler_logic.prefix_dl (if cond then begin
                                           Specs__VM_instr_spec.ibranchf ofs end
                                         else
                                         begin
                                           Specs__VM_instr_spec.inil () end)
      | Imp__Imp.Bfalse ->
        Logic__Compiler_logic.prefix_dl (if cond then begin
                                           Specs__VM_instr_spec.inil () end
                                         else
                                         begin
                                           Specs__VM_instr_spec.ibranchf ofs end)
      | Imp__Imp.Bnot b1 ->
        Logic__Compiler_logic.prefix_dl (compile_bexpr b1 (not cond) ofs idr)
      | Imp__Imp.Beq (a1, a2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              Compiler__Compile_aexpr_reg.compile_aexpr a1
                                                idr))
                                            (Logic__Compiler_logic.prefix_dl (
                                               Compiler__Compile_aexpr_reg.compile_aexpr a2
                                                 (Z.add idr Z.one))))
          (Logic__Compiler_logic.prefix_dl (if cond then begin
                                              Specs__VM_instr_spec.ibeqrf idr
                                                (Z.add idr Z.one) ofs end
                                            else
                                            begin
                                              Specs__VM_instr_spec.ibnerf idr
                                                (Z.add idr Z.one) ofs end))
      | Imp__Imp.Ble (a1, a2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              Compiler__Compile_aexpr_reg.compile_aexpr a1
                                                idr))
                                            (Logic__Compiler_logic.prefix_dl (
                                               Compiler__Compile_aexpr_reg.compile_aexpr a2
                                                 (Z.add idr Z.one))))
          (Logic__Compiler_logic.prefix_dl (if cond then begin
                                              Specs__VM_instr_spec.iblerf idr
                                                (Z.add idr Z.one) ofs end
                                            else
                                            begin
                                              Specs__VM_instr_spec.ibgtrf idr
                                                (Z.add idr Z.one) ofs end))
      | Imp__Imp.Band (b1, b2) ->
        let c2 =
          Logic__Compiler_logic.infix_pc (Logic__Compiler_logic.prefix_dl (
                                            compile_bexpr b2 cond ofs idr)) in
        let ofs1 =
          if cond then begin Z.of_int (List.length c2) end
          else
          begin
            Z.add ofs (Z.of_int (List.length c2)) end in
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl (
                                            compile_bexpr b1 false ofs1 idr))
          c2
      end in Logic__Compiler_logic.hoare c

let compile_bexpr_natural (b: Imp__Imp.bexpr) (cond: bool) (ofs: Z.t)
                          (idr: Z.t) : Vm__Vm.instr list =
  let res = compile_bexpr b cond ofs idr in res

