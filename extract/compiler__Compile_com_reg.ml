let rec compile_com : 'a . Imp__Imp.com -> ('a Logic__Compiler_logic.hl) =
  fun cmd ->
    let res =
      begin match cmd with
      | Imp__Imp.Cskip ->
        Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.inil ())
      | Imp__Imp.Cassign (x, a) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl (
                                            Compiler__Compile_aexpr_reg.compile_aexpr a
                                              Z.zero))
          (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.istoref Z.zero
                                              x))
      | Imp__Imp.Cseq (cmd1, cmd2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl (
                                            compile_com cmd1))
          (Logic__Compiler_logic.prefix_dl (compile_com cmd2))
      | Imp__Imp.Cif (cond, cmd1, cmd2) ->
        let code_false = compile_com cmd2 in
        let code_true =
          Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl (
                                              compile_com cmd1))
            (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.ibranchf (Z.of_int (List.length code_false)))) in
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              Compiler__Compile_bexpr_reg.compile_bexpr cond
                                                false
                                                (Z.of_int (List.length code_true))
                                                Z.zero))
                                            (Logic__Compiler_logic.infix_pc code_true))
          (Logic__Compiler_logic.infix_pc (Logic__Compiler_logic.prefix_dl code_false))
      | Imp__Imp.Cwhile (test, body) ->
        let code_body = compile_com body in
        let body_length = Z.add (Z.of_int (List.length code_body)) Z.one in
        let code_test =
          Compiler__Compile_bexpr_reg.compile_bexpr test false body_length
            Z.zero in
        let ofs = Z.add (Z.of_int (List.length code_test)) body_length in
        let wp_while =
          Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl code_test)
            (Logic__Compiler_logic.infix_pc (Logic__Compiler_logic.infix_mnmn (
                                               Logic__Compiler_logic.prefix_dl code_body)
                                               (Logic__Compiler_logic.prefix_dl (
                                                  Specs__VM_instr_spec.ibranchf (Z.neg ofs))))) in
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl (
                                            Specs__VM_instr_spec.inil ()))
          (Logic__Compiler_logic.make_loop wp_while)
      end in Logic__Compiler_logic.hoare res

let compile_com_natural (com: Imp__Imp.com) : Vm__Vm.instr list =
  let res = compile_com com in res

let compile_program (prog: Imp__Imp.com) : Vm__Vm.instr list =
  let code = compile_com_natural prog in
  let code2 = List.append code Vm__Vm.ihalt in code2

