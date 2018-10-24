exception Err

exception Halt of Vm__Vm.machine_state

let rec split_at (p: Z.t) (c: Vm__Vm.instr list) :
  (Vm__Vm.instr list) * (Vm__Vm.instr list) =
  begin match (c, Z.equal p Z.zero) with
  | ([], _) -> raise Err
  | (_, true) -> ([], c)
  | (e :: cqt, _) ->
    let (hd, tl) = split_at (Z.sub p Z.one) cqt in (e :: hd, tl)
  end

let pop (s: (Z.t) list) : (Z.t) * ((Z.t) list) =
  begin match s with
  | [] -> raise Err
  | rv :: rs -> (rv, rs)
  end

let instr_ex (c: Vm__Vm.instr list) (ms: Vm__Vm.machine_state) : Vm__Vm.machine_state
  =
  let Vm__Vm.VMS (p, r, s, m) = ms in
  begin
    if Z.lt p Z.zero then begin raise Err end;
    begin try
      begin match split_at p c with
      | (_, instr :: _) ->
        begin match instr with
        | Vm__Vm.Iload (idr, id) ->
          Vm__Vm.VMS ((Z.add p Z.one),
            (State__Reg.write r idr (State__State.mixfix_lbrb m id)), s, m)
        | Vm__Vm.Iimm (idr, n) ->
          Vm__Vm.VMS ((Z.add p Z.one), (State__Reg.write r idr n), s, m)
        | Vm__Vm.Istore (idr, id) ->
          let n = State__Reg.read r idr in
          Vm__Vm.VMS ((Z.add p Z.one), r, s,
            (State__State.mixfix_lblsmnrb m id n))
        | Vm__Vm.Ipushr idr ->
          Vm__Vm.VMS ((Z.add p Z.one), r,
            (Vm__Vm.push (State__Reg.read r idr) s), m)
        | Vm__Vm.Ipopr idr ->
          let (n, sqt) = pop s in
          Vm__Vm.VMS ((Z.add p Z.one), (State__Reg.write r idr n), sqt, m)
        | Vm__Vm.Iaddr (lhs, rhs, res) ->
          let lhs1 = State__Reg.read r lhs in
          let rhs1 = State__Reg.read r rhs in
          Vm__Vm.VMS ((Z.add p Z.one),
            (State__Reg.write r res (Z.add lhs1 rhs1)), s, m)
        | Vm__Vm.Iaddur (lhs, rhs, res) ->
          let lhs1 = State__Reg.read r lhs in
          let rhs1 = State__Reg.read r rhs in
          Vm__Vm.VMS ((Z.add p Z.one),
            (State__Reg.write r res (Bv_op__BV_OP.bv_add lhs1 rhs1)), s, m)
        | Vm__Vm.Isubr (lhs, rhs, res) ->
          let lhs1 = State__Reg.read r lhs in
          let rhs1 = State__Reg.read r rhs in
          Vm__Vm.VMS ((Z.add p Z.one),
            (State__Reg.write r res (Z.sub lhs1 rhs1)), s, m)
        | Vm__Vm.Ibeqr (r1, r2, ofs) ->
          let r11 = State__Reg.read r r1 in
          let r21 = State__Reg.read r r2 in
          Vm__Vm.VMS ((Z.add (Z.add p Z.one) (if Z.equal r11 r21 then begin
                                                ofs end
                                              else
                                              begin
                                                Z.zero end)), r, s, m)
        | Vm__Vm.Ibner (r1, r2, ofs) ->
          let r11 = State__Reg.read r r1 in
          let r21 = State__Reg.read r r2 in
          Vm__Vm.VMS ((Z.add (Z.add p Z.one) (if not (Z.equal r11 r21) then
                                                begin ofs end
                                              else
                                              begin
                                                Z.zero end)), r, s, m)
        | Vm__Vm.Ibler (r1, r2, ofs) ->
          let r11 = State__Reg.read r r1 in
          let r21 = State__Reg.read r r2 in
          Vm__Vm.VMS ((Z.add (Z.add p Z.one) (if Z.leq r11 r21 then begin
                                                ofs end
                                              else
                                              begin
                                                Z.zero end)), r, s, m)
        | Vm__Vm.Ibgtr (r1, r2, ofs) ->
          let r11 = State__Reg.read r r1 in
          let r21 = State__Reg.read r r2 in
          Vm__Vm.VMS ((Z.add (Z.add p Z.one) (if Z.gt r11 r21 then begin
                                                ofs end
                                              else
                                              begin
                                                Z.zero end)), r, s, m)
        | Vm__Vm.Iconst n1 ->
          Vm__Vm.VMS ((Z.add p Z.one), r, (Vm__Vm.push n1 s), m)
        | Vm__Vm.Ivar id ->
          Vm__Vm.VMS ((Z.add p Z.one), r,
            (Vm__Vm.push (State__State.mixfix_lbrb m id) s), m)
        | Vm__Vm.Isetvar id ->
          let (n1, sqt1) = pop s in
          Vm__Vm.VMS ((Z.add p Z.one), r, sqt1,
            (State__State.mixfix_lblsmnrb m id n1))
        | Vm__Vm.Ibranch ofs ->
          Vm__Vm.VMS ((Z.add (Z.add p Z.one) ofs), r, s, m)
        | Vm__Vm.Iadd ->
          let (n11, sqt2) = pop s in let (n2, sqtqt) = pop sqt2 in
          Vm__Vm.VMS ((Z.add p Z.one), r, (Vm__Vm.push (Z.add n11 n2) sqtqt),
            m)
        | Vm__Vm.Iaddu ->
          let (n12, sqt3) = pop s in let (n21, sqtqt1) = pop sqt3 in
          Vm__Vm.VMS ((Z.add p Z.one), r,
            (Vm__Vm.push (Bv_op__BV_OP.bv_add n21 n12) sqtqt1), m)
        | Vm__Vm.Isub ->
          let (n13, sqt4) = pop s in let (n22, sqtqt2) = pop sqt4 in
          Vm__Vm.VMS ((Z.add p Z.one), r,
            (Vm__Vm.push (Z.sub n22 n13) sqtqt2), m)
        | Vm__Vm.Ibeq ofs ->
          let (n23, sqt5) = pop s in let (n14, sqtqt3) = pop sqt5 in
          Vm__Vm.VMS ((Z.add (Z.add p Z.one) (if Z.equal n14 n23 then begin
                                                ofs end
                                              else
                                              begin
                                                Z.zero end)), r, sqtqt3, m)
        | Vm__Vm.Ibne ofs ->
          let (n24, sqt6) = pop s in let (n15, sqtqt4) = pop sqt6 in
          Vm__Vm.VMS ((Z.add (Z.add p Z.one) (if not (Z.equal n15 n24) then
                                                begin ofs end
                                              else
                                              begin
                                                Z.zero end)), r, sqtqt4, m)
        | Vm__Vm.Ible ofs ->
          let (n25, sqt7) = pop s in let (n16, sqtqt5) = pop sqt7 in
          Vm__Vm.VMS ((Z.add (Z.add p Z.one) (if Z.leq n16 n25 then begin
                                                ofs end
                                              else
                                              begin
                                                Z.zero end)), r, sqtqt5, m)
        | Vm__Vm.Ibgt ofs ->
          let (n26, sqt8) = pop s in let (n17, sqtqt6) = pop sqt8 in
          Vm__Vm.VMS ((Z.add (Z.add p Z.one) (if Z.gt n17 n26 then begin
                                                ofs end
                                              else
                                              begin
                                                Z.zero end)), r, sqtqt6, m)
        | Vm__Vm.Ihalt -> raise (Halt ms)
        | _ -> raise Err
        end
      | _ -> assert false (* absurd *)
      end with
    | Err -> raise Err
    end
  end

let rec instr_iter_ex (c: Vm__Vm.instr list) (ms: Vm__Vm.machine_state) :
  Vm__Vm.machine_state = let o = instr_ex c ms in instr_iter_ex c o

