let rec aeval_ex (st: State__State.id -> (Z.t)) (e: Imp__Imp.aexpr) :
  (Z.t) * (Z.t) =
  begin match e with
  | Imp__Imp.Anum n -> (n, Z.one)
  | Imp__Imp.Avar x -> (State__State.mixfix_lbrb st x, Z.one)
  | Imp__Imp.Aadd (e1, e2) ->
    let (lhs, cntl) = aeval_ex st e1 in let (rhs, cntr) = aeval_ex st e2 in
    (Z.add lhs rhs, Z.add (Z.add cntl cntr) Z.one)
  | Imp__Imp.Aaddu (e1, e2) ->
    let (lhs1, cntl1) = aeval_ex st e1 in let (rhs1, cntr1) =
    aeval_ex st e2 in
    (Bv_op__BV_OP.bv_add lhs1 rhs1, Z.add (Z.add cntl1 cntr1) Z.one)
  | Imp__Imp.Asub (e1, e2) ->
    let (lhs2, cntl2) = aeval_ex st e1 in let (rhs2, cntr2) =
    aeval_ex st e2 in (Z.sub lhs2 rhs2, Z.add (Z.add cntl2 cntr2) Z.one)
  | Imp__Imp.Asubu (e1, e2) ->
    let (lhs3, cntl3) = aeval_ex st e1 in let (rhs3, cntr3) =
    aeval_ex st e2 in
    (Bv_op__BV_OP.bv_sub lhs3 rhs3, Z.add (Z.add cntl3 cntr3) Z.one)
  end

let rec beval_ex (st: State__State.id -> (Z.t)) (b: Imp__Imp.bexpr) :
  (bool) * (Z.t) =
  begin match b with
  | Imp__Imp.Btrue -> (true, Z.one)
  | Imp__Imp.Bfalse -> (false, Z.one)
  | Imp__Imp.Bnot bqt ->
    let (bqtqt, cnt) = beval_ex st bqt in (not bqtqt, Z.add cnt Z.one)
  | Imp__Imp.Band (b1, b2) ->
    let (lhs4, cntl4) = beval_ex st b1 in let (rhs4, cntr4) =
    beval_ex st b2 in (lhs4 && rhs4, Z.add (Z.add cntl4 cntr4) Z.one)
  | Imp__Imp.Beq (a1, a2) ->
    let (lhs5, cntl5) = aeval_ex st a1 in let (rhs5, cntr5) =
    aeval_ex st a2 in (Z.equal lhs5 rhs5, Z.add (Z.add cntl5 cntr5) Z.one)
  | Imp__Imp.Ble (a1, a2) ->
    let (lhs6, cntl6) = aeval_ex st a1 in let (rhs6, cntr6) =
    aeval_ex st a2 in (Z.leq lhs6 rhs6, Z.add (Z.add cntl6 cntr6) Z.one)
  end

exception MaxStepsReached of (State__State.id -> (Z.t))

let rec ceval_ex (st: State__State.id -> (Z.t)) (c: Imp__Imp.com)
                 (maxCnt: Z.t) : (State__State.id -> (Z.t)) * (Z.t) =
  let (cqt, cnt1) =
  begin match c with
  | Imp__Imp.Cskip -> (st, Z.one)
  | Imp__Imp.Cassign (id, aexpr) ->
    let (res, cnt2) = aeval_ex st aexpr in
    (State__State.mixfix_lblsmnrb st id res, Z.add cnt2 Z.one)
  | Imp__Imp.Cseq (c1, c2) ->
    let (st1, cnt11) = ceval_ex st c1 maxCnt in let (st2, cnt21) =
    ceval_ex st1 c2 maxCnt in (st2, Z.add cnt11 cnt21)
  | Imp__Imp.Cif (bexpr, c1, c2) ->
    let (cond, cnt3) = beval_ex st bexpr in
    begin match cond with
    | true ->
      let (stqt, cntqt) = ceval_ex st c1 maxCnt in (stqt, Z.add cnt3 cntqt)
    | false ->
      let (stqt1, cntqt1) = ceval_ex st c2 maxCnt in
      (stqt1, Z.add cnt3 cntqt1)
    end
  | Imp__Imp.Cwhile (bexpr, com) ->
    let (cond1, cnt4) = beval_ex st bexpr in
    begin match cond1 with
    | true ->
      let (stqt2, cntqt2) = ceval_ex st com maxCnt in let (stqtqt, cntqtqt) =
      ceval_ex stqt2 c maxCnt in (stqtqt, Z.add (Z.add cnt4 cntqt2) cntqtqt)
    | false -> (st, cnt4)
    end
  end in
  if (Z.equal maxCnt Z.zero) || (Z.leq cnt1 maxCnt) then begin
    (cqt, cnt1) end
  else
  begin
    raise (MaxStepsReached cqt) end

