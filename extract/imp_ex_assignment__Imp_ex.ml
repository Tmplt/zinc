let rec aeval_ex (st: State__State.id -> (Z.t)) (e: Imp__Imp.aexpr) : Z.t =
  begin match e with
  | Imp__Imp.Anum n -> n
  | Imp__Imp.Avar x -> State__State.mixfix_lbrb st x
  | Imp__Imp.Aadd (e1, e2) -> Z.add (aeval_ex st e1) (aeval_ex st e2)
  | Imp__Imp.Aaddu (e1, e2) ->
    Bv_op__BV_OP.bv_add (aeval_ex st e1) (aeval_ex st e2)
  | Imp__Imp.Asub (e1, e2) -> Z.sub (aeval_ex st e1) (aeval_ex st e2)
  end

let rec beval_ex (st: State__State.id -> (Z.t)) (b: Imp__Imp.bexpr) : bool =
  begin match b with
  | Imp__Imp.Btrue -> true
  | Imp__Imp.Bfalse -> false
  | Imp__Imp.Bnot bqt -> not (beval_ex st bqt)
  | Imp__Imp.Band (b1, b2) -> (beval_ex st b1) && (beval_ex st b2)
  | Imp__Imp.Beq (a1, a2) -> Z.equal (aeval_ex st a1) (aeval_ex st a2)
  | Imp__Imp.Ble (a1, a2) -> Z.leq (aeval_ex st a1) (aeval_ex st a2)
  end

let rec ceval_ex (st: State__State.id -> (Z.t)) (c: Imp__Imp.com) :
  State__State.id -> (Z.t) =
  begin match c with
  | Imp__Imp.Cskip -> st
  | Imp__Imp.Cassign (id, aexpr) ->
    State__State.mixfix_lblsmnrb st id (aeval_ex st aexpr)
  | Imp__Imp.Cseq (c1, c2) -> let o = ceval_ex st c1 in ceval_ex o c2
  | Imp__Imp.Cif (bexpr, c1, c2) ->
    begin match beval_ex st bexpr with
    | true -> ceval_ex st c1
    | false -> ceval_ex st c2
    end
  | Imp__Imp.Cwhile (bexpr, com) ->
    begin match beval_ex st bexpr with
    | true -> let o = ceval_ex st com in ceval_ex o c
    | false -> st
    end
  end

