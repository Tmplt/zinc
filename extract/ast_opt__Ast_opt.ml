let rec opt_aexpr (e: Imp__Imp.aexpr) : Imp__Imp.aexpr =
  begin match e with
  | Imp__Imp.Anum n -> Imp__Imp.Anum n
  | Imp__Imp.Avar x -> Imp__Imp.Avar x
  | Imp__Imp.Aadd (e1, e2) ->
    let e1qt = opt_aexpr e1 in
    let e2qt = opt_aexpr e2 in
    begin match (e1qt, e2qt) with
    | (Imp__Imp.Anum n1, Imp__Imp.Anum n2) -> Imp__Imp.Anum (Z.add n1 n2)
    | (_, _) -> Imp__Imp.Aadd (e1qt, e2qt)
    end
  | Imp__Imp.Aaddu (e1, e2) ->
    let e1qt = opt_aexpr e1 in
    let e2qt = opt_aexpr e2 in
    begin match (e1qt, e2qt) with
    | (Imp__Imp.Anum n1, Imp__Imp.Anum n2) ->
      Imp__Imp.Anum (Bv_op__BV_OP.bv_add n1 n2)
    | (_, _) -> Imp__Imp.Aaddu (e1qt, e2qt)
    end
  | Imp__Imp.Asub (e1, e2) ->
    let e1qt = opt_aexpr e1 in
    let e2qt = opt_aexpr e2 in
    begin match (e1qt, e2qt) with
    | (Imp__Imp.Anum n1, Imp__Imp.Anum n2) -> Imp__Imp.Anum (Z.sub n1 n2)
    | (_, _) ->
      if e1qt = e2qt then begin Imp__Imp.Anum Z.zero end
      else
      begin
        Imp__Imp.Asub (e1qt, e2qt) end
    end
  | Imp__Imp.Asubu (e1, e2) ->
    let e1qt = opt_aexpr e1 in
    let e2qt = opt_aexpr e2 in
    begin match (e1qt, e2qt) with
    | (Imp__Imp.Anum n1, Imp__Imp.Anum n2) ->
      Imp__Imp.Anum (Bv_op__BV_OP.bv_sub n1 n2)
    | (_, _) ->
      if e1qt = e2qt then begin Imp__Imp.Anum Z.zero end
      else
      begin
        Imp__Imp.Asubu (e1qt, e2qt) end
    end
  end

let rec opt_bexpr (b: Imp__Imp.bexpr) : Imp__Imp.bexpr =
  begin match b with
  | Imp__Imp.Btrue -> Imp__Imp.Btrue
  | Imp__Imp.Bfalse -> Imp__Imp.Bfalse
  | Imp__Imp.Bnot b1 ->
    let bqt = opt_bexpr b1 in
    begin match bqt with
    | Imp__Imp.Btrue -> Imp__Imp.Bfalse
    | Imp__Imp.Bfalse -> Imp__Imp.Btrue
    | _ -> Imp__Imp.Bnot bqt
    end
  | Imp__Imp.Beq (a1, a2) ->
    let a1qt = opt_aexpr a1 in
    let a2qt = opt_aexpr a2 in Imp__Imp.Beq (a1qt, a2qt)
  | Imp__Imp.Ble (a1, a2) ->
    let a1qt = opt_aexpr a1 in
    let a2qt = opt_aexpr a2 in Imp__Imp.Ble (a1qt, a2qt)
  | Imp__Imp.Band (b1, b2) ->
    let b1qt = opt_bexpr b1 in
    let b2qt = opt_bexpr b2 in
    begin match (b1qt, b2qt) with
    | (Imp__Imp.Btrue, Imp__Imp.Btrue) -> Imp__Imp.Btrue
    | (Imp__Imp.Bfalse, _) -> Imp__Imp.Bfalse
    | (_, Imp__Imp.Bfalse) -> Imp__Imp.Bfalse
    | (_, _) ->
      if b1qt = b2qt then begin b1qt end
      else
      begin
        Imp__Imp.Band (b1qt, b2qt) end
    end
  end

let rec opt_com (c: Imp__Imp.com) : Imp__Imp.com =
  begin match c with
  | Imp__Imp.Cskip -> Imp__Imp.Cskip
  | Imp__Imp.Cassign (id, a) -> Imp__Imp.Cassign (id, (opt_aexpr a))
  | Imp__Imp.Cseq (c1, c2) ->
    let c1qt = opt_com c1 in
    let c2qt = opt_com c2 in Imp__Imp.Cseq (c1qt, c2qt)
  | Imp__Imp.Cif (b, c1, c2) ->
    let bqt = opt_bexpr b in
    let c1qt = opt_com c1 in
    let c2qt = opt_com c2 in
    begin match bqt with
    | Imp__Imp.Btrue -> Imp__Imp.Cseq (c1qt, Imp__Imp.Cskip)
    | Imp__Imp.Bfalse -> Imp__Imp.Cseq (c2qt, Imp__Imp.Cskip)
    | _ -> Imp__Imp.Cif (bqt, c1qt, c2qt)
    end
  | Imp__Imp.Cwhile (b, c1) ->
    let bqt = opt_bexpr b in
    let cqt1 = opt_com c1 in
    begin match bqt with
    | Imp__Imp.Bfalse -> Imp__Imp.Cskip
    | _ -> Imp__Imp.Cwhile (bqt, cqt1)
    end
  end

