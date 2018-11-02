let rec aeval_count (e: Imp__Imp.aexpr) : Z.t =
  begin match e with
  | Imp__Imp.Anum _ -> Z.one
  | Imp__Imp.Avar _ -> Z.one
  | Imp__Imp.Aadd (e1, e2) ->
    let cntl = aeval_count e1 in
    let cntr = aeval_count e2 in Z.add (Z.add cntl cntr) Z.one
  | Imp__Imp.Aaddu (e1, e2) ->
    let cntl = aeval_count e1 in
    let cntr = aeval_count e2 in Z.add (Z.add cntl cntr) Z.one
  | Imp__Imp.Asub (e1, e2) ->
    let cntl = aeval_count e1 in
    let cntr = aeval_count e2 in Z.add (Z.add cntl cntr) Z.one
  | Imp__Imp.Asubu (e1, e2) ->
    let cntl = aeval_count e1 in
    let cntr = aeval_count e2 in Z.add (Z.add cntl cntr) Z.one
  end

let rec beval_count (b: Imp__Imp.bexpr) : Z.t =
  begin match b with
  | Imp__Imp.Btrue -> Z.one
  | Imp__Imp.Bfalse -> Z.one
  | Imp__Imp.Bnot bqt -> let cnt = beval_count bqt in Z.add cnt Z.one
  | Imp__Imp.Band (b1, b2) ->
    let cntl = beval_count b1 in
    let cntr = beval_count b2 in Z.add (Z.add cntl cntr) Z.one
  | Imp__Imp.Beq (a1, a2) ->
    let cntl = aeval_count a1 in
    let cntr = aeval_count a2 in Z.add (Z.add cntl cntr) Z.one
  | Imp__Imp.Ble (a1, a2) ->
    let cntl = aeval_count a1 in
    let cntr = aeval_count a2 in Z.add (Z.add cntl cntr) Z.one
  end

let max (a: Z.t) (b: Z.t) : Z.t =
  if Z.gt a b then begin a end else begin b end

let rec ceval_count (c: Imp__Imp.com) : Z.t =
  begin match c with
  | Imp__Imp.Cskip -> Z.one
  | Imp__Imp.Cassign (_, aexpr) ->
    let cnt = aeval_count aexpr in Z.add cnt Z.one
  | Imp__Imp.Cseq (c1, c2) ->
    let cnt1 = ceval_count c1 in let cnt2 = ceval_count c2 in Z.add cnt1 cnt2
  | Imp__Imp.Cif (bexpr, c1, c2) ->
    let cnt = beval_count bexpr in
    let cntqt = ceval_count c1 in
    let cntqtqt = ceval_count c2 in max (Z.add cnt cntqt) (Z.add cnt cntqtqt)
  | Imp__Imp.Cwhile (bexpr, com) ->
    let cnt = beval_count bexpr in
    let cntqt = ceval_count com in Z.add cnt cntqt
  end

