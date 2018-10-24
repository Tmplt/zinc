let bv_add (i1: Z.t) (i2: Z.t) : Z.t =
  let v1 = Int32.of_int(Z.to_int i1) in
  let v2 = Int32.of_int(Z.to_int i2) in
  let v = Int32.add v1 v2 in Z.of_int (Int32.to_int v)

let bv_sub (i1: Z.t) (i2: Z.t) : Z.t =
  let v1 = Int32.of_int(Z.to_int i1) in
  let v2 = Int32.of_int(Z.to_int i2) in
  let v = Int32.sub v1 v2 in Z.of_int (Int32.to_int v)

