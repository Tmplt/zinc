let ult (x: int32) (y1: int32) : bool =
  Z.lt (SHOULD_NOT_BE_HERE) (SHOULD_NOT_BE_HERE)

let ule (x: int32) (y1: int32) : bool =
  Z.leq (SHOULD_NOT_BE_HERE) (SHOULD_NOT_BE_HERE)

let ugt (x: int32) (y1: int32) : bool =
  Z.gt (SHOULD_NOT_BE_HERE) (SHOULD_NOT_BE_HERE)

let uge (x: int32) (y1: int32) : bool =
  Z.geq (SHOULD_NOT_BE_HERE) (SHOULD_NOT_BE_HERE)

let slt (v1: int32) (v2: int32) : bool =
  Z.lt (Z.of_int (Int32.to_int v1)) (Z.of_int (Int32.to_int v2))

let sle (v1: int32) (v2: int32) : bool =
  Z.leq (Z.of_int (Int32.to_int v1)) (Z.of_int (Int32.to_int v2))

let sgt (v1: int32) (v2: int32) : bool =
  Z.gt (Z.of_int (Int32.to_int v1)) (Z.of_int (Int32.to_int v2))

let sge (v1: int32) (v2: int32) : bool =
  Z.geq (Z.of_int (Int32.to_int v1)) (Z.of_int (Int32.to_int v2))

