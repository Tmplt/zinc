type id =
  | Id of (Z.t)

type state = id -> (Z.t)

let get (f: id -> (Z.t)) (x: id) : Z.t = f x

let set (f: id -> (Z.t)) (x: id) (v: Z.t) : id -> (Z.t) =
  fun (y: id) -> let (Id xv, Id yv) = (x, y) in
    if Z.equal xv yv then begin v end else begin f y end

let mixfix_lbrb (f: 'xi -> 'xi1) (x: 'xi) : 'xi1 = f x

let mixfix_lblsmnrb (f: id -> (Z.t)) (x: id) (v: Z.t) : id -> (Z.t) =
  set f x v

let const (v: Z.t) : id -> (Z.t) = fun (_: id) -> v

