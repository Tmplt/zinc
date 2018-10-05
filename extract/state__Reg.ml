type idr = Z.t

type regs = (Z.t) -> (Z.t)

let read (f: (Z.t) -> (Z.t)) (x: Z.t) : Z.t = f x

let write (f: (Z.t) -> (Z.t)) (x: Z.t) (v: Z.t) : (Z.t) -> (Z.t) =
  fun (y1: Z.t) -> if Z.equal x y1 then begin v end else begin f y1 end

let const (v: Z.t) : (Z.t) -> (Z.t) = fun (_1: Z.t) -> v

