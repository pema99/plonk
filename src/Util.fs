module Util

let flip (a, b) = b, a

let konst k _ = k

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b 

let seqTryHead s = Seq.tryPick Some s