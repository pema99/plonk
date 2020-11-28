module Util

let isAlpha c =
  c >= 'a' && c <= 'z'

let isNumeric c =
  c >= '0' && c <= '9'

let isAlphanumeric c =
  isAlpha c || isNumeric c

let isWhitespace c =
  match c with 
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let (|Alpha|_|) c =
  if isAlpha c then Some c
  else None

let (|Numeric|_|) c =
  if isNumeric c then Some c
  else None

let (|Alphanumeric|_|) c =
  if isAlphanumeric c then Some c
  else None

let (|Whitespace|_|) c =
  if isWhitespace c then Some c
  else None

let flip (a, b) = b, a

let konst k _ = k

let curry f a b = f (a, b)
let uncurry f (a, b) = f a b 