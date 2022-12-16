module Combinator
#nowarn "25"
#nowarn "40"

open State

type CombinatorError = string * (int * int)

type CombinatorResult<'T> =
  | Success of 'T
  | Failure
  | FailureWith of CombinatorError
  | CompoundFailure of CombinatorError list 

let joinResult joiner a b =
  match a, b with
  | Success l, Success r                 -> Success (joiner l r)
  | Success _, Failure
  | Success _, FailureWith _
  | Success _, CompoundFailure _         -> b 
  | Failure, Success _         
  | Failure, Failure                     -> Failure
  | Failure, FailureWith _
  | Failure, CompoundFailure _           -> b
  | FailureWith _, Success _
  | FailureWith _, Failure               -> a
  | FailureWith l, FailureWith r         -> CompoundFailure [l; r] 
  | FailureWith l, CompoundFailure r     -> CompoundFailure (l :: r)
  | CompoundFailure _, Success _
  | CompoundFailure _, Failure           -> a
  | CompoundFailure l, FailureWith r     -> CompoundFailure (r :: l)
  | CompoundFailure l, CompoundFailure r -> CompoundFailure (l @ r)

let inline copyFailure a =
  match a with
  | Success _         -> Failure
  | Failure           -> Failure
  | FailureWith e     -> FailureWith e
  | CompoundFailure e -> CompoundFailure e

type CombinatorState<'T> =
  abstract member Peek : Com<'T, 'T>
  abstract member Item : Com<'T, 'T>

and Com<'T, 'S> = StateM<CombinatorResult<'T>, CombinatorState<'S>>

type CombinatorBuilder() =
  member inline this.Return (v: 'T) : Com<'T, 'S> =
    fun s -> Success v, s
  member inline this.ReturnFrom ([<InlineIfLambda>] m: Com<'T, 'S>) : Com<'T, 'S> =
    m
  member inline this.Zero () : Com<unit, 'S> =
    this.Return ()
  member inline this.Bind ([<InlineIfLambda>] m: Com<'T, 'S>, [<InlineIfLambda>] f: 'T -> Com<'U, 'S>) : Com<'U, 'S> =
    fun s ->
      let a, n = m s
      match a with
      | Success v -> (f v) n
      | err -> copyFailure err, n
  member inline this.Combine ([<InlineIfLambda>] m1: Com<'T, 'S>, [<InlineIfLambda>] m2: Com<'U, 'S>) : Com<'U, 'S> =
    fun s ->
      let a, n = m1 s
      match a with
      | Success _ -> m2 n
      | err -> copyFailure err, n
  member inline this.Delay ([<InlineIfLambda>] f: unit -> Com<'T, 'S>): Com<'T, 'S> =
    this.Bind (this.Return (), f)
  member inline this.get() =
    fun s -> Success s, s
  member inline this.set v =
    fun _ -> Success (), v

let com = CombinatorBuilder()

let look : Com<'T, 'T> =
  fun s -> s.Peek s

let item : Com<'T, 'T> =
  fun s -> s.Item s

let inline ( <|> ) ([<InlineIfLambda>] m1: Com<'T, 'S>) ([<InlineIfLambda>] m2: Com<'T, 'S>) : Com<'T, 'S> = state {  
  match! m1 with
  | Success v -> return Success v
  | _ -> return! m2
}

let inline ( <*> ) ([<InlineIfLambda>] f: Com<'T -> 'U, 'S>) ([<InlineIfLambda>] m: Com<'T, 'S>) : Com<'U, 'S> = com {
  let! a = f
  let! b = m
  return a b
}

let inline ( <!> ) ([<InlineIfLambda>] f: 'T -> 'U) ([<InlineIfLambda>] m: Com<'T, 'S>) : Com<'U, 'S> = com {
  let! v = m
  return f v
}

let inline ( |>> ) ([<InlineIfLambda>] m: Com<'T, 'S>) ([<InlineIfLambda>] f: 'T -> 'U) : Com<'U, 'S> =
  f <!> m

let inline ( <* ) ([<InlineIfLambda>] m1: Com<'T, 'S>) ([<InlineIfLambda>] m2: Com<'U, 'S>) : Com<'T, 'S> = com {
  let! a = m1
  let! b = m2
  return a
}
 
let inline ( *> ) ([<InlineIfLambda>] m1: Com<'T, 'S>) ([<InlineIfLambda>] m2: Com<'U, 'S>) : Com<'U, 'S> = com {
  let! a = m1
  let! b = m2
  return b
}

let inline ( <+> ) ([<InlineIfLambda>] m1: Com<'T, 'S>) ([<InlineIfLambda>] m2: Com<'U, 'S>) : Com<'T * 'U, 'S> = com {
  let! a = m1
  let! b = m2
  return a, b
}

let inline ( >>= ) ([<InlineIfLambda>] m: Com<'T, 'S>) ([<InlineIfLambda>] f: 'T -> Com<'U, 'S>) : Com<'U, 'S> =
  com.Bind (m, f)

let inline just (a: 'T) : Com<'T, 'S> =
  com.Return a

let inline joinl ([<InlineIfLambda>] m1: Com<'T, 'S>) ([<InlineIfLambda>] m2: Com<'T, 'S>) : Com<'T, 'S> = state {
    let! a = m1
    let! b = m2
    return joinResult (fun a _ -> a) a b
}

let inline joinr ([<InlineIfLambda>] m1: Com<'T, 'S>) ([<InlineIfLambda>] m2: Com<'T, 'S>) : Com<'T, 'S> = state {
    let! a = m1
    let! b = m2
    return joinResult (fun _ b -> b) a b
}

let fail () : Com<'T, 'S> =
  fun s -> Failure, s

let failWith msg : Com<'T, 'S> =
  fun s -> FailureWith msg, s

let inline many ([<InlineIfLambda>] v: Com<'T, 'S>) : Com<'T list, 'S> = com {
  let rec loop acc = state {
    match! v with
    | Success v -> return! loop (v :: acc)
    | _ -> return Success (List.rev acc)
  }
  let! res = loop []
  return res
}

let inline many1 ([<InlineIfLambda>] v: Com<'T, 'S>) : Com<'T list, 'S> = com {
  let! res = many v
  if List.isEmpty res then return! fail()
  else return res
}

let opt (p: Com<'T, 'S>) : Com<'T option, 'S> =
  (p |>> Some) <|> just None

let maybe (p: Com<'T, 'S>) : Com<bool, 'S> =
  (p *> just true) <|> just false

let between (l: Com<'T, 'S>) (v: Com<'U, 'S>) (r: Com<'V, 'S>) : Com<'U, 'S> =
  l *> v <* r

let within (w: Com<'T, 'S>) (v: Com<'U, 'S>) : Com<'U, 'S> =
  between w v w

let sepBy2 (p: Com<'T, 'S>) (sep: Com<'U, 'S>) : Com<'T list, 'S> =
  p <+> (sep *> p) <+> many (sep *> p)
  |>> fun ((a, b), c) -> a :: b :: c

let sepBy1 (p: Com<'T, 'S>) (sep: Com<'U, 'S>) : Com<'T list, 'S> =
  p <+> many (sep *> p)
  |>> List.Cons

let sepBy (p: Com<'T, 'S>) (sep: Com<'U, 'S>) : Com<'T list, 'S> =
  sepBy1 p sep <|> just []

let satisfy (pred: 'T -> bool) : Com<'T, 'T> = com {
  let! next = look
  if pred next then
    let! _ = item
    return next
  else return! fail()
}

let check (pred: 'T -> bool) : Com<bool, 'T> = state {
  match! look with
  | Success v ->
    if pred v then
      return Success true
    else return Success false
  | Failure -> return Success false
}

let one (tar: 'T) : Com<'T, 'T> =
  satisfy ((=) tar)

let oneOf (lst: 'T list) : Com<'T, 'T> =
  satisfy (fun x -> List.contains x lst)

let eatWhile1 pred : Com<'T list, 'T> =
  many1 (satisfy pred)

let eatWhile pred : Com<'T list, 'T> =
  many (satisfy pred)

let choice (lst: Com<'T, 'S> list) : Com<'T, 'S> =
  List.fold (<|>) (fail()) lst 

let attempt (p: Com<'T, 'S>) : Com<'T, 'S> = 
  fun s ->
    let nt, ns = p s
    match nt with
    | Success _ -> nt, ns
    | _ -> nt, s

let guard (pred: 'T -> bool) (p: Com<'T, 'S>) : Com<'T, 'S> =
  p >>= fun s -> if pred s then just s else fail()

let lookAhead (p: Com<'T, 'S>) : Com<'T, 'S> = 
  fun s ->
    let nt, _ = p s
    match nt with
    | Success _ -> nt, s
    | _ -> nt, s

let chainL1 (p: Com<'T, 'S>) (op: Com<'T -> 'T -> 'T, 'S>) : Com<'T, 'S> = com {
  let! first = p 
  let rec loop prev = state {
    match! op <+> p with
    | Success (f, curr) -> return! loop (f prev curr)
    | Failure -> return Success prev
  }
  return! loop first
}

let chainR1 (p: Com<'T, 'S>) (op: Com<'T -> 'T -> 'T, 'S>) : Com<'T, 'S> = com {
  let rec loop prev = state {
    match! op <+> scan with
    | Success (f, curr) -> return Success (f prev curr)
    | Failure -> return Success prev
    }
  and scan = com {
    let! first = p 
    return! loop first
    }
  return! scan
}

let eof : Com<unit, 'S> =
  fun s ->
    let nt, _ = look s
    match nt with
    | Success _ -> Failure, s
    | _ -> Success (), s

let delete (p: Com<'T, 'S>) : Com<unit, 'S> =
  p *> just ()

let declParser() : Com<'T, 'S> * Com<'T, 'S> ref =
  let decl = ref (fail())
  (fun s -> (!decl) s), decl

let implParser decl impl =
  decl := impl