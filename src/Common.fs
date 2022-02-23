module Common
// This module contains some common combinator data sources

open Combinator
open Util

// Parser state for multiline text
type MultiLineTextCombinatorState =
  { Source: string
    Line: int
    Column: int }
  interface CombinatorState<char> with
    member this.Peek = com {
      if this.Source.Length > 0 then
        return this.Source.[0]
      else
        return! fail()
    }
    member this.Item = com {
      if this.Source.Length > 0 then
        let res = this.Source.[0]
        let updated = { this with
                          Source = this.Source.[1..]
                          Line = if res = '\n' then this.Line + 1 else this.Line
                          Column = if res = '\n' then 1 else this.Column + 1 }
        do! com.set (updated :> CombinatorState<char>)
        return res
      else
        return! fail()
    }

let mkMultiLineParser source = {
  Source = source
  Line = 1
  Column = 0
}

// Parser state for arbitrary (peekable) sequences
type SequenceCombinatorState<'t> =
  { Sequence: 't seq
    Peeked: 't option }
  interface CombinatorState<'t> with
    member this.Item = com {
      match this.Peeked with
      | Some v ->
        let updated = { this with
                          Peeked = None }
        do! com.set (updated :> CombinatorState<'t>)
        return v
      | None ->
        match seqTryHead this.Sequence with
        | Some res ->
          let updated = { this with
                            Sequence = Seq.tail this.Sequence }
          do! com.set (updated :> CombinatorState<'t>)
          return res
        | None -> return! fail()
    }
    member this.Peek = com {
      match this.Peeked with
      | Some v -> return v
      | None -> 
        match seqTryHead this.Sequence with
        | Some res ->
          let updated = { this with
                            Sequence = Seq.tail this.Sequence
                            Peeked = Some res }
          do! com.set (updated :> CombinatorState<'t>)
          return res
        | None -> return! fail()
    }

let mkSeqParser seq = {
  Sequence = seq
  Peeked = None
}

// Parser for arrays of data
type ArrayCombinatorState<'t> =
  { Toks: 't []
    Offset: int }
  interface CombinatorState<'t> with
    member this.Peek = com {
      if this.Offset < this.Toks.Length then
        return this.Toks.[this.Offset]
      else
        return! fail()
    }
    member this.Item = com {
      if this.Offset < this.Toks.Length then
        let res = this.Toks.[this.Offset]
        let updated = { this with
                          Offset = this.Offset + 1 }
        do! com.set (updated :> CombinatorState<'t>)
        return res
      else
        return! fail()
    }

let mkArrayParser toks = {
  Toks = toks
  Offset = 0
}