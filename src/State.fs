module State

type StateM<'T, 'S> = 'S -> 'T * 'S

let get =
  fun s -> s, s
let inline set state =
  fun _ -> (), state

type StateBuilder() =
  member inline this.Return (v: 'T) : StateM<'T, 'S> =
    fun s -> v, s
  member inline this.ReturnFrom ([<InlineIfLambda>] m: StateM<'T, 'S>) : StateM<'T, 'S> =
    m
  member inline this.Zero () : StateM<unit, 'S> =
    this.Return ()
  member inline this.Bind ([<InlineIfLambda>] m: StateM<'T, 'S>, [<InlineIfLambda>] f: 'T -> StateM<'U, 'S>) : StateM<'U, 'S> =
    fun s ->
      let (a, n) = m s
      (f a) n
  member inline this.Combine ([<InlineIfLambda>] m1: StateM<'T, 'S>, [<InlineIfLambda>] m2: StateM<'U, 'S>) : StateM<'U, 'S> =
    fun s ->
      let (_, n) = m1 s
      m2 n
  member inline this.Delay ([<InlineIfLambda>] f: unit -> StateM<'T, 'S>): StateM<'T, 'S> =
    this.Bind (this.Return (), f)
  member inline this.For (coll: 'T seq, [<InlineIfLambda>] f: 'T -> StateM<'U, 'S>) =
    coll
      |> Seq.map f
      |> Seq.reduceBack (fun m1 m2 -> this.Combine (m1, m2))    
  member this.While (guard, m: StateM<'T, 'S>) = 
    if guard () then 
      this.Combine (m, this.While (guard, m))
    else this.Zero ()

let state = new StateBuilder()
