module JsonParser

open Combinator

// Combinator data source which reads a string one character at a time
type TextCombinatorState =
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
                          Column = if res = '\n' then 1 else this.Column + 1}
        do! com.set (updated :> CombinatorState<char>)
        return res
      else
        return! fail()
    }

// Type alias for text combinator
type TextCom<'T> = Com<'T, char>

// JSON AST
type JSONTree =
  | JSONBool of bool
  | JSONNumber of float
  | JSONString of string
  | JSONArray of JSONTree list
  | JSONObject of (string * JSONTree) list
  | JSONNull

// Parse 1 character
let charP = item

// Parse as much whitespace as possible
let whitespace =
  many (oneOf [' '; '\r'; '\n'; '\t']) *> just ()

// Parse any string
let anyStringP =
  whitespace *>
  within (one '"') (eatWhile ((<>) '"') |>> List.toArray |>> System.String)
  <* whitespace

// Parse a specific string
let stringP target =
  anyStringP >>= fun s ->
    if s = target then just s else fail()

// Parse a quoted number
let boolP = 
  (stringP "true" <|> stringP "false") |>> System.Boolean.Parse |>> JSONBool

// Parse a quoted number
let numberP =
  anyStringP >>= fun s -> 
    match System.Double.TryParse s with
    | true, n -> just (JSONNumber n)
    | _ -> fail()

// Parse a quoted null literal
let nullP =
  stringP "null" *> just JSONNull

// Forward declare recursive parser
let termP, termPImpl = declParser()

// Parse a JSON array
let arrayP =
  between
    (one '[') 
    (sepBy (termP) (one ','))
    (one ']')
    |>> JSONArray

// Parse a JSON object
let objectP =
  between
    (one '{') 
    (sepBy ((anyStringP <* one ':') <+> termP) (one ','))
    (one '}')
    |>> JSONObject

// Supply implementation for recursive parser
// Parse a JSON syntax element
termPImpl :=
  whitespace *>
  attempt boolP
  <|> attempt numberP
  <|> attempt nullP
  <|> (anyStringP |>> JSONString)
  <|> objectP
  <|> arrayP
  <* whitespace

// Load in some JSON
let jsonDataSource = {
    Source = System.IO.File.ReadAllText("test.json")
    Line = 1
    Column = 0
}

// Parse it and show the result
let result, state = objectP jsonDataSource 
match result with
| Success v -> printf "%A" v
| _ -> printf "Parsing failure. State: %A" state