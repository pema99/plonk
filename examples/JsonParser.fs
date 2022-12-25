module JsonParser

open Combinator
open Common

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

// Load in some JSON, using the builtin text parser data source located in src/Common.fs
let jsonDataSource = mkMultiLineParser (System.IO.File.ReadAllText("test.json"))

// Parse it and show the result
let result, state = objectP jsonDataSource 
match result with
| Success v -> printfn "%A" v
| _ -> printfn "Parsing failure. State: %A" state