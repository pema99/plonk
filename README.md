# Plonk.
This is Plonk, son of Gunk. Plonk is a small F# parser combinator library which is agnostic of the type of input data. Anything that you can implement 2 basic iterator operations for, you can parse with this library. To see how this works in practice, check out the examples directory, which contains an implementation of a JSON parser. The syntax should be familiar to those who have used Parsec, but some of the operators are just made up. Operators are subject to change, and more will probably be added.

# Show me some code.
Here is a little snippet showing a simple combinator which parses a JSON array. This code is ripped straight out of the example project in the examples folder.
```fsharp
let arrayP =
  between
    (one '[') 
    (sepBy termP (one ','))
    (one ']')
    |>> JSONArray
```
Here is another example, this time parsing JSON boolean literals.
```fsharp
let boolP = 
  (stringP "true" <|> stringP "false")
    |>> System.Boolean.Parse
    |>> JSONBool
```

# Should you use this?
Probably not. If you are looking for a production ready parser combinator framework for F#, check out FParsec, which is based on the famous Parsec library from Haskell.

# How to build.
First install the dotnet core at https://dotnet.microsoft.com/download.

To build standalone DLL, using the dotnet core CLI:
```sh
git clone https://github.com/pema99/plonk
cd plonk
dotnet build
```
To build and run the example JSON parser program, also using dotnet core CLI:
```sh
git clone https://github.com/pema99/plonk
cd plonk/examples
dotnet run
```
