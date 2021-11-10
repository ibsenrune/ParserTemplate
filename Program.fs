open System
open FParsec
open CharParsers

(* Types *)
type Parser<'t> = Primitives.Parser<'t,unit>
type Expression =
  | Not of Expression
  | Equals of Expression * Expression
  | Add of Expression * Expression
  | Subtract of Expression * Expression
  | Multiply of Expression * Expression
  | Divide of Expression * Expression
  | Minus of Expression
  | Power of Expression * Expression
  | Value of Value
and Value =
  | Identifier of string
  | Integer of int

(* Parsers *)
let ws1 : Parser<unit> = many1 (pchar ' ' <|> pchar '\t') |>> ignore
let ws : Parser<unit> = ws1 <|> preturn ()
let str_ws s = pstring s .>> ws
let integer : Parser<Value> =
  let number : Parser<NumberLiteral> = 
    numberLiteral (NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowMinusSign) "number"
  let isInteger (i : NumberLiteral) = if i.IsInteger then preturn (System.Int32.Parse(i.String)) else fail (sprintf "Not an integer: %s" i.String)
  number >>=? isInteger |>> Integer
let parenthesized p = (p .>> ws) |> between (pchar '(' .>> ws) (pchar ')' .>> ws)
let pIdentifier : Parser<Value> =
  let isIdentifierFirstChar c = isLetter c || c = '_'
  let isIdentifierChar c = isLetter c || isDigit c || c = '_'
  (many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier") .>> ws |>> Identifier
let value : Parser<Value> = pIdentifier <|> integer
let valueExpression : Parser<Expression> =
  value .>> ws |>> Value

(* Expressions parser *)
let (expression : Parser<Expression>), expressionImplRef = createParserForwardedToRef<Expression,unit>()
let opp = new OperatorPrecedenceParser<_,unit,unit>()
let expr = opp.ExpressionParser
opp.TermParser <- valueExpression <|> between (str_ws "(") (str_ws ")") expr
opp.AddOperator(InfixOperator("=", ws, 5, Associativity.Left, fun x y -> Equals(x,y)))
opp.AddOperator(InfixOperator("+", ws, 10, Associativity.Left, fun x y -> Add(x,y)))
opp.AddOperator(InfixOperator("-", ws, 10, Associativity.Left, fun x y -> Subtract(x,y)))
opp.AddOperator(InfixOperator("*", ws, 20, Associativity.Left, fun x y -> Multiply(x,y)))
opp.AddOperator(InfixOperator("/", ws, 20, Associativity.Left, fun x y -> Divide(x,y)))
opp.AddOperator(PrefixOperator("-", ws, 30, false, fun x -> Minus(x)))
opp.AddOperator(InfixOperator("^", ws, 40, Associativity.Left, fun x y -> Power(x,y)))

expressionImplRef := expr

let test p str =
  match run p str with
  | Success(result, _, _) -> Result.Ok(result)
  | Failure(errorMsg, _, _) -> Result.Error(errorMsg)

let output = function
| Result.Ok(r) -> printfn "%A" r
| Result.Error(msg) -> printfn "%s" msg
  
[<EntryPoint>]
let main argv =
  test expression "20 + (21 + foo) * 3" |> output
  0 // return an integer exit code
