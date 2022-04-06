// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser

    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) a b = a .>> spaces .>>. b 
    let (.>*>) a b = a .>> spaces .>> b 
    let (>*>.) a b  = a >>. spaces >>. b
    
    

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')' 

    let pid = pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_')|>> (fun (x,xs) -> x::xs) |>> List.toArray |>> System.String

    
    let unop op a = op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2 

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    let CharParse, cref = createParserForwardedToRef<cExp>()
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse <?> "Par"
    let NegParse = unop (pchar '-') TermParse |>> (fun x -> Mul (N (-1), x)) <?> "Neg"
    let PVParse = pPointValue >*>. parenthesise TermParse |>> PV <?> "PV"
    let VParse = pid |>> V <?> "Pid"
    let CTIParser = pCharToInt >*>. parenthesise CharParse |>> CharToInt <?> "CharToInt"
    do aref := choice [CTIParser; NegParse; ParParse; PVParse; VParse; NParse; ]

    let AexpParse = TermParse 
    
    
    let TLoParser = pToLower >*>. parenthesise CharParse |>> ToLower <?> "ToLower"
    let TUpParser = pToUpper >*>. parenthesise CharParse |>> ToUpper <?> "ToUpper"
    
    let ChaParser = pchar ''' >>. satisfy (fun _ -> true) .>> pchar ''' |>> C <?> "Char"
    
    
    let ITCParser = pIntToChar >*>. parenthesise TermParse |>> IntToChar <?> "IntToChar"
    let CVParser = pCharValue >*>. parenthesise TermParse |>> CV <?> "CV"
    do cref := choice [TLoParser; TUpParser; ITCParser; CVParser; ChaParser]
    let CexpParse = CharParse

    let BexpParse = pstring "not implemented"

    let stmParse = pstring "not implemented"

    (* The rest of your parser goes here *)
    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    // Default (unusable) board in case you are not implementing a parser for the DSL.
    let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
