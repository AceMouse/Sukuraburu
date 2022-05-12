// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)
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
    let curlysise p = pchar '{' >*>. p .>*> pchar '}' 

    let pid = (pletter <|> pchar '_') .>>. many (palphanumeric <|> pchar '_') |>> (fun (x,xs) -> x::xs) |>> List.toArray |>> System.String

    
    let unop op a = op >*>. a
    let binop op p1 p2 = p1 .>*> op .>*>. p2 

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref.Value <- choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref.Value <- choice [MulParse; DivParse; ModParse; AtomParse]
    let CharParse, cref = createParserForwardedToRef<cExp>()
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse <?> "Par"
    let NegParse = unop (pchar '-') TermParse |>> (fun x -> Mul (N (-1), x)) <?> "Neg"
    let PVParse = pPointValue >*>. parenthesise TermParse |>> PV <?> "PV"
    let VParse = pid |>> V <?> "Pid"
    let CTIParser = pCharToInt >*>. parenthesise CharParse |>> CharToInt <?> "CharToInt"
    do aref.Value <- choice [CTIParser; NegParse; ParParse; PVParse; VParse; NParse; ]

    let AexpParse = TermParse 
    
    
    let TLoParser = pToLower >*>. parenthesise CharParse |>> ToLower <?> "ToLower"
    let TUpParser = pToUpper >*>. parenthesise CharParse |>> ToUpper <?> "ToUpper"
    
    let ChaParser = pchar ''' >>. satisfy (fun _ -> true) .>> pchar ''' |>> C <?> "Char"
    
    
    let ITCParser = pIntToChar >*>. parenthesise TermParse |>> IntToChar <?> "IntToChar"
    let CVParser = pCharValue >*>. parenthesise TermParse |>> CV <?> "CV"
    do cref.Value <- choice [TLoParser; TUpParser; ITCParser; CVParser; ChaParser]
    let CexpParse = CharParse
    
    let BTermParse, btref = createParserForwardedToRef<bExp>()
    let BProdParse, bpref = createParserForwardedToRef<bExp>()
    let BAtomParse, baref = createParserForwardedToRef<bExp>()
    let TruParser = pTrue |>> (fun _ -> TT) <?> "TT"
    let FlsParser = pFalse |>> (fun _ -> FF) <?> "FF"
    let BParParse = parenthesise BTermParse <?> "Par"
    let AndParser =  BProdParse .>*> pstring @"/\" .>*>. BProdParse |>> Conj <?> "Conj"
    let OrParser =  BProdParse .>*> pstring @"\/" .>*>. BProdParse |>> (fun (a,b) -> ((.||.) a b)) <?> "Disj"
    let AEqParser = AexpParse .>*> pchar '=' .>*>. AexpParse |>> AEq <?> "AEq"
    let ANEqParser =  AexpParse .>*> pstring "<>" .>*>. AexpParse |>> (fun (a,b) -> ((.<>.) a b)) <?> "ANEq"
    let ALTParser =  AexpParse .>*> pchar '<' .>*>. AexpParse |>> (fun (a,b) -> ((.<.) a b)) <?> "ALT"
    let ALEqParser =  AexpParse .>*> pstring "<=" .>*>. AexpParse |>> (fun (a,b) -> ((.<=.) a b)) <?> "ALEq"
    let AGTParser =  AexpParse .>*> pchar '>' .>*>. AexpParse |>> (fun (a,b) -> ((.>.) a b)) <?> "AGT"
    let AGEqParser =  AexpParse .>*> pstring ">=" .>*>. AexpParse |>> (fun (a,b) -> ((.>=.) a b)) <?> "AGEq"
    let NegParser = pchar '~' >*>. BTermParse |>> Not <?> "Not"
    let IsLetter = pIsLetter >*>. parenthesise CharParse |>> IsLetter <?> "IsLetter"
    let IsVowel = pIsVowel >*>. parenthesise CharParse |>> IsVowel <?> "IsVowel"

    let IsDigit = pIsDigit >*>. parenthesise CharParse |>> IsDigit <?> "IsDigit"
    
    do btref.Value <- choice [AndParser; OrParser; BProdParse]
    do bpref.Value <- choice [AEqParser; ANEqParser; ALTParser; ALEqParser; AGTParser; AGEqParser; BAtomParse]
    do baref.Value <- choice [NegParser; IsLetter; IsVowel; IsDigit; TruParser; FlsParser; BParParse]
    let BexpParse = BTermParse
    let stmntParser, sref = createParserForwardedToRef<stm>()
    let stmntSeqParser, seqref = createParserForwardedToRef<stm>()
    
    let DeclareParser = pdeclare >>. spaces1 >>. pid |>> Declare <?> "Declare"
    let AssParser = pid .>*> pstring ":=" .>*>. TermParse |>> Ass <?> "Ass"
    let SeqParser = stmntSeqParser .>*> pchar ';' .>*>. stmntParser |>> Seq <?> "Seq"
    let ITEParser = pif >*>. BParParse .>*> pthen .>*>. curlysise stmntParser .>*> pelse .>*>. curlysise stmntParser |>> (fun ((b, st1), st2) -> ITE (b, st1, st2)) <?> "ITE"
    let ITParser = pif >*>. BParParse .>*> pthen .>*>. curlysise stmntParser |>> (fun (x, y) -> ITE (x, y, Skip)) <?> "IT"
    let WhileParser = pwhile >*>. parenthesise BTermParse .>*> pdo .>*>. curlysise stmntParser |>> While <?> "While"
    let SkipParse = pstring "Skip" |>> (fun _ -> Skip) <?> "Skip"
    
    do sref.Value <- choice [SeqParser; stmntSeqParser] 
    do seqref.Value <- choice [AssParser; DeclareParser; ITEParser; ITParser; WhileParser; SkipParse]

    (* The rest of your parser goes here *)
    
    let parseSquareProg (sqp : squareProg) : square =
        Map.map (fun _ w -> stmntToSquareFun (getSuccess (run stmntParser w))) sqp

    let parseBoardProg prog (sqs : Map<int, square>) : boardFun2 =
         stmntToBoardFun (getSuccess (run stmntParser prog)) sqs
    
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }
    
    let mkBoard (prog:boardProg) : board = 
        {
            center = prog.center
            defaultSquare = Map.find prog.usedSquare (Map.map (fun _ -> parseSquareProg) prog.squares)
            squares = parseBoardProg prog.prog (Map.map (fun _ -> parseSquareProg) prog.squares)
        }
    