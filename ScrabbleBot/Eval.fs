// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H',4);('E',1);('L',1);('L',1);('O',1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add (a:SM<int>) (b:SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        ret(y + x)
        
    let sub (a:SM<int>) (b:SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        ret(x - y)
        
    let mul (a:SM<int>) (b:SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
        ret(y * x)
    let div  (a:SM<int>) (b:SM<int>) : SM<int> =
        a >>= fun x ->
        b >>= fun y ->
            match y with 
            | 0 -> fail DivisionByZero
            | _ -> ret(x / y)
        
        

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    
    
    
    let rec charEval cExp : SM<char> =
        match cExp with
            | C c           -> ret(c)
            | CV a          -> (arithEval a) >>= characterValue
            | ToUpper c     -> (charEval c) >>= fun ch -> ret(System.Char.ToUpper ch)
            | ToLower c     -> (charEval c) >>= fun ch -> ret(System.Char.ToLower ch)
            | IntToChar a   -> (arithEval a) >>= fun x -> ret ((char) x)
    and arithEval (aExp:aExp) : SM<int> =
        match aExp with
            | N a           -> ret(a)
            | V a           -> lookup a
            | WL            -> wordLength
            | PV a          -> (arithEval a) >>= pointValue
            | Mul (a, b)    -> mul (arithEval a) (arithEval b)
            | Add (a, b)    -> add (arithEval a) (arithEval b)
            | Sub (a, b)    -> sub (arithEval a) (arithEval b)
            | Div (a, b)    -> div (arithEval a) (arithEval b)
            | Mod (a, b)    ->
                let evalA = (arithEval a)
                let evalB = (arithEval b)
                sub evalA (mul (div evalA evalB) evalB)
            | CharToInt c   -> (charEval c) >>= fun x -> ret ((int) x)
    and boolEval bExp : SM<bool> =
        match bExp with
            | TT            -> ret true
            | FF            -> ret false
            | AEq (a, b)    -> (arithEval a) >>= fun x ->
                               (arithEval b) >>= fun y -> ret (x = y)
            | ALt (a, b)    -> (arithEval a) >>= fun x ->
                               (arithEval b) >>= fun y -> ret (x < y)
            | Not a         -> (boolEval a) >>= fun x -> ret (not x)
            | Conj (a, b)   -> (boolEval a) >>= fun x ->
                               (boolEval b) >>= fun y -> ret (x && y)
            | IsVowel c     ->
                (charEval c) >>= fun c ->
                ret <|
                match (System.Char.ToLower c) with
                    | 'a' | 'e' | 'u' | 'i'| 'o' -> true
                    | _ -> false
            | IsLetter c     ->
                (charEval c) >>= fun c ->
                ret <| System.Char.IsLetter c
            | IsDigit c     ->
                (charEval c) >>= fun c ->
                ret <| System.Char.IsDigit c

    // Adding word lookups
    let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
    let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
    let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")

    let arithDoubleWordScore = N 2 .*. V "_acc_"
    let arithTripleWordScore = N 3 .*. V "_acc_"
    
    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
            | Declare s -> declare s
            | Ass(s, aExp) -> arithEval aExp >>= update s 
            | Skip -> ret()
            | Seq (st1, st2) -> stmntEval st1 >>>= stmntEval st2
            | ITE (b, st1, st2) -> boolEval b >>= fun b -> if b then stmntEval st1 else stmntEval st2
            | While (b, st) -> boolEval b >>= fun b1 -> if b1 then stmntEval (Seq(st, While(b, st))) else ret ()
                    
                    
                                    
    
(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    let stmntToSquareFun stmnt : squareFun =
        fun w pos acc -> evalSM (mkState [("_pos_", pos); ("_acc_", acc)] w ["_result_"]) (stmntEval stmnt >>>= lookup "_result_")


    type coord = int * int

    type boardFun = coord -> Result<squareFun option, Error>
    type boardFun2 = coord -> Result<square option, Error>

    let stmntToBoardFun stmnt (m : Map<int,squareFun>) : boardFun2 =
        fun coord -> 
            let x = stmntEval stmnt >>>= 
                    lookup "_result_" |>
                    evalSM (mkState [("_x_", fst coord); ("_y_", snd coord); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"])
            match x with
            | Success i -> Success (Some (Map.empty.Add (i, (m.TryFind i).Value)))
            | _ -> failwith "stmntToBoardFun failed."

    type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"