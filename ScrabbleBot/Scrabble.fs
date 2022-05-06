namespace Sukuraburu

open System.Runtime.InteropServices.ComTypes
open Parser
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad
// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList
    let parseChange ts =
        let pattern = @"([0-9]+)[ ]" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [ id;] ->
                    id |> uint32
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MultiSet<uint32>
        playerTurn    : uint32
        playerCount   : uint32
        notForfeited  : bool list
        points        : int list
        placedTiles   : Map<(int * int),(uint32*(char*int))>
    }

    let mkState b d pn h pt pc nff p ptl = {board = b; dict = d;  playerNumber = pn; hand = h; playerTurn = pt; playerCount = pc; notForfeited = nff; points = p; placedTiles = ptl}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            let move, change =
                //if st.playerNumber = st.playerTurn then
                    Print.printHand pieces (State.hand st)
                    System.Console.WriteLine st.hand
                    System.Console.WriteLine "Move or Change or Forfeit or Pass?(m|c|f|p)\n\n"
                    let action = System.Console.ReadLine()[0]
                    // remove the force print when you move on from manual input (or when you have learnt the format)
                    match action with
                    | 'c' ->
                        forcePrint "Input change (format '(<id> )*')\n\n"
                        let input =  System.Console.ReadLine()
                        let change = RegEx.parseChange input
                        send cstream (SMChange change)
                        debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) change) // keep the debug lines. They are useful.
                        (None, Some(change))
                    | 'f' ->
                        forcePrint "Forfeited!\n\n"
                        send cstream SMForfeit
                        (None, None)
                    | 'p' ->
                        forcePrint "Passed!\n\n"
                        send cstream SMPass
                        (None, None)
                    | _ ->
                        
                        //forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                        //let input =  System.Console.ReadLine()
                        //let move = RegEx.parseMove input
                        forcePrint "calculating..."
                        let move = BestMove.suggestMove st.board st.placedTiles st.dict (MultiSet.toList st.hand pieces)
                        forcePrint "done!"
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                        send cstream (SMPlay move)
                        
                        debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                        (Some(move), None)    
                //else
                    //(None, None)
                    
            let msg = recv cstream
                        
            
            let rec removePieces i hand (ids : uint32 list) =
                let id = ids[i]
                if (i < ids.Length-1) then
                    removePieces (i+1) (MultiSet.removeSingle id hand) ids
                else
                    (MultiSet.removeSingle id hand)
                    
            let rec addPieces i hand (pieces : (uint32* uint32)List)=
                let id, amount = pieces[i]
                //System.Console.WriteLine (sprintf "id %d amount %d" id amount)
                if (i < pieces.Length-1) then
                    addPieces (i+1) (MultiSet.add id amount hand) pieces
                else
                    (MultiSet.add id amount hand)
                    
            let nextPlayer (st:State.state) curPlayer =
                let playerOrder = List.append (st.notForfeited[(((curPlayer |> int)+1)%(st.playerCount|>int))..]) (st.notForfeited[..(curPlayer |> int)])
                (curPlayer + ((List.findIndex id playerOrder) |> uint32))%st.playerCount
                
            let rec placeTiles placedTiles (tiles:((int*int)*(uint32*(char*int))) list) : Map<(int*int),(uint32*(char*int))>=
                match tiles with
                | [] -> placedTiles
                | (k,v)::l -> Map.add k v (placeTiles placedTiles l)
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                System.Console.WriteLine "----------"
                System.Console.WriteLine "before"
                System.Console.WriteLine st.hand
                let reducedHand = removePieces 0 st.hand (List.map (fun x -> x |> snd |> fst) ms)
                let newHand = addPieces 0 (reducedHand) newPieces
                
                let st' = {st with playerTurn = nextPlayer st st.playerNumber; hand = newHand; points = List.mapi (fun i v -> if i = (st.playerNumber |> int) then v + points else v) st.points; placedTiles = placeTiles st.placedTiles ms}  // This state needs to be updated
                System.Console.WriteLine "after"
                System.Console.WriteLine st'.hand
                System.Console.WriteLine "----------"
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = {st with playerTurn = nextPlayer st pid; points = List.mapi (fun i v -> if i = (pid |> int) then v + points else v) st.points; placedTiles = placeTiles st.placedTiles ms}  // This state needs to be updated
                aux st'
            | RCM (CMPassed pid) ->
                let st' = {st with playerTurn = nextPlayer st pid}  // This state needs to be updated
                aux st'
            | RCM (CMChange(pid, numTiles)) ->
                let st' = {st with playerTurn = nextPlayer st pid}  // This state needs to be updated
                aux st'
            | RCM (CMChangeSuccess newTiles) ->
                System.Console.WriteLine "----------"
                System.Console.WriteLine "before"
                System.Console.WriteLine st.hand
                let reducedHand = removePieces 0 st.hand change.Value
                let newHand = addPieces 0 (reducedHand) newTiles
                let st' = {st with playerTurn = nextPlayer st st.playerNumber; hand = newHand}  // This state needs to be updated
                System.Console.WriteLine "after"
                System.Console.WriteLine st'.hand
                System.Console.WriteLine "----------"
                aux st'
            | RCM (CMForfeit pid) ->
                let st' = {st with playerTurn = nextPlayer st pid; notForfeited = List.mapi (fun i v -> if i = (pid |> int) then false else v) st.notForfeited }  // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMTimeout pid) ->
                let st' = {st with playerTurn = nextPlayer st pid}  // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
        let printSquare coord = 
            let tt = board.squares coord
            match tt with
            | Success squareOption ->
                System.Console.WriteLine (sprintf "%d,%d = %A"(fst coord)(snd coord) ( squareOption))
            | Failure error -> System.Console.WriteLine error
            
        let rec aux r =
            let rec aux2 c =
                printSquare (r,c)
                if (c < 7) then
                    aux2 (c+1) 
            aux2 -7
            if (r < 7) then
                aux (r+1)
                
        aux -7
        
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn numPlayers (List.init (numPlayers |> int) (fun _ -> true)) (List.ofArray (Array.zeroCreate (numPlayers |> int))) Map.empty)
        