namespace Sukuraburu

open System
open System.Runtime.InteropServices.ComTypes
open Parser
open ScrabbleBot
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
        MultiSet.fold (fun _ x i -> System.Console.Write (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

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
    let playGame cstream pieces (st : State.state) infinite =
        let dictPath = (Directory.GetCurrentDirectory() + "/../../../../ScrabbleBot/Dictionaries/English.txt")
        printfn "%s" dictPath;
        let dicts = (DickSplitter.splitDictionary dictPath)

        let rec aux (st : State.state) =
            
            let move, change = if st.playerTurn = st.playerNumber then
                                    forcePrint "calculating... \n"
                                    Print.printHand pieces (State.hand st)
                                    let move = BestMove.suggestMove st.board st.placedTiles dicts (MultiSet.toList st.hand pieces) infinite
                                    if not move.IsEmpty then 
                                        forcePrint (sprintf "done!\n found move %A" move)
                                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                                        send cstream (SMPlay move)
                                        
                                        debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                                        (Some(move), None)   
                                    else
                                        forcePrint "Changing tiles\n\n"
                                        //let input =  System.Console.ReadLine()
                                        let change = List.map fst (MultiSet.toList st.hand pieces)
                                        send cstream (SMChange change)
                                        debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) change) // keep the debug lines. They are useful.
                                        (None, Some(change))
                                else
                                    (None, None)
            let msg = recv cstream
                        
                        
            
            // Remove pieces with matching ids from the hand
            let removePieces hand (ids : uint32 list) =
                let rec aux i : MultiSet.MultiSet<uint32> =
                    let id = ids[i]
                    if (i < ids.Length-1) then
                        MultiSet.removeSingle id (aux (i+1))
                    else
                        MultiSet.removeSingle id hand
                aux 0
                    
            let rec addPieces i hand (pieces : (uint32 * uint32) list)=
                let id, amount = pieces[i]
                if (i < pieces.Length-1) then
                    addPieces (i+1) (MultiSet.add id amount hand) pieces
                else
                    (MultiSet.add id amount hand)
                    
            let nextPlayer (st:State.state) curPlayer =
                let player = (curPlayer |> uint32) - 1u
                let s, e = List.splitAt curPlayer st.notForfeited
                let playerOrder = e @ s
                let offset = (List.findIndex id playerOrder |> uint32)+ 1u
                let ret = ((player + offset)%st.playerCount) + 1u
                ret
                
            let rec placeTiles placedTiles (tiles:((int*int)*(uint32*(char*int))) list) : Map<(int*int),(uint32*(char*int))>=
                match tiles with
                | [] -> placedTiles
                | (k,v)::l -> Map.add k v (placeTiles placedTiles l)
            
            match msg with
            | RCM (CMPlaySuccess(move, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                
                //                                     | Maps a function that will return the ids of the moved pieces
                let reducedHand = removePieces st.hand (List.map (fun (_, x) -> fst x) move)
                let newHand = addPieces 0 (reducedHand) newPieces
                
                // Update state
                let st' = {st with
                               playerTurn = nextPlayer st (st.playerTurn |> int)
                               hand = newHand
                               points = List.mapi (fun i v -> if i = (st.playerNumber |> int) then v + points else v) st.points
                               placedTiles = placeTiles st.placedTiles move}  // This state needs to be updated
                
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let newPlacedTiles = placeTiles st.placedTiles ms
                let st' = {st with
                               playerTurn = nextPlayer st (st.playerTurn |> int)
                               points = List.mapi (fun i v -> if i = (pid |> int) then v + points else v) st.points
                               placedTiles = newPlacedTiles}  // This state needs to be updated
                aux st'
            | RCM (CMPassed pid) ->
                let st' = {st with playerTurn = nextPlayer st (pid |> int)}  // This state needs to be updated
                aux st'
            | RCM (CMChange(pid, numTiles)) ->
                let st' = {st with playerTurn = nextPlayer st (pid |> int)}  // This state needs to be updated
                aux st'
            | RCM (CMChangeSuccess newTiles) ->
                let reducedHand = removePieces st.hand change.Value
                let newHand = addPieces 0 (reducedHand) newTiles
                let st' = {st with
                               playerTurn = nextPlayer st (st.playerTurn |> int)
                               hand = newHand}  // This state needs to be updated
                aux st'
            | RCM (CMForfeit pid) ->
                let st' = {st with
                               playerTurn = nextPlayer st (pid |> int)
                               notForfeited = List.mapi (fun i v -> if i = (pid |> int) then false else v) st.notForfeited }  // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMTimeout pid) ->
                let st' = {st with playerTurn = nextPlayer st (pid |> int)}  // This state needs to be updated
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
        
        // Prints the square at a given coordinate
        let printSquare coord = 
            let tt = board.squares coord
            match tt with
            | Success squareOption ->
                printfn "%A = %A" coord squareOption.Value
            | Failure error -> System.Console.WriteLine error
        
        // Prints the squares around the center within a given radius
        let rec aux r =
            let rec aux2 c =
                printSquare (r,c)
                if (c < 7) then
                    aux2 (c+1) 
            aux2 -7
            if (r < 7) then
                aux (r+1)
        
        // Print all squares around the center within a radius of 7
        aux -7
        
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet playerTurn numPlayers (List.init (numPlayers |> int) (fun _ -> true)) (List.ofArray (Array.zeroCreate (numPlayers |> int))) Map.empty) boardP.isInfinite
        