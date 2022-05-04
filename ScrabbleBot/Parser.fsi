// This file is new and hides the implementation details of the parser.

module internal Parser
    
    open ScrabbleUtil
    open StateMonad
    open Eval
    
    
    type boardFun2 = coord -> Result<square option, Error>
        
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    val mkBoard : boardProg -> board