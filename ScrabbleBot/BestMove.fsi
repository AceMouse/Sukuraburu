module internal BestMove
    
    open ScrabbleUtil
    open MultiSet
    val suggestMove : Parser.board -> Map<(int*int), (char*int)> -> (Dictionary.Dict) -> ( MultiSet<int>)-> ((int * int) * (uint32 * (char * int))) list
    

