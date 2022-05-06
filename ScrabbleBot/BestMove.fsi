module internal BestMove
    
    open ScrabbleUtil
    open MultiSet
    val suggestMove : Parser.board -> Map<(int*int), uint32*(char*int)> -> (Dictionary.Dict) -> ((uint32*Set<(char*int)>) list)-> ((int * int) * (uint32 * (char * int))) list
    

