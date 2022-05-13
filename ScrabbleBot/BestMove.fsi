module internal BestMove
    
    val suggestMove : Parser.board -> Map<(int*int), uint32*(char*int)> -> (Dictionary.Dict list) -> ((uint32*Set<(char*int)>) list) -> (bool) -> ((int * int) * (uint32 * (char * int))) list
    

