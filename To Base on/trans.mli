type 'a procedure = Frame.frame * 'a
type 'a program =
    { number_of_globals : int;
      main : 'a procedure;
      procedures : 'a procedure list
    } 

val program : Pp.program -> Code.stm program
