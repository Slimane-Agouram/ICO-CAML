val exp : Code.exp -> unit
val stm : Code.stm -> unit
val code : Code.stm list -> unit
val frame_code : Frame.frame * Code.stm -> unit

val program : Code.stm Trans.program -> unit
val program_canon : Code.stm list Trans.program -> unit
