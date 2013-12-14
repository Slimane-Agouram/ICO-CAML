type demand =
    Pretty | Interpret | IPretty | Simul | Compile | Liveness | Spim | Spill
  | IFlowGraph
val what : demand ref
val prog : string ref
val verbose : bool ref
val nregs : int ref
val dump_flowgraph : bool ref
val dump_flowgraph_opt : bool ref
val dump_icode: bool ref
