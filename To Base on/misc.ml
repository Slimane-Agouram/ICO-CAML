type demand =
    Pretty | Interpret | IPretty | Simul | Compile | Liveness | Spim
  | Spill | IFlowGraph
let what = ref Compile
let prog = ref ""
let verbose = ref false
let nregs = ref (-1)
let inline = ref 0
let dump_flowgraph = ref false
let dump_flowgraph_opt = ref false
let dump_icode = ref false

let usage () =
  prerr_endline 
"Usage: zyva [opt] [-v] programme
   + sans option, compile le programme
   + l'option ``-v'' affiche divers diagnostics
   + l'option ``opt'' est l'une des suivantes :
      -pp, imprime le source
      -i, interprète le programme
      -pc, imprime le code intermédiaire
      -pg, imprime le graphe de flot du code intermédiaire avant optimisation
      -pog, imprime le graphe de flot du code intermédiaire après optimisation
      -ic, interprète le code intermédiaire
      -liveness, imprime le graphe des durées de vie
      -spim, imprime le code spim avec les temporaires (non exécutable)
      -3, alloue 3 registres seulement
      -4, alloue 4 registres seulement
      -5, alloue 5 registres seulement
      -spill, n'utilise aucune allocation de registre
      -help, affiche ce message
"
;;

for i = 1 to Array.length Sys.argv-1 do
 match Sys.argv.(i) with
  | "-pp" -> what := Pretty
  | "-i"  -> what := Interpret
  | "-pc" -> what := IPretty ; dump_icode := true ;
  | "-pg" -> what := IFlowGraph ; dump_flowgraph := true ;
  | "-pog" -> what := IFlowGraph ; dump_flowgraph_opt := true ;
  | "-ic" -> what := Simul
  | "-liveness" -> what := Liveness
  | "-spim" -> what := Spim
  | "-spill" -> what := Spill
  | "-v"  -> verbose := true
  | "-3"  -> nregs := 3
  | "-4"  -> nregs := 4
  | "-5"  -> nregs := 5
  | "-help" -> usage () ; exit 1
  | name  -> prog := name
done
;;

