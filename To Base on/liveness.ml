open Misc
open Ass
open Smallset

(* Seul bout de code dépendant de la cible *)
module Mach = Spim

(* Ensemble des registres spéciaux, à enlever des defs et des uses *)
let specials = Smallset.of_list Mach.special_registers

(* Ensemble des registres machine utilisables (graphe des moves) *)
let registers = Smallset.of_list Mach.registers

                          

type flowinfo = {
    instr : Ass.instr ;
    def : temp set;  use : temp set; 
    mutable live_in : temp set; mutable live_out : temp set;
  }

let flowinfo_bidon = {
  instr = Oper ("", [], [], None) ; (* plus nop, tu meurs *)
  def = empty ; use = empty ;
  live_in = empty ; live_out = empty ;
} 

(* Le graphe de flot

   - les noeuds  sont les instructions
   - un arc de b1 vers b2 signifie que le controle peut passer au
     de'but de b2 a` la fin de b1
    - Les noeuds sont de'core's par les uses et les defs du bloc
    - live_out et live_out initialement vides.
*)

type flowgraph = flowinfo Graph.t

(* affichages du graphe de flot (debug !) *)

let print_regs chan rs =
   Smallset.iter rs (fun t -> Printf.fprintf chan "%s " (Mach.namer t))

let print_node chan i =
  Ass.print chan Mach.namer i.instr;
  output_string chan "\t# "; print_regs chan i.def;
  output_string chan "<= "; print_regs chan i.use;
  output_string chan "\t# "; print_regs chan i.live_out;
  output_char chan '\n'

let print_flowgraph chan g =
  Graph.iter g
    (fun n -> print_node chan (Graph.info g n))

let debug_node chan i =
  Ass.print chan Mach.namer i.instr;
  output_string chan "\t# "; print_regs chan i.def;
  output_string chan "<= "; print_regs chan i.use;
  output_char chan '\n'

let debug_flowgraph chan g =
  Graph.debug chan (fun chan n -> debug_node chan (Graph.info g n)) g

  
let mk_graph code =
  (* cette table sera utile pour créer les arcs *)
  let lab2nodes = Hashtbl.create 17 in (* clé anglaise *)
  let g = Graph.create flowinfo_bidon in

(* D'abord créer les noeud et les arcs de l'exécution en séquence 
   en associant étiquettes et noeuds au passage *)
  let mk_node i def use =
    let i =
      { instr=i ; def = (diff def specials) ; use = (diff use specials) ;
        live_in = empty ; live_out = empty } in
    Graph.new_node g i in

  
  let rec mk_nodes pred = function
    | [] -> []
    | i::rem ->
        let node = match i with
        |  Oper (s, src, dst, _) ->
            mk_node i (of_list dst) (of_list src)
        | Move (s, src, dst) ->
            mk_node i (singleton dst) (singleton src)
        | Label (s,l) ->
            let n = mk_node i empty empty in
            Hashtbl.add lab2nodes l n ;
            n in
        begin match pred with
        | Some n_pred -> Graph.new_edge g n_pred node
        | None -> ()
        end ;
        begin match i with
        | Oper (_, _, _, Some labs) -> (node,labs)::mk_nodes None rem
        | _                      -> mk_nodes (Some node) rem
        end in

  let to_patch = mk_nodes None code in
  (* ajouter des arcs des sauts explicites *)
  List.iter
    (fun (n,labs) ->
      List.iter
        (fun lab ->
          try
            Graph.new_edge g n (Hashtbl.find lab2nodes lab)
          with
          | Not_found -> assert false)
        labs)
    to_patch ;
  (* C'est le résultat *)
  g

let fixpoint g =
  let encore = ref true
  and niters = ref 0 in
  (* Calcul dans l'ordre inverse des instructions *)
  let nodes = List.rev (Graph.nodes g) in 

  while !encore do
    if !verbose then begin
      Printf.fprintf stderr "***** Itération %d *******\n" !niters ;
      print_flowgraph stderr g
    end ;
      
    encore := false ;
    List.iter
      (fun node ->
        let info = Graph.info g node in
        let new_out =
          List.fold_left
            (fun r to_n ->
              union (Graph.info g to_n).live_in r)
            empty
            (Graph.succ g node) in
        let new_in = union info.use (diff new_out info.def) in
        encore :=  !encore || not (eqset info.live_out new_out) ;
        info.live_in <- new_in ;
        info.live_out <- new_out)
      nodes ;
    niters := !niters + 1
  done ;  
  !niters

(*****************************************)
(* Code décoré des defs et des live-outs *)
(*****************************************)

type flowinstr =
 {finstr:Ass.instr ;
 fdef: Gen.temp set ; fout : Gen.temp set}

let print_flowcode chan code =
  List.iter
    (fun i ->
      Ass.print chan Mach.namer i.finstr;
      output_string chan "\t# "; print_regs chan i.fout;
      output_char chan '\n')
    code

let flow_table =
  Table.create
    {finstr=Oper ("", [], [], None) ; fdef=empty ; fout=empty}

let flow code =
  let g = mk_graph code in
  let niters = fixpoint g in
  if !verbose then begin
    prerr_endline
      ("Graphe de flot calculé en "^string_of_int niters^" itérations") ;
    print_flowgraph  stderr g
  end ;
(* C'est le résultat *)
  Graph.iter g
    (fun n ->
      let i = Graph.info g n in
      Table.emit flow_table
        {finstr=i.instr ; fdef=i.def ; fout=i.live_out}) ;
  Table.trim_to_list flow_table

        
    

(*
  Les contenu des noeuds du graphe d'interférence
    - Un temporaire
    - D'autres informations, qui seront complétées plus tard 
*)

type interference = {
    temp : temp;
(* Les champs suivants sont utiles pour l'allocation de registres *)
    mutable color  : temp option ;
    mutable occurs : int ;
    mutable degree : int ;
    mutable elem   : ((interference Sgraph.node) Partition.elem) option ;
  }

(* Les deux sortes d'arcs *)
type ilab = Inter | Move_related

(* Fonction à utiliser pour créer les enregistrements « interference » *)
let mk_info t =
  {temp=t ; color = None ; occurs=0 ; degree=0 ; elem=None}


let ibidon = mk_info (Gen.new_temp ())

(*
  Affichage du graphe d'interférence (debug !)
*)

let print_adjs chan ig adjs =
  List.iter
    (fun n ->
      output_char chan ' ' ;
      output_string chan (Mach.namer (Sgraph.info ig n).temp))
    adjs

let print_interference chan ig =
  Sgraph.iter ig
    (fun n ->
      let i = Sgraph.info ig n in
      output_string chan (Mach.namer i.temp) ;
      output_string chan " <=>" ;
      print_adjs chan ig (Sgraph.adj ig n Inter) ;
      output_char chan '\n')

let print_moves chan ig =
  Sgraph.iter ig
    (fun n ->
      let i = Sgraph.info ig n in
      output_string chan (Mach.namer i.temp) ;
      output_string chan " <=>" ;
      print_adjs chan ig (Sgraph.adj ig n Move_related) ;
      output_char chan '\n')

type igraph = (interference, ilab) Sgraph.t


(****************************)
(* Fermeture des arcs moves *)
(****************************)
(*
  L'idée est de détecter les chaînes de move t1, t2, .... tn
  où les ti intermédiaires ne sont pas des registres.
  Cette étape est optionnelle.
*)
  
(* Quadratique, mais ça ne gêne pas trop, le graphe des moves étant creux *)
let reachable g n =   
  let rec dfs seen n =
    if mem n seen then seen
    else if mem (Sgraph.info g n).temp registers then
      add n seen
    else
      let adjs = Sgraph.adj g n Move_related in
      List.fold_left dfs (add n seen) adjs in
  dfs empty n
          
      
let ferme_moves g =
  Sgraph.iter g
    (fun n ->
      let seen = reachable g n in
      iter seen
        (fun m -> Sgraph.new_edge g n m Move_related))
  
(****************************************)
(* Fabrication du graphe d'interférence *)
(****************************************)
(*
  Retirer les registres spéciaux complique un peu le code,
  mais l'allocation de registres en sera simplifiée d'autant
*)

let interference code =
  let ig = Sgraph.create ibidon in
(* Besoin d'une association temporaire -> sommet *)
  let temp2nodes = Hashtbl.create 17 in
  let get_node r =
    try
      Hashtbl.find temp2nodes r
    with
    | Not_found ->
        let n = Sgraph.new_node ig (mk_info r) in
        Hashtbl.add temp2nodes r n ;
        n in
  
(* Fabrication du graphe proprement dit *)
  let step_instr i =
      match i.finstr with
      | Move (_,s,d) ->
          let s_special = mem s specials and d_special = mem d specials in
          if not d_special then begin
            let nd = get_node d in
            if not s_special then begin
              let ns = get_node s in
              Sgraph.new_edge ig ns nd Move_related
            end ;
            iter
              i.fout
              (fun r -> if r <> s then
                Sgraph.new_edge ig (get_node r) nd Inter)
          end else if not s_special then
            let _ = get_node s in ()
      | _ ->
          iter i.fdef
            (fun d ->
              let nd = get_node d in
              iter i.fout
                (fun r -> Sgraph.new_edge ig nd (get_node r) Inter)) in

  List.iter step_instr code ;

(* Fermer les moves, optionel *)
  ferme_moves ig ;
(* Le résultat *)
  ig

    
    



