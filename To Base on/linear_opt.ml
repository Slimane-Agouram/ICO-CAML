open Printf
open Code

(*****************************************)
(* Type des blocs de base, usage interne *)
(*****************************************)

type basic_block = {enter:Gen.label ; mutable succ:stm ; body:stm list}


(* Fonction pratique qui donne les étiquettes des successeurs d'un bloc *)
let get_succ b = match b.succ with
| Jump lab -> [lab]
| Cjump (_,_,_,lab1,lab2) -> [lab1 ; lab2]
|  _ -> assert false

(* Utilitaire d'affichage du graphe de flot *)

let print_flow_graph blocks =
  let print_node_and_arcs block =
    let lab = block.enter in
    begin match block with
    | {body=[] ; succ=Jump _} ->
        printf "  %s [fillcolor=gray, style=filled];\n"
          (Gen.label_string lab)
    | _ ->  ()
    end ;
    let print_arc tolab =
      printf "  %s -> %s;\n"
        (Gen.label_string lab)
        (Gen.label_string tolab)
    in
    List.iter print_arc (get_succ block) 
  in
  List.iter print_node_and_arcs blocks

(*******************************)
(* Fabriquer les blocs de base *)
(*******************************)

(* Découpe en blocs de base.
  Type:
    Gen.label -> Gen.label -> code -> basic_block list
  Appel: 
    « cut_blocks lstart lend code »
   Les arguments sont
     - lstart étiquette d'entrée dasn la fonction.
     - lexit étiquette de sortie (ie celle de l'épilogue).
     - code: liste des instructions du corps de la fonction.
   Revoie la liste des blocs de base, avec
     - Le premier bloc porte l'étiquette lstart.
     - Un saut vers lend signifie retourner de la fonction.
*)


let cut_blocks start_label exit_label code =
  let rec in_block cur_lab cur_ydob = function
    | Label lab::rem ->
        let r = in_block lab [] rem in
        {enter=cur_lab ; succ=Jump lab ; body=List.rev cur_ydob}::r
    | (Jump _ | Cjump (_,_,_,_,_)) as stm::rem ->
        let r = start_block rem in
        {enter=cur_lab ; succ=stm ; body=List.rev cur_ydob}::r
    | stm::rem ->
        in_block cur_lab (stm::cur_ydob) rem
    | []       -> (* dernier bloc, ajsuccer un saut vers end_label *)
        [{enter=cur_lab ; succ=Jump exit_label ; body=List.rev cur_ydob}]

  and start_block stms = match stms with
  | Label lab::rem -> in_block lab [] rem
(* on pourrait aussi chercher l'étiquette suivante *)
  | _              -> assert false in
  in_block start_label [] code

let code_to_blocks f code =
  let start_label = Frame.frame_name f in
  let exit_label = Frame.frame_return f in
  cut_blocks start_label exit_label code;;


(***************************************************)
(* Reconstituer le code à partir des blocs de base *)
(***************************************************)

let blocks_to_code blocks = match blocks with
| {body=stms ; succ=stm}::rem ->
    stms @
    stm ::
    List.fold_right
      (fun {enter=lab; body=stms; succ=stm} r -> 
        Label lab::stms @ stm :: r)
      rem []
| _ -> assert false

(***************************************************)
(* Optimisations du contrôle sur le graphe de flot *)
(***************************************************)

(*
 Le graphe de flot est représenté par
   labs -> liste d'étiquettes
   t    -> table de hachage des étiquettes vers les blocs
*)
let blocks_to_flowgraph blocks =
  let t = Hashtbl.create 17 in
  let labs =
    List.map
      (fun b ->
        let lab = b.enter in
        Hashtbl.add t lab b ;
        lab)
      blocks in
  labs, t

let get_block t lab = Hashtbl.find t lab

(* optimisation proprement dites *)

(* Court-circuitage des blocs vides *)
let rec shorten_lab t lab =
  try
    let b = get_block t lab in
    match b with
    | {body=[]; succ=Jump olab} -> shorten_lab t olab
    | _ -> lab
  with
  | Not_found -> lab

let shorten_block t b = match b.succ with
| Jump lab -> b.succ <- Jump (shorten_lab t lab)
| Cjump (op,e1,e2,lab1,lab2) ->
    b.succ <- Cjump (op, e1, e2, shorten_lab t lab1, shorten_lab t lab2)
| _ -> ()

let shorten_blocks t blocks = List.iter (shorten_block t) blocks

(* ensembles sans se fatiguer *)
let empty = []
let mem x l = List.mem x l
let add x l = if mem x l then l else x::l

(* Renvoie la liste des blocs atteignables à partir de l'étiquette
   d'entrée *)
let remove_unreachable t labs =

  let rec dfs r lab =
    if mem lab r then r
    else
      let r = add lab r in
      if Hashtbl.mem t lab then
        let succ = get_succ (get_block t lab) in
        List.fold_left dfs r succ
      else
        r in

  match labs with
  | start::rem ->
      let reach = dfs [] start in
      let labs = start::List.filter (fun lab -> mem lab reach) rem in
      List.map (get_block t) labs
  | _ -> assert false

(************************************)
(* Optimisation « trou de serrure » *)
(************************************)

(*
   Faire le minimum, c'est-à-dire forcer
   Cjump (_,_,_,l1,l2) ; Label l2 ; ..
*)
let neg = function
  | Req -> Rne | Rne -> Req
  | Rle -> Rgt | Rge -> Rlt | Rlt -> Rge | Rgt -> Rle

let rec peephole f = function
(* Cas particulier du saut vers l'épilogue en fin de code *)
  | [Cjump (relop, e1, e2, lab1 , lab2)] when lab1 = Frame.frame_return f ->
       [Cjump (neg relop, e1, e2, lab2 , lab1)]
  | [Cjump (relop, e1, e2, lab1 , lab2)] when lab2 = Frame.frame_return f ->
       [Cjump (relop, e1, e2, lab1 , lab2)]
  | [Jump lab] when lab=Frame.frame_return f -> []
(* Tous les sauts *)
  | Cjump (binop, c1, c2, l1, l2) :: t ->
      begin match t with
      | Label l3 :: t3 when l1 = l3 ->
          Cjump (neg binop, c1, c2, l2, l1) ::
          Label l3 :: peephole f t3
      | Label l3 :: t3 when l2 = l3 ->
          Cjump (binop, c1, c2, l1, l2) ::
          Label l3 :: peephole f t3
      | _ ->
          let l4 = Gen.new_label () in
          Cjump (binop, c1, c2, l1, l4) ::
          Label l4 :: Jump l2 :: peephole f t
      end
  | Jump l1 :: Label l2 :: t when l1 = l2 ->
      Label l2 :: peephole f t
(* h n'est pas un saut *)
  | h :: t ->
      h :: peephole f t 
  | [] -> []
              

(* Organisation des optmimisations *)
let opt_trace f c = 
  let blocks = code_to_blocks f c in
  if !Misc.dump_flowgraph then print_flow_graph blocks;
  let labs,t = blocks_to_flowgraph blocks in
  shorten_blocks t blocks ;
  let blocks = remove_unreachable t labs in
  if !Misc.dump_flowgraph_opt then print_flow_graph blocks;
  peephole f (blocks_to_code blocks)
(**************************************************)
(* Affichage du code intermédiaire à divers stades *)
(**************************************************)

let pcode pfun f c = if !Misc.dump_icode then pfun f c

let porig f c =
  printf "*** Code généré ***\n" ;
  Iprint.frame_code (f,c)


let pcanon _ c =
  printf "*** Code canonique et linéarisé ***\n" ;
  Iprint.code c

let popt _ c =
  printf "*** Code après optimisation du contrôle ***\n" ;
  Iprint.code c ;
  print_newline ()

(*********************************************)    
(* Itérer sur chaque fonction d'un programme *)
(*********************************************)    

open Trans

let map f p = 
  { number_of_globals = p.number_of_globals ;
    main =
      (match p.main with (frame,code) -> frame,f frame code);
    procedures =
    List.map (fun (frame, code) -> frame, f frame code)
      p.procedures;
  } 


let program p =
  if !Misc.dump_flowgraph || !Misc.dump_flowgraph_opt then
    printf "digraph \"%s\" {\n" (Filename.basename !Misc.prog);
  let output =
    map (fun f c ->
      pcode porig f c ;
      let c = Canon_opt.code c in
      pcode pcanon f c ;
      let c = opt_trace f c in
      pcode popt f c ;
      c) p in
  if !Misc.dump_flowgraph || !Misc.dump_flowgraph_opt then printf "}\n";
  output
