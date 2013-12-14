open Misc
open Gen
open Spim
open Liveness
open Sgraph
open Smallset
open Partition
open Liveness

module Mach = Spim

(* Pour le debug et la trace *)
let elt_to_info ig elt =
  let n = Partition.info elt in
  Sgraph.info ig n

let prerr_reg  t =
 prerr_string " " ;
 prerr_string (Mach.namer t)

let prerr_regs ts = List.iter prerr_reg ts

let prerr_elts ig nodes =
   List.iter
    (fun elt -> prerr_reg (elt_to_info ig elt).temp) nodes

let prerr_part ig p =
  List.iter
    (fun s -> prerr_reg (elt_to_info ig s).temp) (list p) ;
  prerr_endline ""


(*
  Partition des sommets du graphe d'interférence
*)
    
let sets = make 6

let precolored = sets.(0) (* precolored, remains here for ever *)
and low        = sets.(1) (* candidates for simplify *)
and high       = sets.(2) (* candidates for spill *)
and spilled    = sets.(3) (* to be spilled *)
and colored    = sets.(4) (* colored *)
and in_stack   = sets.(5) (* on the stack waiting to be colored *)



(* Deux trois trucs sur couleurs *)
let ncolors = List.length Mach.registers
let colors = of_list Mach.registers

(* Fabrication d'un graphe d'interférence enrichi et
   répartition des sommets en precolored/iinitial *)

let build code =
 (* Graphe de flot *)
  let old_verbose = !verbose in
  verbose := false ;

  let fcode = Liveness.flow code in

 (* Graphe d'interférence *)
  let ig = Liveness.interference fcode in

  verbose := old_verbose ;
  if !verbose then begin
    prerr_endline "**** Liveness *****" ;
    print_flowcode stderr fcode ;
    prerr_endline "**** Graphe d'interférence *****" ;
    print_interference stderr ig ;
    prerr_endline "**** Graphe des moves *****" ;
    print_moves stderr ig
  end ;

 (*************************************)
 (* Enrichir le graphe d'interférence *)
 (* et produire la partition initiale *)
 (*************************************)
  Partition.clear sets ; (* impératif *)

  (* première étape, ajuster champs degree, color et répartir *)
  Sgraph.iter ig
    (fun n ->
      let i = Sgraph.info ig n in
      i.degree <- List.length (Sgraph.adj ig n Inter) ;
      i.color  <- (if mem i.temp colors then Some i.temp else None) ;
      let elem =
        match i.color with
        | Some _ -> Partition.put precolored n
        | None   ->
            if i.degree < ncolors then
              Partition.put low n
            else
              Partition.put high n in
      i.elem <- Some elem) ;

  (* deuxième étape, calcul champ occurs *)
  (* association des temporaires aux sommets du graphe d'interférence *)
  let temp2nodes = Hashtbl.create 17 in (* clé anglaise *)
  Sgraph.iter ig (fun n -> Hashtbl.add temp2nodes (Sgraph.info ig n).temp n) ;

  let get_inode t = Hashtbl.find temp2nodes t in
      (* Peut échouer pour un registre présent dans le code, mais pas
         dans le graphe d'interférence.
          - Couleur lue seulement.
          - Registre spécial *)

  let incr_occur t =
    try
      let n = get_inode t in
      let i = Sgraph.info ig n in
      i.occurs <- i.occurs + 1
    with
    | Not_found -> () in

  List.iter
    (fun i -> match i.finstr with
    | Ass.Move (_, p, q)    -> incr_occur p ; incr_occur q
    | Ass.Oper (_, ps, qs, _) ->
      List.iter incr_occur ps ;
      List.iter incr_occur qs
    | _ -> ())
    fcode ;

(* Rendre le graphe d'interférence bien décoré *)
  ig


(* Besoin de ça deux fois *)
let is_ephemere ig elt =
  let n = Partition.info elt in
  let i = Sgraph.info ig n in
  Gen.is_ephemere i.temp

(* Décrementer les degrés et passage high -> low oucazou *)
let decrement_degree ig elt =
  if belong elt low || belong elt high then begin
    let n = Partition.info elt in
    let i = Sgraph.info ig n in 
    let d = i.degree in
    i.degree <- d -1;
    if d = ncolors then begin
      if !verbose then begin
        prerr_string "From high" ;
        prerr_reg i.temp ;
        prerr_endline ""
      end ;
      move elt low
    end
  end else
    assert (belong elt in_stack || belong elt precolored)

(* Phase Symplify *)
let simplify ig elt =
  let n = Partition.info elt in
  if !verbose then begin
    let i = Sgraph.info ig n in 
    prerr_string "Push: " ;
    prerr_reg i.temp ;  
    prerr_string (" ("^string_of_int i.degree^") ") ;
    prerr_endline "" ;
  end ;
(* Mettre dans la pile *)
  move elt in_stack ;
(* Ne pas oublier de décrementer les degrés des voisins *)
  let adjs = Sgraph.adj ig n Liveness.Inter in
  List.iter
    (fun n ->
      let i = Sgraph.info ig n in
      match i.elem with
      | Some elt -> decrement_degree ig elt
      | _        -> assert false)
    adjs

let cost ig elt =
  let n = Partition.info elt in
  let i = Sgraph.info ig n in
  (* Heuristique de coût pour le spill *)
  let c = 1.0 /. (1.0 /. (float i.occurs)  +. 100.0 *. (float i.degree)) in
  (* Un éphémère introduit par un spill précédent coûte beaucoup *)
  let c = if is_ephemere ig elt then c +. 200. else c in
  if !verbose then begin
    prerr_string "Cost " ;
    prerr_reg i.temp ;
    prerr_string " = " ;
    prerr_float c ;
    prerr_string (" (occs="^string_of_int i.occurs) ;
    prerr_string (") (deg="^string_of_int i.degree) ;
    prerr_endline ")"
  end ;
  c

let potential = ref []

(* Choix d'un spill potentiel *)
let select_spill ig = match pick_lowest (cost ig) high with
| Some elt as r ->
    if !verbose then begin
      let n = Partition.info elt in
      let i = Sgraph.info ig n in
      prerr_string "Potential spill: " ;
      prerr_reg i.temp ;
      prerr_endline "" ;
      potential := elt :: !potential
    end ;
    r
| None -> None

    
(********************)
(* Boucle pricipale,*)
(*  du colorieur    *)
(********************)

(* Renvoyer la pile de coloriage *)
let make_stack ig =
  let rec step stack  = match pick low with
  | Some elt -> simplify ig elt ; step (elt::stack)
  | None     ->
      match select_spill ig with
      | None     -> stack
      | Some elt -> simplify ig elt ; step (elt::stack) in

  step []


(* Coloriage biasé *)

(* L'ensemble des couleurs d'une liste de sommets du graphe d'interférence *)
let prerr_color r  = prerr_string (Mach.namer r)

let prerr_colors rs =  Smallset.iter rs (fun r -> prerr_color r ; prerr_string " ")

let get_colors ig inodes =
  let cs =
    List.fold_left
      (fun r n ->
        let i = Sgraph.info ig n in
        match i.color with
        | Some c -> singleton c::r
        | None   -> r)
      [] inodes in
  Smallset.union_list cs

let choose_color ig elt =
 let n = Partition.info elt in
 let ok_colors = diff colors (get_colors ig (Sgraph.adj ig n Inter)) in
 let wish = get_colors ig (Sgraph.adj ig n Move_related) in
 let col = match choose (inter ok_colors wish) with
 | Some _ as r -> r
 | None ->
     let avoid =
       union_list
         (List.map
            (fun n -> get_colors ig (Sgraph.adj ig n Move_related))
            (Sgraph.adj ig n Inter)) in
     match choose (diff ok_colors avoid) with
     | Some _ as r -> r
     | None        -> choose ok_colors in
 if !verbose then begin
   let t = (Sgraph.info ig n).temp in
   prerr_color t ;
   match col with
   | Some r -> prerr_string " -> " ; prerr_color r ; prerr_endline ""
   | None   -> prerr_endline " -> No color"
 end ;
 col


let assign_colors ig stack =
  let rec step stack = match stack with
  | []       -> Partition.list spilled (* Terminé *)
  | elt::rem ->
      match choose_color ig elt with
      | Some c ->
          let n = Partition.info elt in
          let i = Sgraph.info ig n in
          i.color <- Some c ;
          move elt colored ;
          step rem
      | None ->
          move elt spilled ;
          step rem in
  step stack

(**********************)
(* Toilettage du code *)
(**********************)


(* Enlever les ajustements de pile *)
let remove_that that l =
  List.filter
    (function
      | Ass.Oper (i,_,_,_) ->
          List.fold_left
            (fun r y -> r && i != y) (* ine'galite' physique *)
            true that
      | _ -> true)
    l
(* Calculer  la taille du frame de la fonction *)
(* Si c'est nul, on enlève les ajustements de pile *)
let fix_size f remove_them code =
  let size = Frame.frame_size f in
  Ass.Oper
    ("\n#FUNSTART "^Gen.label_string (Frame.frame_name f),
     [],[],None):: (* pour faire joli *)
  Ass.Oper
    (Frame.frame_size_label f^"="^string_of_int size,
     [],[],None)::
  (match size with
  | 0 -> remove_that remove_them code
  | _ -> code)
;;

(**********************************)
(* Effectivement colorier le code *)
(**********************************)

let make_map f nodes =
  let map = Hashtbl.create 13 in
  List.iter (f map) nodes;
  map


let register_map ig nodes =
  let f map elt =
    let i = elt_to_info ig elt in
    match i.color with
    | Some c -> Hashtbl.add map i.temp c
    | _ -> assert false in
  make_map f nodes

let assign_registers_instr map = 
  let reg r = try Hashtbl.find map r with  Not_found -> r in
  fun instr k -> match instr with
    | Ass.Oper (op, s, d, l) ->
        Ass.Oper (op, List.map reg s, List.map reg d, l)::k
    | Ass.Move (op, s, d) ->
        let new_s = reg s and new_d = reg d in
        if new_s = new_d then k
        else
          Ass.Move (op, new_s, new_d)::k
    | _ -> instr::k


let assign_registers map code =
  let code = List.fold_right  (assign_registers_instr map) code [] in
  code

let color_code ig f =
  fix_size f.frame f.remove_ifzero
    (assign_registers (register_map ig (list colored)) f.code)

(*****************************)
(* Allocateur proprement dit *)
(*****************************)

let warn_ok ig = 
  if !verbose then begin
    prerr_string "Alloc is over" ;
    prerr_string " (" ;
    prerr_elts ig  !potential ;
    prerr_endline ")" ;
    potential := []
  end 

and warn_spill ig f spills =
  if !verbose then begin
    prerr_string
      ("Real spill "^Gen.label_string (Frame.frame_name f)^": ") ;
    prerr_elts ig spills ;
    prerr_string " (" ;
    prerr_elts ig !potential ;
    prerr_endline ")" ;
    potential := []
  end

and warn_enter f n =
  if !verbose then begin
    prerr_string ("Alloc for "^Gen.label_string (Frame.frame_name f)) ;
    prerr_string (" with "^string_of_int ncolors^" colors, ")  ;
    prerr_endline ("attempt: "^string_of_int n)
  end


let rec alloc n ({frame=frame ; code=code} as f) =
  warn_enter frame n ;
  let ig = build code in
  let spills = assign_colors ig (make_stack ig) in
  match spills with
  | [] -> (* Réussi *)
      warn_ok ig ;
      (* Toilettage et coloriage *)
      color_code ig f
  | _ -> (* Raté *)
      warn_spill ig frame spills ;
      (* Vérifier les spills *)
      List.iter
        (fun elt ->
          if is_ephemere ig elt then
            failwith
              "Ca peut boucler dans Alloc, heuristique ou nombre de registres insuffisant")
        spills ;
      (* Spiller les ``real spills'' *)
      let spill_set =
        of_list (List.map (fun elt -> (elt_to_info ig elt).temp) spills) in
      let new_f = Spill.spill_fun spill_set f in
      alloc  (n+1) new_f

let program p  =
   p.prelude::
  (let funs = List.map (alloc  0) p.procedures in
  let main = alloc 0 p.main in
  List.concat (funs @ [main]))

  
