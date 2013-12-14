type 'a t = { all : 'a elem list array; id : int }
and 'a elem = { info : 'a; mutable parent : 'a t }
let make n =
  let all = Array.create n [] in
  Array.mapi (fun i _ -> { all = all; id = i }) all

let put s e =
  let n = { info = e; parent = s } in
  s.all.(s.id) <- n :: s.all.(s.id) ;
  n

let eqsets s1 s2 = (s1.all == s2.all && s1.id = s2.id)
let eq u v = (u == v)
let parent e = e.parent
let info e = e.info
let clear ss = Array.iter (fun s -> s.all.(s.id) <- []) ss
    
let belong e s = eqsets e.parent s

let move e s =
  if eqsets e.parent s then ()
  else (e.parent <- s; s.all.(s.id) <- e::s.all.(s.id))

let pick s =
  let rec next = function
    | h::t as l ->
        if eqsets h.parent s
        then (s.all.(s.id) <- l; Some h)
        else next t
    | [] -> None
  in next s.all.(s.id)
;;

let empty s = try let _ = pick s in false with Not_found -> true

let list s =
  let rec next = function
    | h::t as l ->
        if eqsets h.parent s
        then h :: next t
        else next t
    | [] -> [] in
  let l = next s.all.(s.id) in
  s.all.(s.id) <- l;
  l
;;

let pick_lowest cost s = match pick s with
| None -> None
| Some x ->
    let m = ref x in
    let rec find_and_clean q = function
      | h::t ->
          if eqsets h.parent s
          then
            let q' = cost h in
            let q'' = if q' <= q then (m := h; q') else q in
            h :: find_and_clean q'' t
          else
            find_and_clean q t
      | [] -> [] in
    let l = find_and_clean (cost !m) s.all.(s.id) in
    s.all.(s.id) <- l;
  Some !m


let sort f s = s.all.(s.id) <- Sort.list f (list s)


