open Def
open Printf

let rec print_l l= (*prints an integer list (often the address of a node)*)
  match l with 
    |[] -> ()
    |p::q -> print_int p ; print_string " "; print_l q

let print_tab i =
  for j = 0 to (i-1) do
    Printf.fprintf stdout "\t"
  done

let rec print_arbre_v t =  (* Prints a tree so that it is recognized by Caml*)
 match t with
  |Node(fl,ent) ->
    print_string "Node([";
    let rec apply l = match l with
      |p::q -> print_arbre_v p; printf ";"; apply q
      |[] -> printf "],"
    in
    apply fl ; printf "%f)" ent;
  |Server(fl,w,ent) ->
    print_string "S";print_int (List.length fl);
    let rec apply l = match l with
      |p::q -> print_arbre_v p; print_string ","; apply q
      |[] -> ()
    in
    print_string "("; apply fl ; print_string ")"


let print_ar ar = 
  let n = Array.length ar in
  for i = 0 to (n-1) do
    printf "%d %d " i (fst ar.(i)); print_arbre_v (snd ar.(i)); printf "\n";
  done

(*let ens_satur tr taille = (*calcul l'ensemble des sommets saturés dans tr*)*)
(*  let rec tot tree = match tree with*)
(*    |Node(fl,ent) -> ent + apply_tot fl*)
(*    |Server(fl,n,ent) -> ent + apply_tot fl*)
(*  and apply_tot fl = match fl with*)
(*    |p::q -> tot p + apply_tot q*)
(*    |[] -> 0*)
(*  in*)
(*  let t = tot tr in*)
(*  let rep = ref [] in*)
(*  let rec cre tree adr = match tree with*)
(*    |Server(fl,n,ent) -> if (n.w > t/(taille) || (t mod taille = 0 && n.w = t/taille)) then rep := (List.rev adr) :: !(rep);*)
(*      apply_cre fl adr 0*)
(*    |Node(fl,ent) -> ()*)
(*  and apply_cre fl adr i = match fl with*)
(*    |p::q -> *)
(*      cre p (i::adr);*)
(*      apply_cre q adr (i+1)*)
(*    |[] -> ()*)
(*  in*)
(*  cre tr [];*)
(*!rep*)

(*let ens_pres tr =  (*calcul l'ensemble des sommets présents dans tr*)*)
(*  let rep = ref [] in*)
(*  let rec prs tree adr= match tree with*)
(*    |Server(fl,n,ent) ->*)
(*      rep := (List.rev adr) :: (!rep); apply_prs fl adr 0*)
(*    |Node(fl,ent) -> *)
(*      ()*)
(*  and apply_prs fl adr i = match fl with*)
(*    |p::q -> *)
(*      prs (p) (i::adr); apply_prs q adr (i+1)*)
(*    |[] -> ()*)
(*  in*)
(*  prs tr [];*)
(*  !rep*)

let rec print_arbre t = match t with (* prints an arbre*)
  |Node(fl,ent) ->
    print_string "N";print_int (List.length fl);
    let rec apply l = match l with
      |p::q -> print_arbre p; print_string ","; apply q
      |[] -> ()
    in
    print_string "("; apply fl ; print_string ")"
  |Server(fl,w,ent) ->
    printf "S%d_%f" (List.length fl) w.w; 
    let rec apply l = match l with
      |p::q -> print_arbre p; print_string ","; apply q
      |[] -> ()
    in
    print_string "("; apply fl ; print_string ")"


let print_arbre_latex t =
  Printf.fprintf stdout "\\paragraph{}\n\\begin{tikzpicture}[{level 1/.style={sibling distance=4cm},level 2/.style={sibling distance=1.5cm},level 3/.style={sibling distance=0.5cm},level 4/.style={sibling distance=3cm},level 5/.style={sibling distance=3cm},level 6/.style={sibling distance=3cm},level distance = 1.5cm}]\n";
  let rec pal tr b i= match tr with
    |Server(fl,n,ent) -> 
      begin
	if b then
	  (
	    fprintf stdout ("\\node [arn_r] {%f} child {node [arn_x] {%f}}\n") n.w ent;
	    apply_pal fl (i+1);
	  )
	else
	  (
	    print_tab i;
	    fprintf stdout ("child{node [arn_r] {%f} child {node [arn_x] {%f}}\n") n.w ent;
	    apply_pal fl (i+1);
	    print_tab i;
	    fprintf stdout "}\n"
	  )
      end
    |Node(fl,ent) -> 
      begin
	if b then
	  (
	    fprintf stdout ("\\node [arn_n] {} child {node [arn_x] {%f}}\n") ent;
	    apply_pal fl (i+1)
	  )
	else
	  (
	    print_tab i;
	    fprintf stdout ("child{node [arn_n] {} child {node [arn_x] {%f}}\n") ent;
	    apply_pal fl (i+1);
	    print_tab i;
	    fprintf stdout ("}\n")
	  )
      end
  and apply_pal l i= match l with
    |p::q -> pal p false i; apply_pal q i
    |[] -> ()
  in
  pal t true 0;
  Printf.fprintf stdout ";\n\\end{tikzpicture}\n"



