open Pervasives
open Def
open Print
open Tools
(* Partie algorithme optimale (par brute-force de la solution) *)

let incrt t tt max= (*incrémente pour l'ordre lexicographique un tableau d'entiers donc chacun est borné par une valeur du tableau max. On ne touche pas au dernier élement de ce tableau *)
  let i = ref (tt-2) in
  while (!i>=0 && t.(!i) = max.(!i)) do
    t.(!i) <- 0;
    decr i
  done;
  if (!i = -1) then (t.(0) <- -1) else (t.(!i) <- t.(!i) + 1);
  let sum = ref 0 in
  for i = 0 to (tt-2) do
    sum := !sum + (t.(i))
  done;
  !sum



let maxi a b = if a < b then b else a


let suivant n t max= (* calcul la prochaine répartition à considérer *)
  let taillet = Array.length t in
  if (t = (Array.make taillet 0) && n <= max.(taillet -1)) then
    t.(taillet-1) <- n
  else
    begin
      if ((taillet = 1 && t.(0) = n)) then
	t.(0) <- -1
      else
	begin
	  let sommedeb = ref (incrt t taillet max) in
	  while ((!sommedeb > n || n - !sommedeb > max.(taillet-1)) && !sommedeb <> (-1)) do
	    sommedeb := incrt t taillet max
	  done;
	  if ((!sommedeb) = (-1)) then
	    t.(0) <- -1
	  else
	    t.(taillet-1) <- n -  !sommedeb
	end  
    end



let taillela l = (* range les tailles des listes de l dans un tableau *)
  let rep = Array.make (List.length l) 0 in
  let rec aux li i= match li with
    |p::q -> 
      rep.(i) <- taille_tarbre p;
      aux q (i+1)
    |[] -> ()
  in
  aux l 0;
  rep


let rec ajout elem li = match li with (* ajoute elem à toutes les liste de li*)
  |p::q -> (elem::p) :: (ajout elem q)
  |[] -> []


let l_to_a (l : 'a list) = (* list to array *)
  match l with
    |p::q ->
      begin
	let n = List.length l in
	let rep = Array.make n (p) in
	let rec aux li i = match li with
	  |t::qu -> rep.(i) <- t; aux qu (i+1)
	  |[] -> ()
	in
	aux l 0;rep
      end
    |[] -> [||]


let next_mp tc tm n= (*incrémente pour l'ordre lexicographique un tableau d'entiers donc chacun est borné par une valeur du tableau max*)
  let i = ref (n-1) in
  while (!i >= 0 && tc.(!i) = (tm.(!i)-1)) do
    tc.(!i) <- 0;
    decr i
  done;
  (*print_int (!i);*)
  if ((!i) = -1) then
    tc.(0) <- (-1)
  else
    tc.(!i) <- tc.(!i) +1



let mul_produit t ladresse= (* calcul l'ensemble des répartitions possibles comme étant l'ensemble des produits possibles de toutes les répartitions des sous listes*) 
  let n = Array.length t in
  let taux = Array.make n [||] in
  for i = 0 to (n-1) do
      taux.(i) <- l_to_a t.(i)
  done;
  let tmax = Array.make n 0 in
  for i = 0 to (n-1) do
    tmax.(i) <- maxi 1 (Array.length (taux.(i)))
  done;
  let tcompt = Array.make n 0 in
  let rep = ref [] in
  let stop = ref false in
  while (not (!stop)) do
    let laux = ref [ladresse] in
    for i = 0 to (n-1) do
      if (tmax.(i) > 0) then laux := taux.(i).(tcompt.(i)) @ !laux
    done;
    rep := !laux :: (!rep);
    next_mp tcompt tmax n;
    if (tcompt.(0) = (-1)) then (stop := true)
  done;
  !rep


let mul_produit_a t = (* calcul l'ensemble des répartitions possibles comme étant l'ensemble des produits possibles de toutes les répartitions des sous listes*) 
  let n = Array.length t in
  let taux = Array.make n [||] in
  for i = 0 to (n-1) do
      taux.(i) <- l_to_a t.(i)
  done;
  let tmax = Array.make n 0 in
  for i = 0 to (n-1) do
    tmax.(i) <- maxi 1 (Array.length (taux.(i)))
  done;
  let tcompt = Array.make n 0 in
  let rep = ref [] in
  let stop = ref false in
  while (not (!stop)) do
    let laux = ref [] in
    for i = 0 to (n-1) do
      if (tmax.(i) > 0) then laux := taux.(i).(tcompt.(i)) @ !laux
    done;
    rep := !laux :: (!rep);
    next_mp tcompt tmax n;
    if (tcompt.(0) = (-1)) then (stop := true)
  done;
  !rep

let rec sup_doub l t= match l with (*supprime les doublons*)
  |[] -> []
  |p::q -> 
    if (p = []) then 
      if t then (sup_doub q t) 
      else (p :: (sup_doub q true))
    else
      p::(sup_doub q t)


let rec sd l = match l with
  |[] -> []
  |p::q -> (sup_doub p false ) :: (sd q)


let reordre1 l = match l with (*réordonne les liste de l*)
  |p::q -> (List.rev p) :: q
  |[] -> []


let reordre l = match l with
  |p::q -> (reordre1 p) :: q
  |[] -> []


let rec genere_placement t n ladresse = (* génère toutes les répartitions possibles de n serveurs dans l'arbre t en les mettant en forme de tête *)
  (*print_string "genere placement appelé avec : \n"; print_arbre t;print_newline (); print_int n ; print_newline () ; print_l ladresse; print_newline ();*)
  let Node(fl,ent) = t in
  let rep = ref [] in
  if (n = 1) then (rep := [[ladresse]])
  else
    begin
	match fl with
	  |[] -> rep := [[ladresse]]
	  |p::[] -> 
	    begin
	      let liste = (genere_placement p (n-1) (0::ladresse)) in
	      rep := ajout ladresse liste
	    end
	  |p::q ->
	    begin
	      let tf = List.length fl in
	      let tt = taillela fl in
	      let repart = Array.make tf 0 in
	      suivant (n-1) repart tt;
	
	      while (repart.(0) <> -1) do
		(*print_string "la répartition est : "; print_array repart;*)
		let tab_plac = Array.make tf ([[[]]]) in
		for i = 0 to (tf-1) do
		  if repart.(i) <> 0 then
		    tab_plac.(i) <- (genere_placement (List.nth fl i) (repart.(i)) (i::ladresse))
        done;
		rep := (mul_produit tab_plac ladresse) @ (!rep);
 		suivant (n-1) repart tt;
	      done;
	    end;
       end;
((sd !rep))

let rec get_subtree t adr = match adr with
  |p::q -> 
    begin
      match t with
	|Server(fl,_,_) -> get_subtree (List.nth fl p) q
	|Node(fl,_) ->   get_subtree (List.nth fl p) q
    end
  |[] -> t


let rec genere_placement_a t n l =
  let rep = ref [] in
  let tf = List.length l in
  let tt = Array.make tf 0 in
  let tli = Array.make tf [] in
  let rec remplit li i= match li with
    |p::q -> tt.(i) <- taille_subtree t p ;tli.(i) <- p; remplit q (i+1)
    |[] -> () 
  in
  remplit l 0;
  let repart = Array.make tf 0 in
  suivant n repart tt;
  while (repart.(0) <> -1) do
    let tab_plac = Array.make tf ([[[]]]) in
    for i = 0 to (tf-1) do
      if repart.(i) <> 0 then
	tab_plac.(i) <- (genere_placement (get_subtree t (tli.(i))) (repart.(i)) (List.rev (tli.(i))))
    done;
    rep := (mul_produit_a tab_plac) @ (!rep);
    suivant (n) repart tt;
  done;
  (sd !rep)


let rec no_son_server t = match t with
  |Server(fl,_,_) -> apply_nss fl
  |Node(fl,_) -> apply_nss fl
and apply_nss l = match l with (*Vérifie qu'aucun des fils d'un noeud (ou serveur) donné n'est un serveur.*)
  |Server(_,_,_)::q -> false
  |Node(_,_)::q -> apply_nss q
  |[] -> true


let rec tot tree = match tree with
  |Node(fl,ent) -> ent +. apply_tot fl
  |Server(fl,n,ent) -> ent +. apply_tot fl
and apply_tot fl = match fl with
  |p::q -> tot p +. apply_tot q
  |[] -> 0.


let maxminl l =
  let repmax = ref min_float in
  let repmin = ref max_float in
  let rec aux l = match l with
    |p::q -> if p > !repmax then repmax := p ;  if p < !repmin then repmin := p ; aux q
    |[] -> ()
  in
  aux l;
  !repmax,!repmin


let enter_value fl = 
  let reps = ref [] in
  let repn = ref [] in
  let rec entry_value fl = match fl with
    |Server(l,n,ent)::q -> if (no_son_server (Server(l,n,ent))) then ((reps := (tot (Server(l,n,ent))) :: !reps); entry_value q) else (entry_value q)
    |Node(l,ent)::q -> repn := (tot (Node(l,ent))) :: (!repn) ; entry_value q
    |[] -> ()
  in
  entry_value fl;
  let a,_ = maxminl (!repn) in
  let _,b = maxminl (!reps) in
  a<=b


let rec in_normal_forme t = match t with
  |Server(fl,n,ent) -> enter_value fl && apply_inf fl
  |Node(fl,ent) -> true
and apply_inf fl = match fl with
  |p::q -> in_normal_forme p && apply_inf q
  |[] -> true

    
let colle l = 
  let max = ref (0) in
  let rec tmax l = match l with
    |p::q ->
      let a = Array.length p in
      if a > (!max) then max := a;
      tmax q
    |[] -> ()
  in
  tmax l;
  let rep = Array.make (!max+1) 0 in
  let rec add l = match l with
    |p::q ->
      let a = Array.length p in
      for i = 1 to a do
	rep.(i) <- p.(i-1) + rep.(i)
      done;
      add q
    |[] -> ()
  in
  add l;
  rep.(0) <- 1 ;
  rep


let rec ltf t = match t with
  |Server(fl,n,ent) -> colle (apply_ltf fl)
  |Node(_,_) -> [||]
and apply_ltf l = match l with
  |p::q ->
    (ltf p) :: (apply_ltf q)
  |[] -> []


let ppetit a b =
  let n = Array.length a in
  let m = Array.length b in
  if (n=m) then
    begin
      let i = ref (n-1) in
      while (!i >= 0 && a.(!i) = b.(!i)) do
	decr i
      done;
      if (!i = -1) then false
      else 
	begin
	  a.(!i) < b.(!i)
	end
    end
  else (n < m)


let egale a b =
  let n = Array.length a in
  let m = Array.length b in
  if (n<>m) then false else
    begin
      let i = ref (n-1) in
      while (!i >= 0 && a.(!i) = b.(!i)) do
	decr i
      done;
      (!i = -1)
    end


let rec nb_server t = match t with
  |Server(fl,_,_) -> apply_nb_server fl + 1
  |Node(_,_) -> 0
and apply_nb_server fl =  match fl with
  |p::q -> nb_server p + apply_nb_server q
  |[] -> 0

let rec nb_server_son fl = match fl with
  |p::q -> (nb_server p)::(nb_server_son q)
  |[] -> []


let rec cp l1 l2 = match l1,l2 with
  |p1::q1,p2::q2 -> if (p2 > p1) then (false,false) else (if p1 > p2 then (true,false) else cp q1 q2)
  |[],[] -> (false,true)
  |_ -> failwith "not the same trees"


let rec ppetit_2 a b =
  let a1,b1 = 
	match a,b with
	  |Node(fl,_),Node(fl2,_) -> (fl,fl2)
	  |Server(fl,a,ent),Server(fl2,_,_) -> (fl,fl2)
	  |Server(fl,a,ent),Node(fl2,_) ->  (fl,fl2)
	  |Node(fl2,_),Server(fl,a,ent) -> (fl2,fl)
  in
  let la = nb_server_son a1 in
  let lb = nb_server_son b1 in
  let b1,b2 = cp la lb in
  if (not b2) then (b1,b2)
  else (if la = [] then (false,true)
    else
      begin
	match a,b with
	  |Node(fl,_),Node(fl2,_) -> (false,true)
	  |Server(fl,a,ent),Server(fl2,_,_) -> apply_ppetit_2 fl fl2
	  |Server(fl,a,ent),Node(fl2,_) -> apply_ppetit_2 fl fl2
	  |Node(fl2,_),Server(fl,a,ent) -> apply_ppetit_2 fl2 fl
      end
  )
and apply_ppetit_2 fl1 fl2 = match fl1,fl2 with
  |p1::q1,p2::q2 -> 
	      let b1,b2 = ppetit_2 p1 p2 in
	      if (b2) then apply_ppetit_2 q1 q2
	      else b1,b2
  |[],[] -> (false,true)
  |_ -> failwith "pas meme arbre"



let rec recup_contient l l1 = match l with
  |[] -> []
  |p::q -> if inclus l1 p then (p :: (recup_contient q l1)) else (recup_contient q l1)


let liste_son adr n =
  let rep = ref [] in
  for i = 0 to (n-1) do
    rep := (List.rev (i :: (List.rev adr))):: !rep
  done;
  !rep

let rec son_list t adr adrinit = match adr with
  |[] -> 
    begin
      match t with
	|Node(fl,_) -> liste_son adrinit (List.length fl)
	|Server(fl,_,_) -> liste_son adrinit (List.length fl)
    end
  |p::q -> 
    begin
      match t with
	|Node(fl,_) -> son_list (List.nth fl p) q adrinit
	|Server(fl,_,_) -> son_list (List.nth fl p) q adrinit
    end


let rec mega_son_list t l = match l with
  |p::q -> (son_list t p p) @ mega_son_list t q
  |[] -> []


let rec map_rev l = match l with
  |p::q -> (List.rev p)::(map_rev q)
  |[] -> []


let rec ajoute_partout a l = match l with
  |p::q -> ((map_rev a)@p)::(ajoute_partout a q)
  |[] -> []


let next t t_pere p = 
  let continue = ref true in
  let n = Array.length t in
  let nb_p = ref p in
  let valid = ref 1 in
  while !continue do
    let cont = ref true in
    let ret = ref 1 in
    let i = ref 1 in
    while (!i < n && !ret = 1) do
      begin
	if t.(!i) = 1 then (
	  t.(!i) <- 0 ; nb_p := !nb_p - 1;incr i)
	else (
	  t.(!i) <- 1;
	  ret := 0;
	  nb_p := !nb_p +1
	);
      end
    done;
    let test = (!nb_p = p) in
    if (test) then 
      (
	let j = ref 1 in
	while (!j < n && !cont) do
	  cont := (t.(!j) = 0 || t.(t_pere.(!j)) = 1);
	  incr j
	done;
      )
    else
      (cont := false;);
    continue := not (!cont);
    if (!i = n) then (valid := -1 ; continue := false);
  done;
  t.(0) <- !valid


let n_tree_to_l n t n_to_a =
  
  let rep = ref [] in
  for i = 0 to (n-1) do
    if t.(i) = 1 then 
      (
	rep := (n_to_a.(i)) :: !(rep)
      )
  done;
  !rep


let n_brute_force t p =
  let n = taille_tree t in
  let n_to_ad = Array.make n [] in
  let t_pere = Array.make n (-1) in
  let compt = ref 0 in
  let repart = Array.make n 0 in
  let rec remplis tr ad pere= match tr with
    |Server(_,_,_) -> failwith "not the good type"
    |Node(fl,ent) -> n_to_ad.(!compt) <- ad ; t_pere.(!compt) <- pere ;incr (compt); apply_remplis fl ad 0 (!compt-1)
  and apply_remplis fl ad i pere = match fl with
    |p::q -> remplis p (i::ad) pere; apply_remplis q ad (i+1) pere
    |[] -> ()
  in
  remplis t [] (-1);
  for j = 0 to (p-1) do
    repart.(j) <- 1
  done;
  let min = ref max_float in
  let argmin = ref (Node([],0.)) in
  while (repart.(0) <> (-1)) do
    let l = n_tree_to_l n repart n_to_ad in
    let a = cree_tarbre t l [] in
    let b = tarbre_to_arbre a in
    let _ = opt b in
    let c = puiss b in
    if (c <= !min) then
      (
	min := c;
	argmin := a
      );
    next repart t_pere p;
  done;
  !min,!argmin


let brute_force t (* l'arbre *) n (*le nombre de serveurs*) = (*brute force le positionnement optimal de n serveurs dans t*)
  let reparts = genere_placement t n [] in
  let min = ref max_float in
  let argmin = ref [] in
  let rec recherche l = match l with
    |p::q ->
      begin
	let a = cree_tarbre t p [] in
	let b = tarbre_to_arbre a in
	let _ = opt b in
	let c = puiss b in
	if (c < !min) then
	  begin
	    min := c;
	    argmin := [a]
	  end
	else
	  begin
	    if c = (!min) then
	      argmin := a  :: !(argmin)
	  end;
	recherche q
      end
    |[]  -> ()
  in
  recherche reparts;
  let targmin = ref (List.hd (!argmin)) in
  let rep = ref [] in
  let rec best l = match l with
    |p::q ->
      begin
	(
	  let binf = in_normal_forme p in
	  if ppetit (ltf p) (ltf (!targmin)) && binf then (targmin := p ; rep := [!targmin])
	  else (
	    if (egale (ltf p) (ltf (!targmin))) && binf then (rep := p :: !(rep)))
	);
	best q
      end
    |[] -> ()
  in
  best (!argmin);
  let targmin = ref (List.hd (!rep)) in
  let rec best_2 l = match l with
    |p::q ->
      begin
	(
	  let b1,b2 = ppetit_2 (p) ( (!targmin)) in
	  if b2 then  (print_string "surement une erreur : ppetit_2 "; print_newline (); print_arbre p ; print_arbre (!targmin));
	  if b1 then (targmin := p);
	);
	best_2 q
      end
    |[] -> ()
  in 
  (!min,!targmin)


(*let ens_satur tr taille = (*calcul l'ensemble des sommets saturés dans tr*)*)
(*  let rec tot tree = match tree with*)
(*    |Node(fl,ent) -> ent +. apply_tot fl*)
(*    |Server(fl,n,ent) -> ent +. apply_tot fl*)
(*  and apply_tot fl = match fl with*)
(*    |p::q -> tot p +. apply_tot q*)
(*    |[] -> 0.*)
(*  in*)
(*  let t = tot tr in*)
(*  let rep = ref [] in*)
(*  let rec cre tree adr = match tree with*)
(*    |Server(fl,n,ent) -> if ((float_of_int n.w) > (float_of_int t)/(float_of_int taille) || ((float_of_int t) mod (float_of_int taille) = 0 && n.w = t/.taille)) then rep := (List.rev adr) :: !(rep);*)
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


(*let rec brute_force_a t n =*)
(*  match n with*)
(*    |1 -> (brute_force t 1,[[]])*)
(*    |_ -> *)
(*      begin*)

(*	let ((_,_),l) = brute_force_a t (n-1) in*)
(*	let ls = remove_l l (mega_son_list t l) in*)
(*	let m = n - (List.length l) in*)
(*	let repart = genere_placement_a t m ls in*)
(*	let reparts = ajoute_partout l repart in*)
(*	let min = ref max_int in*)
(*	let argmin = ref t in*)
(*	let rec recherche l = match l with*)
(*	  |p::q ->*)
(*	    begin*)
(*	      let a = cree_tarbre t p [] in*)
(*	      let b = tarbre_to_arbre a in*)
(*	      let _ = opt b in*)
(*	      let c = puiss b in*)
(*	      if (c < !min) then*)
(*		begin*)
(*		  min := c;*)
(*		  argmin := a*)
(*		end*)
(*	      else*)
(*		begin*)
(*		  if c = (!min) then*)
(*		    argmin := a*)
(*		end;*)
(*	      recherche q*)
(*	    end*)
(*	  |[]  -> ()*)
(*	in*)
(*	recherche reparts;*)
(*	let e = ens_satur (!argmin) n in*)
(*	let lfin = ajoutel e l in*)
(*	((!min,!argmin),lfin)*)
(*      end*)

let rec verif_l l = match l with
  |p::q ->
    let b = tarbre_to_arbre p in
    let _ = opt b in
    let c = puiss b in
    print_float c ; print_newline ();
    verif_l q
  |[] -> ()

(* \ Partie algorithme optimale (par brute-force de la solution) *)
