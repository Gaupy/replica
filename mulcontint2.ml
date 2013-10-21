Random.self_init ();;
(* Définition des types *)
type noeud = { n : int ; mutable w : int };;

type arbre = 
  |Noeud of ((arbre list) * noeud * (int));;

type tarbre =
  |Server of ((tarbre list) * noeud * int)
  |Node of ((tarbre list) * int);;

(* Boîte à outils *)
let decoupe_string s = 
  let rep = ref [] in
  let beg = ref 0 in
  let n = String.length s in
  let cur_prof = ref 0 in
  for i = 0 to (n-1) do
    match s.[i] with  
      |'('-> decr cur_prof
      |')'-> incr cur_prof
      |';'-> if !cur_prof = 0 then 
	  begin
	    rep := (String.sub s (!beg) (i- !beg)) :: !rep;
	    beg := i+1;
	  end
      |_-> ()
  done;
  !rep;;
let decoupe_string_2 s = 
  let rep = ref [] in
  let beg = ref 0 in
  let n = String.length s in
  let cur_prof = ref 0 in
  for i = 0 to (n-1) do
    match s.[i] with  
      |'('-> decr cur_prof
      |')'-> incr cur_prof
      |','-> if !cur_prof = 0 then 
	  begin
	    rep := (String.sub s (!beg) (i- !beg)) :: !rep;
	    beg := i+1;
	  end
      |_-> ()
  done;
  rep := (String.sub s (!beg) (n- !beg)) :: !rep;
  !rep;;
let rec string_to_tarbre s =
  let n = String.length s in
  let l = List.rev (decoupe_string_2 (String.sub s 5 (n-6))) in
  let fl = List.hd (l) in
  let ent = int_of_string (List.hd (List.tl l)) in
  Node(apply_stt (decoupe_string (String.sub fl 1 ((String.length fl) - 2))),ent)

and apply_stt l = match l with
  |p::q -> string_to_tarbre p :: (apply_stt q)
  |[] -> [];;

let rec insert (c,n) l = match l with
  |(a,b)::q -> 
    if (a > c) then 
      ((a,b)::(insert (c,n) q))
    else
      ((c,n)::(a,b)::q)
  |[] -> [(c,n)]
;;

let print_tab i =
  for j = 0 to (i-1) do
    Printf.fprintf stdout "\t"
  done;;
let lpop l = 
  l := List.tl !l;;

let rec minsert l1 l2 = match l1 with
  |[] -> l2
  |p::q -> insert p (minsert q l2);;

let minf a b = if a < b then a else b;;

let rec app f l = match l with
  |[] -> []
  |p::q -> minsert (f p) (app f q);;

let rec get n l = match n with
  |0 -> List.hd l
  |_ -> 
    begin 
      match l with
	|p::q -> get (n-1) q
	|[] -> failwith "liste_vide"
    end
;;
let coupel l p = (*découpe la liste l suivant le p-ieme élément*)
  let debut = ref [] in
(*  print_int p;print_string " ";
  print_int (List.length l);print_newline ();*)
  
  let rec cut li p = match p with
    |0 -> (List.rev !debut,List.hd li, List.tl li)
    |_ -> match li with
	|t::q ->
	  begin
	    debut := t:: !debut;
	    cut q (p-1)
	  end
	|[] -> failwith "lv"
  in
  cut l p;;
let coupel2 l p =(*découpe la liste l suivant le p-ieme server*)
  let debut = ref [] in
(*  print_int p;print_string " ";
  print_int (List.length l);print_newline ();*)
  (*print_string "coupel : "; print_int p ; print_string " "; print_int (List.length l);print_newline ();*)
  let rec cut li p = match p with
    |0 -> 
      begin
	match li with
	  |Server(a,b,c)::q -> 
	    (List.rev !debut,List.hd li, List.tl li)
	  |Node(a,b)::q ->
	    begin
	      debut := (Node(a,b)):: !debut;
	      cut q (0)
	    end
	  |[] -> failwith "lv2"
	end
    |_ -> 
      begin
	match li with
	  |Server(a,b,c)::q ->
	    begin
	      debut := (Server(a,b,c)):: !debut;
	      cut q (p-1)
	    end
	  |Node(a,b)::q ->
	    begin
	      debut := (Node(a,b)):: !debut;
	      cut q (p)
	    end
	  |[] -> failwith "lv1"
      end
  in
  cut l p;;

let rec print_arbre t = match t with (* imprime un arbre*)
  |Node(fl,ent) ->
    print_string "N";print_int (List.length fl);
    let rec apply l = match l with
      |p::q -> print_arbre p; print_string ","; apply q
      |[] -> ()
    in
    print_string "("; apply fl ; print_string ")"
  |Server(fl,w,ent) ->
    print_string "S";print_int (List.length fl);print_string "_" ; print_int w.w; 
    let rec apply l = match l with
      |p::q -> print_arbre p; print_string ","; apply q
      |[] -> ()
    in
    print_string "("; apply fl ; print_string ")";;

let rec print_arbre_v t = match t with (* imprime un arbre de sorte qu'il soit reconnu par Caml*)
  |Node(fl,ent) ->
    print_string "Node([";
    let rec apply l = match l with
      |p::q -> print_arbre_v p; print_string ";"; apply q
      |[] -> print_string "],"
    in
    apply fl ; print_int ent ; print_string ")"
  |Server(fl,w,ent) ->
    print_string "S";print_int (List.length fl);
    let rec apply l = match l with
      |p::q -> print_arbre_v p; print_string ","; apply q
      |[] -> ()
    in
    print_string "("; apply fl ; print_string ")";;

let rec print_l l= match l with (*imprime une liste d'entier (généralement une adresse)*)
  |[] -> ()
  |p::q -> print_int p ; print_string " "; print_l q
;;

let rec print_l_l l = match l with (* imprime une liste d'adresse *)
  |p::q -> print_l p; print_newline(); print_l_l q;
  |[] -> ()
;;

let rec sup elem l = match l with (* supprime toutes les occurences de elem dans l*)
  |p::q -> if p = elem then (sup elem q) else p::(sup elem q)
  |[] -> [];;

let rec get_entree t = match t with (* étant donnée un arbre de Node, renvoit la somme de toutes les entrées*)
  |Node(fl,ent) -> ent + ap_get_entree fl
  |Server(_,_,_) -> failwith "Server not at his place"

and ap_get_entree l = match l with
  |[] -> 0
  |p::q -> get_entree p + (ap_get_entree q);;

let rec tarbre_to_arbre t = match t with (* transforme un tarbre en arbre *)
  |Node(fl,ent) -> failwith "node reached"
  |Server(fl,n,ent) -> 
    let a,b = ap_ta_to_t fl in
    Noeud(a,n,ent+b)

and ap_ta_to_t l = match l with
  |[] -> [],0
  |p::q ->
    begin
      let a,b = ap_ta_to_t q in
      match p with
	|Node(fl,ent) -> (a,b+ get_entree (p))
	|Server(fl,_,ent) -> ((tarbre_to_arbre p)::a,b)
    end
;;
let rec appart elem l = match l with (* test l'appartenance d'un élement elem à une liste l *)
  |[] -> false
  |p::q -> (p = elem) || appart elem q;;

let rec cree_tarbre tarbre l adress= match tarbre with (* place les serveurs aux adresses contenues dans l*)
  |Node(fl,ent) -> 
    if appart adress l then
      Server(ap_cree_tarbre fl l adress 0,{n = 0 ; w = 0},ent)
    else
      Node(fl,ent)
      |Server(_,_,_) -> failwith "server at wrong place"
and ap_cree_tarbre li l adress i = match li with
  |[] -> []
  |p::q -> (cree_tarbre p l (i::adress)) :: (ap_cree_tarbre q l adress (i+1));;

let rec puiss arbre = match arbre with (* calcul la puissance d'un arbre*)
  |Noeud(fl,n,ent) -> 
    let a = n.w in
    a*a*a + ap_puiss fl
and ap_puiss l = match l with
  |[] -> 0
  |p::q -> puiss p + (ap_puiss q)
;;
let rec puisst arbre = match arbre with (* calcul de la puissance d'un tarbre*)
  |Server(fl,n,ent) -> 
    let a = n.w in
    a*a*a + ap_puisst fl
  |Node(fl,ent) -> 
    ap_puisst fl
and ap_puisst l = match l with
  |[] -> 0
  |p::q -> puisst p + (ap_puisst q);;

let rec taille_tree t = match t with
  |Server(fl,_,_) -> 1 + apply_tt fl
  |Node(fl,_) -> 1 + apply_tt fl
and apply_tt l = match l with
  |p::q -> taille_tree p + apply_tt q
  |[] -> 0;;

let rec taille_t_tree t = match t with
  |Noeud(fl,_,_) -> 1 + apply_t_tt fl
and apply_t_tt l = match l with
  |p::q -> taille_t_tree p + apply_t_tt q
  |[] -> 0;;

let rec taille_subtree t adr = match adr with
  |[] -> taille_tree t
  |p::q -> 
    begin
      match t with
	|Server(fl,ent,a) -> taille_subtree (get p fl) (q)
	|Node(fl,ent) -> taille_subtree (get p fl) (q)
    end;;

let rec remove elem l = match l with (*enleve elem de la liste l *)
  |p::q ->
    if p = elem then q
    else (p::(remove elem q))
  |[] -> failwith "not in the list";;
let rec remove_tt elem l = match l with (*enleve elem de la liste l *)
  |p::q ->
    if p = elem then (remove_tt elem q)
    else (p::(remove_tt elem q))
  |[] -> [];;

let rec remove_l lelem l = match lelem with
  |p::q -> 
    remove_l (q) (remove_tt p l)
  |[] -> l;;

remove_l ([1;2;3]) [4;1;5;2;6;3;54;7;2;3;6;9];;
remove_tt 3 [4;1;5;2;6;3;54;7;2;3;6;9];;

let rec prefixe l1 l2 = match l1,l2 with (*dit si l1 est préfixe de l2*)
  |p1::q1,p2::q2 -> p1 = p2 && (prefixe q1 q2)
  |p1::q1,[] -> false
  |[],p::q  -> true
  |[],[] -> false;;

let rec inclus a b = match a with (*test l'inclusion de la liste a dans la liste b*)
  |p::q -> (appart p b) && (inclus q b)
  |[] -> true;;

let rec ajoute a l = (*ajoute a à la liste s'il n'y ait pas déjà*)
  if appart a l then l else (a::l);;

let rec ajoutel l1 l2 = match l1 with (*de même avec une liste*)
  |p::q -> ajoute (p) (ajoutel q l2)
  |[] -> l2;;
let print_array_float ar = (*imprime une matrice floatante*)
  let n = Array.length ar in
  let p = Array.length (ar.(0)) in
  for i = 0 to (p-1) do
    print_int (i+1) ; print_string " ";
    for j = 0 to (n-1) do
      print_float (ar.(j).(i)) ; print_string " ";
    done;
    print_newline ()
  done;;
let print_array_fst_int ar = (*imprime les entiers des couples*)
  let n = Array.length ar in
  let p = Array.length (ar.(0)) in
  for i = 0 to (p-1) do
    print_int (i+1) ; print_string " ";
    for j = 0 to (n-1) do
      print_int (if ((fst ar.(j).(i)) = max_int) then 0 else (fst ar.(j).(i))) ; print_string " ";
    done;
    print_newline ()
  done;;

let rec not_incl l1 l2 = match l1 with (*renvoit les éléments de l1 non inclus dans l2*)
  |p::q -> if appart p l2 then not_incl q l2 else (p:: (not_incl q l2))
  |[] -> [];;

let print_ar ar = 
  let n = Array.length ar in
  for i = 0 to (n-1) do
    print_int i ; print_string " "; print_int (fst ar.(i));print_string " "; print_arbre_v (snd ar.(i)); print_newline ();
  done;;
let ens_satur tr taille = (*calcul l'ensemble des sommets saturés dans tr*)
  let rec tot tree = match tree with
    |Node(fl,ent) -> ent + apply_tot fl
    |Server(fl,n,ent) -> ent + apply_tot fl
  and apply_tot fl = match fl with
    |p::q -> tot p + apply_tot q
    |[] -> 0
  in
  let t = tot tr in
  let rep = ref [] in
  let rec cre tree adr = match tree with
    |Server(fl,n,ent) -> if (n.w > t/(taille) || (t mod taille = 0 && n.w = t/taille)) then rep := (List.rev adr) :: !(rep);
      apply_cre fl adr 0

    |Node(fl,ent) -> ()
  and apply_cre fl adr i = match fl with
    |p::q -> 
      cre p (i::adr);
      apply_cre q adr (i+1)
    |[] -> ()
  in
  cre tr [];
!rep;;
let ens_pres tr =  (*calcul l'ensemble des sommets présents dans tr*)
  let rep = ref [] in
  let rec prs tree adr= match tree with
    |Server(fl,n,ent) ->
      rep := (List.rev adr) :: (!rep); apply_prs fl adr 0
    |Node(fl,ent) -> 
      ()
  and apply_prs fl adr i = match fl with
    |p::q -> 
      prs (p) (i::adr); apply_prs q adr (i+1)
    |[] -> ()
  in
  prs tr [];
  !rep;;

let print_arbre_latex t =
  Printf.fprintf stdout "\\paragraph{}\n\\begin{tikzpicture}[{level 1/.style={sibling distance=4cm},level 2/.style={sibling distance=1.5cm},level 3/.style={sibling distance=0.5cm},level 4/.style={sibling distance=3cm},level 5/.style={sibling distance=3cm},level 6/.style={sibling distance=3cm},level distance = 1.5cm}]\n";
  let rec pal tr b i= match tr with
    |Server(fl,n,ent) -> 
      begin
	if b then
	  (
	    Printf.fprintf stdout ("\\node [arn_r] {%d} child {node [arn_x] {%d}}\n") n.w ent;
	    apply_pal fl (i+1);
	  )
	else
	  (
	    print_tab i;
	    Printf.fprintf stdout ("child{node [arn_r] {%d} child {node [arn_x] {%d}}\n") n.w ent;
	    apply_pal fl (i+1);
	    print_tab i;
	    Printf.fprintf stdout "}\n"
	  )
      end
    |Node(fl,ent) -> 
      begin
	if b then
	  (
	    Printf.fprintf stdout ("\\node [arn_n] {} child {node [arn_x] {%d}}\n") ent;
	    apply_pal fl (i+1)
	  )
	else
	  (
	    print_tab i;
	    Printf.fprintf stdout ("child{node [arn_n] {} child {node [arn_x] {%d}}\n") ent;
	    apply_pal fl (i+1);
	    print_tab i;
	    Printf.fprintf stdout ("}\n")
	  )
      end
  and apply_pal l i= match l with
    |p::q -> pal p false i; apply_pal q i
    |[] -> ()
  in
  pal t true 0;
  Printf.fprintf stdout ";\n\\end{tikzpicture}\n";;
(* \ Boite à outils *)

(* Partie sur la fonction qui calcul les charges optimales à serveurs placés  *opt* *) 


let max l = (* renvoit la liste des plus grands éléments de la liste ainsi que la valeur de ces plus grands éléments *) 
  let aux = ref 0 in
  if !l = [] then (failwith "") else (aux := fst (List.hd !l));
  let rep = ref [] in
  let rec maxs li = match li with
    |(a,b)::q -> 
      if (a= !aux) then (rep := (a,b) :: !(rep) ; lpop l ; maxs q)
    |[] -> ()
  in
  maxs !l;
  rep,aux;;


let rec addk k l = match l with (* enleve 1 aux k premiers éléments de la liste*)
  |[] -> []
  |(a,b)::q ->
    if (k = 0) then (a,b)::q else ( 
      b.w <- b.w - 1;
      (a-1,b)::(addk (k-1) q)
    )
;;

let rec opt tr = match tr with (* fonction qui calcule la répartition optimale des charges *)
  |Noeud([],n,entree) -> n.w <- entree ; insert (entree,n) []
  |Noeud(l,n,entree) ->
    n.w <- entree ;
    let long = app opt l in
    let lref = ref long in
    let lmax,maxl = max lref in

    let deux = 
       ref (if (!lref) <> [] then (fst (List.hd (!lref))) else (-1))
    in
    (*print_newline ();*)
    while (!maxl > n.w) do
      let tl = List.length (!lmax) in
      (*print_int tl ; print_newline();*)
      let diff = minf (float_of_int (!maxl - !deux)) ((float_of_int (!maxl - n.w)) /. ((float_of_int tl) +. 1.)) in
      if (n.n = 3) then
	begin
	  print_float diff;print_newline ();
	end;
      (*print_float diff;*)
      (*print_newline ();*)
      if (diff >= 1.) then
	begin
	  n.w <- n.w + (int_of_float diff) * tl;
	  let rec add l diff = match l with
	    |(a,b)::q ->
	      b.w <- b.w - diff;
	      (a - diff , b) :: (add q diff)
	    |[] -> []
	  in
	  let l2 = add (!lmax) (int_of_float diff) in
	  lref := minsert (l2) (!lref);
	end
      else
	begin
	  let k = !maxl - n.w in
	  n.w <- n.w + k;
	  let l2 = addk k (!lmax) in
	  lref := minsert (l2) (!lref);
	end;
      let lmaxb,maxlb = max lref in
      lmax := !lmaxb;
      maxl := !maxlb;
      deux := if (!lref) <> [] then (fst (List.hd (!lref))) else (-1);
    done;
    lref := minsert (!lmax) (!lref);
    insert (n.w,n) (!lref)
;;
let rec opt_k tr k= (* fonction qui calcule la répartition optimale des charges en en laissant sortir k*)
    let lref = ref (opt tr) in
    let lmax,maxl = max lref in
    let deux = 
       ref (if (!lref) <> [] then (fst (List.hd (!lref))) else (-1))
    in
    let reste = ref k in
    (*print_newline ();*)
    while (!reste > 0) do
      let tl = List.length (!lmax) in
      (*print_int tl ; print_newline();*)
      let diff = minf (float_of_int (!maxl - !deux)) ((float_of_int (!reste)) /. ((float_of_int tl))) in
      (*print_float diff;*)
      (*print_newline ();*)
      if (diff >= 1.) then
	begin
	  reste := !reste - (int_of_float diff) * tl;
	  let rec add l diff = match l with
	    |(a,b)::q ->
	      b.w <- b.w - diff;
	      (a - diff , b) :: (add q diff)
	    |[] -> []
	  in
	  let l2 = add (!lmax) (int_of_float diff) in
	  lref := minsert (l2) (!lref);
	end
      else
	begin
	  let k = !reste in
	  reste := !reste - k;
	  let l2 = addk k (!lmax) in
	  lref := minsert (l2) (!lref);
	end;
      let lmaxb,maxlb = max lref in
      lmax := !lmaxb;
      maxl := !maxlb;
      deux := if (!lref) <> [] then (fst (List.hd (!lref))) else (-1);
    done;
    lref := minsert (!lmax) (!lref)
;;
(* \ Partie sur la fonction qui calcul les charges optimales à serveurs placés  *opt* *) 

(* Générateur aléatoire d'arbre et d'arbre binaire *)

let rec addn l t vmax = (* ajoute un noeud dans l'arbre (t) comme fils du noeud dont l'adresse est celle passée en argument (l) dont l'entrée est inférieure à vmax *)
  (*print_string "addn : ";print_l l;print_newline ();*)
  match l with
  |[] ->
    begin
      let Node(fl,ent) = t in
      let tfl = List.length fl in
      (*(print_string "ajouter en : ";print_int tfl; print_newline() ;*)
      (Node(fl@([Node([],Random.int vmax)]),ent),tfl)
    end
  |p::q ->
    begin
      let Node(fl,ent) = t in
      let (de,mi,fi) = coupel fl p in
      let a,b = addn q mi vmax in
      (Node(de@([a])@fi,ent),b)
    end
;;

let cree_alea n valmax = (* crée un arbre aléatoire ayant n noeuds et des entrées bornées par valmax*)
  let valpossible = ref [[]] in
  let i = ref 0 in
  let arbre_init = ref (Node([],Random.int valmax)) in
  let add_node () =
    let taille = List.length (!valpossible) in
    let next = Random.int taille in
    let v = get next (!valpossible) in
    (*print_string "v:";print_int (List.length v);
    print_newline ();*)
    let a,b = addn v (!arbre_init) valmax in
    arbre_init := a;
    valpossible :=  !valpossible @ [List.rev (b :: List.rev (v))];
  in
  for k = 0 to (n-2) do
    add_node ();
    incr i;
    (*print_l_l (!valpossible);*)
    (*print_string "New tree : "; print_arbre (!arbre_init) ; print_newline ()*)
  done;
  !arbre_init;;
let rec sup elem l = match l with
  |p::q -> if p = elem then (sup elem q) else p::(sup elem q)
  |[] -> [];;
(*let tex = cree_alea 6 10;;*)
(*let _,texc = algo_intelligent tex 3;;*)
(*brute_force tex 3;;*)
(*print_arbre_latex texc;;*)
let cree_alea_binaire n valmax = (* crée un arbre binaire aléatoire ayant n noeuds et des entrées bornées par valmax*) 
  let valpossible = ref [[]] in
  let i = ref 0 in
  let arbre_init = ref (Node([],Random.int valmax)) in
  let add_node () =
    (*print_l_l (!valpossible);*)
    let taille = List.length (!valpossible) in
    let next = Random.int taille in
    let v = get next (!valpossible) in
    (*print_string "v:";print_int (List.length v);
    print_newline ();*)
    let a,b = addn v (!arbre_init) valmax in
    if b = 1 then 
      valpossible := sup v !(valpossible);
    arbre_init := a;
    valpossible :=  !valpossible @ [List.rev (b :: List.rev (v))];
  in
  for k = 0 to (n-2) do
    add_node ();
    incr i;
    (*print_l_l (!valpossible);*)
    (*print_string "New tree : "; print_arbre (!arbre_init) ; print_newline ()*)
  done;
  !arbre_init;;

let rec taille_tarbre t = match t with
  |Node(fl,ent) -> 1 + ap fl
  |Server(fl,_,_) -> 1 + ap fl
and  ap l = match l with
  |[] -> 0
  |p::q -> taille_tarbre p + ap q
;;

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
;;

let maxi a b = if a < b then b else a;;

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
;;

let taillela l = (* range les tailles des listes de l dans un tableau *)
  let rep = Array.make (List.length l) 0 in
  let rec aux li i= match li with
    |p::q -> 
      rep.(i) <- taille_tarbre p;
      aux q (i+1)
    |[] -> ()
  in
  aux l 0;
  rep;;

let rec ajout elem li = match li with (* ajoute elem à toutes les liste de li*)
  |p::q -> (elem::p) :: (ajout elem q)
  |[] -> [];;

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
    |[] -> [||];;

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
    tc.(!i) <- tc.(!i) +1;
;;

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
  !rep;;
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
  !rep;;

let rec sup_doub l t= match l with (*supprime les doublons*)
  |[] -> []
  |p::q -> 
    if (p = []) then 
      if t then (sup_doub q t) 
      else (p :: (sup_doub q true))
    else
      p::(sup_doub q t)
;;
let rec sd l = match l with
  |[] -> []
  |p::q -> (sup_doub p false ) :: (sd q);;

let reordre1 l = match l with (*réordonne les liste de l*)
  |p::q -> (List.rev p) :: q
  |[] -> []
;;
let reordre l = match l with
  |p::q -> (reordre1 p) :: q
  |[] -> [];;

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
		    tab_plac.(i) <- (genere_placement (get i fl) (repart.(i)) (i::ladresse))
        done;
		rep := (mul_produit tab_plac ladresse) @ (!rep);
 		suivant (n-1) repart tt;
	      done;
	    end;
       end;
((sd !rep));;
let rec get_subtree t adr = match adr with
  |p::q -> 
    begin
      match t with
	|Server(fl,_,_) -> get_subtree (get p fl) q
	|Node(fl,_) ->   get_subtree (get p fl) q
    end
  |[] -> t;;

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
  (sd !rep);;

let rec no_son_server t = match t with
  |Server(fl,_,_) -> apply_nss fl
  |Node(fl,_) -> apply_nss fl
and apply_nss l = match l with
  |Server(_,_,_)::q -> false
  |Node(_,_)::q -> apply_nss q
  |[] -> true;;

let rec tot tree = match tree with
  |Node(fl,ent) -> ent + apply_tot fl
  |Server(fl,n,ent) -> ent + apply_tot fl
and apply_tot fl = match fl with
  |p::q -> tot p + apply_tot q
  |[] -> 0
;;
let maxminl l =
  let repmax = ref min_int in
  let repmin = ref max_int in
  let rec aux l = match l with
    |p::q -> if p > !repmax then repmax := p ;  if p < !repmin then repmin := p ; aux q
    |[] -> ()
  in
  aux l;
  !repmax,!repmin;;

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
  a<=b;;

let rec in_normal_forme t = match t with
  |Server(fl,n,ent) -> enter_value fl && apply_inf fl
  |Node(fl,ent) -> true
and apply_inf fl = match fl with
  |p::q -> in_normal_forme p && apply_inf q
  |[] -> true;;
    
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
  rep;;

let rec ltf t = match t with
  |Server(fl,n,ent) -> colle (apply_ltf fl)
  |Node(_,_) -> [||]
and apply_ltf l = match l with
  |p::q ->
    (ltf p) :: (apply_ltf q)
  |[] -> []
;;
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
  else (n < m);;

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
;;
let rec nb_server t = match t with
  |Server(fl,_,_) -> apply_nb_server fl + 1
  |Node(_,_) -> 0
and apply_nb_server fl =  match fl with
  |p::q -> nb_server p + apply_nb_server q
  |[] -> 0;;
let rec nb_server_son fl = match fl with
  |p::q -> (nb_server p)::(nb_server_son q)
  |[] -> [];;

let rec cp l1 l2 = match l1,l2 with
  |p1::q1,p2::q2 -> if (p2 > p1) then (false,false) else (if p1 > p2 then (true,false) else cp q1 q2)
  |[],[] -> (false,true)
  |_ -> failwith "not the same trees";;

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
;;

let rec recup_contient l l1 = match l with
  |[] -> []
  |p::q -> if inclus l1 p then (p :: (recup_contient q l1)) else (recup_contient q l1)
;;
let liste_son adr n =
  let rep = ref [] in
  for i = 0 to (n-1) do
    rep := (List.rev (i :: (List.rev adr))):: !rep
  done;
  !rep;;
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
	|Node(fl,_) -> son_list (get p fl) q adrinit
	|Server(fl,_,_) -> son_list (get p fl) q adrinit
    end;;

let rec mega_son_list t l = match l with
  |p::q -> (son_list t p p) @ mega_son_list t q
  |[] -> [];;

let rec map_rev l = match l with
  |p::q -> (List.rev p)::(map_rev q)
  |[] -> [];;

let rec ajoute_partout a l = match l with
  |p::q -> ((map_rev a)@p)::(ajoute_partout a q)
  |[] -> [];;

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
  t.(0) <- !valid;;

let n_tree_to_l n t n_to_a =
  
  let rep = ref [] in
  for i = 0 to (n-1) do
    if t.(i) = 1 then 
      (
	rep := (n_to_a.(i)) :: !(rep)
      )
  done;
  !rep;;

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
  let min = ref max_int in
  let argmin = ref (Node([],0)) in
  while (repart.(0) <> (-1)) do
    let l = n_tree_to_l n repart n_to_ad in
    let a = cree_tarbre t l [] in
    let b = tarbre_to_arbre a in
    let usl = opt b in
    let c = puiss b in
    if (c <= !min) then
      (
	min := c;
	argmin := a
      );
    next repart t_pere p;
  done;
  !min,!argmin;;

let brute_force t (* l'arbre *) n (*le nombre de serveurs*) = (*brute force le positionnement optimal de n serveurs dans t*)
  let reparts = genere_placement t n [] in
  let min = ref max_int in
  let argmin = ref [] in
  let rec recherche l = match l with
    |p::q ->
      begin
	let a = cree_tarbre t p [] in
	let b = tarbre_to_arbre a in
	let usl = opt b in
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
  (!min,!targmin);;

let rec brute_force_a t n =
  match n with
    |1 -> (brute_force t 1,[[]])
    |_ -> 
      begin

	let ((_,_),l) = brute_force_a t (n-1) in
	let ls = remove_l l (mega_son_list t l) in
	let m = n - (List.length l) in
	let repart = genere_placement_a t m ls in
	let reparts = ajoute_partout l repart in
	let min = ref max_int in
	let argmin = ref t in
	let rec recherche l = match l with
	  |p::q ->
	    begin
	      let a = cree_tarbre t p [] in
	      let b = tarbre_to_arbre a in
	      let usl = opt b in
	      let c = puiss b in
	      if (c < !min) then
		begin
		  min := c;
		  argmin := a
		end
	      else
		begin
		  if c = (!min) then
		    argmin := a
		end;
	      recherche q
	    end
	  |[]  -> ()
	in
	recherche reparts;
	let e = ens_satur (!argmin) n in
	let lfin = ajoutel e l in
	((!min,!argmin),lfin)
      end;;
let rec verif_l l = match l with
  |p::q ->
    let b = tarbre_to_arbre p in
    let usl = opt b in
    let c = puiss b in
    print_int c ; print_newline ();
    verif_l q
  |[] -> ();;
(* \ Partie algorithme optimale (par brute-force de la solution) *)


(* Partie algorithme basé dur opt(n) \include opt(n+1) *)

let adresse_fils ad n = (*renvoit les adresses de tout les fils de celui situé à l'adresse ad *)
  if n = 0 then []
  else
    let rep = ref [] in
    for i = 0 to (n-1) do
      rep := (List.rev (i::(List.rev ad))):: (!rep)
    done;
    !rep;;

let rec ajout_server t ad adinit = match ad with (* rajoute un serveur dans t à l'adresse ad et renvoit l'adresse de tous ses fils *)
  |p::q ->
    begin
      match t with
	|Server(fl,w,ent) -> 
	  let a,b,c = coupel fl p in
	  let d,e = ajout_server b q adinit in
	  Server(a@[d]@c,w,ent),e
	|Node(fl,ent) -> 
	  let a,b,c = coupel fl p in
	  let d,e = ajout_server b q adinit in
	  Node(a@[d]@c,ent),e
    end
  |[] ->
    begin
      match t with
	|Server(fl,c,ent) ->
	  print_l ad;
	  failwith "server already placed"
	|Node(fl,ent) -> 
	  (Server(fl,{ n=0; w=0},ent),adresse_fils adinit (List.length fl))
    end;;


let algo_intelligent t n = (*calcul la solution du problème de placement des serveurs sous l'hypothèse précédante *)
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0 in
  let repargmin = ref t in
  for i = 1 to n do
    let mini = ref max_int in
    let argmin = ref t in
    let choix = ref [] in
    let potentiel = ref [] in
    (*print_string "accessible :\n";
    print_l_l (!accessible);print_newline ();*)
    let rec test_acces l = match l with
      |p::q ->
	(*print_string "ajout serveur appellé avec :\n";
	print_arbre (!tree);print_newline ();
	print_l p; print_newline ();*)
	let t_test,fils = ajout_server (!tree) p p in
	(*print_string "fin ajout serveur\n";*)
	let b = tarbre_to_arbre t_test in
	let usl = opt b in
	let c = puiss b in
	if c < !mini then
	  begin
	    mini := c;
	    argmin := t_test;
	    choix := p;
	    potentiel := fils
	  end;
	test_acces q
      |[] -> ()
    in
    test_acces (!accessible);
    accessible := (!potentiel) @ (remove (!choix) (!accessible));

    tree := !argmin;
    (*print_string "new: ";print_arbre (!tree);print_newline ();*)
    if i = n then
      (repmin := !mini; 
       repargmin := !argmin);

  done;
  let usl = opt (tarbre_to_arbre (!repargmin)) in 
  !repmin,!repargmin;;

(* \ Partie algorithme basé sur opt(n) \include opt(n+1) *)

(* Partie algorithme amélioré du précedant par la modification du placement du plus petit serveur*)

let find_min_arbre t pasadr= (*trouve le plus petit élément de t (différent de pasadr )*)
  let rep = ref [] in
  let rec aux t adr= match t with
    |Noeud(fl,n,ent) -> 
      if fl = [] then 
	rep := (n.w,adr) :: (!rep)
      else
	applyaux fl 0 adr
  and applyaux l i adr= match l with
    |p::q -> aux p (i::adr) ; applyaux q (i+1) adr
    |[] -> ()
  in
  aux t [];
  let rec find_min l min argmin= match l with
    |p::q -> if (fst p < min && (snd p <> pasadr)) then find_min q (fst p) (snd p)
      else find_min q min argmin
    |[] -> min,(List.rev argmin)
  in
  find_min (!rep) max_int []
;;

let remove_min_arbre t pasadr= (* enleve le plus petit serveur  de l'arbre *)
  let t1 = tarbre_to_arbre t in
  (*print_string "rma :" ; print_arbre t ; print_newline ();*)
  let usl = opt t1 in
  let a,b = find_min_arbre (t1) pasadr in
  if a = max_int then (t,[-1])
  else 
    begin(*print_string "minimum de l'arbre :"; print_l b; print_newline ();*)
      let rec ote tr adr parc= match adr with
	|[] -> 
	  begin
	    match tr with
	      |Node(fl,ent) -> failwith "already a node"
	      |Server(fl,n,ent) -> (*print_string "adresse b : " ; print_l (List.rev (parc)) ;print_newline ();*) Node(fl,ent),List.rev (parc)
	  end
	|p::q ->
	  begin
	    match tr with
	      |Node(fl,ent) -> failwith "reached the boundary of server"
	      |Server(fl,n,ent) -> 
	    (*print_string "coupel_ext : "; print_int p ; print_string " "; print_int (List.length fl);print_newline ();*)
		let a,b,c = coupel2 fl p in
		let c1,c2 = ote b q ((List.length a) :: parc) in
		Server(a@[c1]@c,n,ent),c2
	  end
      in
      ote t (b) []
    end;;


let rec enleve_cont_pref pref l = match l with (*enleve de l toutes les adresses dont pref est prefixe *)
  |p::q -> if prefixe pref p then enleve_cont_pref pref q else p :: (enleve_cont_pref pref q)
  |[] -> [];;

let algo_intelligent_bis t n = (*résoud le problème du placement des serveurs de la même manire que précedemment mais en essayant de replacer de manière optimale le serveur le moins chargé *)
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0 in
  let repargmin = ref t in
  for i = 1 to n do
    let mini = ref max_int in
    let argmin = ref t in
    let choix = ref [] in
    let potentiel = ref [] in
    (*print_string "accessible :\n";
    print_l_l (!accessible);print_newline ();*)
    let rec test_acces l = match l with
      |p::q ->
	(*print_string "ajout serveur appellé avec :\n";*)
	(*print_arbre (!tree);print_newline ();
	(*print_l p; print_newline ();*)*)
	let t_test,fils = ajout_server (!tree) p p in
	(*print_string "fin ajout serveur\n";*)
	let b = tarbre_to_arbre t_test in
	let usl = opt b in
	let c = puiss b in
	if c < !mini then
	  begin
	    mini := c;
	    argmin := t_test;
	    choix := p;
	    potentiel := fils
	  end;
	test_acces q
      |[] -> ()
    in
    (*print_string "i : ";print_int i;print_newline ();*)
    test_acces (!accessible);
    accessible := (!potentiel) @ (remove (!choix) (!accessible));
    tree := !argmin;

    let a,b = remove_min_arbre (!tree) !choix in
    if b <> [-1] then 
      begin
	tree := a;
	accessible := b::(enleve_cont_pref b (!accessible)); 
	mini := max_int;
	test_acces (!accessible);
	accessible := (!potentiel) @ (remove (!choix) (!accessible));
	tree := !argmin;
      end
    else
      ();
    if i = n then
      (repmin := !mini; 
       repargmin := !argmin);
  done;
  let usl = opt (tarbre_to_arbre (!repargmin)) in 
  !repmin,!repargmin;;

(* \ Partie algorithme amélioré du précedant par la modification du placement du plus petit serveur*)


(*  Partie algorithme amélioré du premier *)

(*Lorsqu'on ajoute un serveur s , on réoptimise tout les arbres enracinés en les fils d'un acêtre de s lorsque le flux sortant de cette arbre a changé entre les deux itérations *)

let rec copy_tree tr = match tr with (* fais une copie de tr*)
  |Node(a,b) -> Node(apply_copy_tree a,b)
  |Server(a,no,b) -> Server(apply_copy_tree a,{n = no.n ; w = no.w},b)
and apply_copy_tree l = match l with
  |p::q -> (copy_tree p) :: (apply_copy_tree q)
  |[] -> [] ;;

let sortie_tree t = (*Calcul le flux sortant de l'arbre *)
  let rep = ref 0 in
  let rec aux tr = match tr with
    |Node(a,b) -> rep := b + !rep ; apply_aux a
    |Server(a,e,b) -> rep := b + !rep - e.w ; apply_aux a
  and apply_aux l = match l with
    |p::q -> aux p ; apply_aux q
    |[] -> ()
  in
  aux t;
  !rep;;
let rec remet_a_zero t = match t with (* RAZ *) 
  |Node(fl,e) -> Node(fl,e)
  |Server(fl,n,e) -> Node(apply_raz fl,e)
and apply_raz fl = match fl with
  |p::q -> (remet_a_zero p) :: (apply_raz q)
  |[] -> [];;

let recalc_potentiel t = (*réapplique algorithme_ter sur tous les ancêtres du noeud modifié*)
  let rep = ref [] in
  let rec aux tr adr= match tr with
    |Node(_,_) -> rep := (List.rev adr) :: !(rep)
    |Server(fl,a,e) -> apply_aux fl adr 0
  and apply_aux fl adr i = match fl with
    |p::q -> aux p (i::adr) ; apply_aux q adr (i+1)
    |[] -> ()
  in
  aux t [];
  !rep;;
let rec reapplique adr at nt =
  let rec aux adr a b = match adr with
    |p::q ->
      let Server(fl,n,ent) = a in
      let Server(fl1,n2,ent2) = b in
      Server(apply (fl) (fl1) 0 adr,n2,ent2)
    |[] -> b
  and apply l1 l2 i adr= match l1,l2 with
    |p::q,p2::q2 -> 
      let t::r = adr in
      if (t=i) then 
	(aux r p p2) :: (apply q q2 (i+1) adr)
      else
	begin
	  match p,p2 with
	    |Server(_,n1,_),Server(_,n2,_) ->
	      let aps = sortie_tree p2 in
	      let bef = sortie_tree p in
	      (*print_string "valeur de sortie du noeud "; print_l (adr) ; print_string ", avant : " ; print_int (bef); print_string ", après : "; print_int (aps); print_string "la valeur de mon arbre est : "; print_int (n2.w); print_newline ();*)
	      if sortie_tree p <> sortie_tree p2 then 
		begin
		  let ta = List.length (ens_pres p) in
		  let valu,rep = algo_intelligent_ter (remet_a_zero p2) ta (sortie_tree p2) in
	          rep :: (apply q q2 (i+1) adr )
		end
	      else (p2 :: (apply q q2 (i+1) adr) )
	    |Node(_,_),Node(_,_) -> p2 :: (apply q q2 (i+1) adr)
	end
    |[],[] -> []
  in
  aux adr at nt

and algo_intelligent_ter t n k= (*calcul le placement optimal de n serveurs dans l'arbre t en laissant sortir k *) (* non optimal *)
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0 in
  let repargmin = ref t in
  let argmin = ref t in
  for i = 1 to n do
    let mini = ref max_int in
    let choix = ref [] in
    let potentiel = ref [] in
    accessible := recalc_potentiel (!tree);
    (*print_string "accessible :\n";
    print_l_l (!accessible);print_newline ();*)
    let rec test_acces l = match l with
      |p::q ->
	(*print_string "ajout serveur appellé avec :\n";
	print_arbre (!tree);print_newline ();
	print_l p; print_newline ();*)
	let t_test,fils = ajout_server (!tree) p p in
	(*print_string "fin ajout serveur\n";*)
	let b = tarbre_to_arbre t_test in
	let usl = opt_k b k in
	let c = puiss b in
	(*print_int c;
	print_arbre t_test;*)
	if c < !mini then
	  begin
	    mini := c;
	    argmin := t_test;
	    choix := p;
	    potentiel := fils
	  end;
	test_acces q
      |[] -> ()
    in
    let taux = copy_tree (!argmin) in
    test_acces (!accessible);
    let b = tarbre_to_arbre (!argmin) in
    let usl = opt_k b k in
    accessible := (!potentiel) @ (remove (!choix) (!accessible));
    if i > 1 then
      begin
	let auxi = reapplique (!choix) taux (!argmin) in
	argmin := auxi;
      end;
    tree := !argmin;


    let a,b = remove_min_arbre (!tree) !choix in
    if b <> [-1] then 
      begin
	tree := a;
	accessible := b::(enleve_cont_pref b (!accessible)); 
	mini := max_int;
	accessible := recalc_potentiel (!tree);
	test_acces (!accessible);
	let b = tarbre_to_arbre (!argmin) in
	let usl = opt_k b k in
	accessible := (!potentiel) @ (remove (!choix) (!accessible));
	tree := !argmin;
      end;
    if i = n then
      (repmin := !mini; 
       repargmin := !argmin);

  done;
  puisst (!repargmin),!repargmin;;
print_string "test L1412";; print_newline ();;

(*  \Partie algorithme amélioré du premier *)

(* Greedy 4 *)
let rec son_server l = match l with
  |Server(_,_,_) :: q -> true
  |Node(_,_) ::q -> son_server q
  |[] -> false;;

let list_to_remove tree = 
  let rep = ref [] in
  let rec aux t adr = match t with
    |Server(fl,_,_) ->
      if (not (son_server fl)) then 
	rep := (List.rev adr) :: (!rep)
      else
	apply_aux fl adr 0
    |Node(_,_) -> ()
  and apply_aux l adr i = match l with
    |p::q -> aux p (i::adr) ; apply_aux q adr (i+1)
    |[] -> ()
  in
  aux tree [];
  !rep;;

let remove_server t adr =
  let rec ote tr adr= match adr with
    |[] -> 
      begin
	match tr with
	  |Node(fl,ent) -> failwith "already a node"
	  |Server(fl,n,ent) -> (*print_string "adresse b : " ; print_l (List.rev (parc)) ;print_newline ();*) Node(fl,ent)
      end
    |p::q ->
      begin
	match tr with
	  |Node(fl,ent) -> failwith "reached the boundary of server"
	  |Server(fl,n,ent) -> 
		(*print_string "coupel_ext : "; print_int p ; print_string " "; print_int (List.length fl);print_newline ();*)
	    let a,b,c = coupel fl p in
	    let c1 = ote b q in
	    Server(a@[c1]@c,n,ent)
      end
  in
  ote t adr;;

let rec reapplique_4 adr at nt triple =
  let rec aux adr a b = match adr with
    |p::q ->
      let Server(fl,n,ent) = a in
      let Server(fl1,n2,ent2) = b in
      Server(apply (fl) (fl1) 0 adr,n2,ent2)
    |[] -> b
  and apply l1 l2 i adr= match l1,l2 with
    |p::q,p2::q2 -> 
      let t::r = adr in
      if (t=i) then 
	(aux r p p2) :: (apply q q2 (i+1) adr)
      else
	begin
	  match p,p2 with
	    |Server(_,n1,_),Server(_,n2,_) ->
	      let aps = sortie_tree p2 in
	      let bef = sortie_tree p in
	      (*print_string "valeur de sortie du noeud "; print_l (adr) ; print_string ", avant : " ; print_int (bef); print_string ", après : "; print_int (aps); print_string "la valeur de mon arbre est : "; print_int (n2.w); print_newline ();*)
	      if sortie_tree p <> sortie_tree p2 then 
		begin
		  let ta = List.length (ens_pres p) in
		  let valu,rep = algo_intelligent_4 (remet_a_zero p2) ta (sortie_tree p2) triple in
	          rep :: (apply q q2 (i+1) adr )
		end
	      else (p2 :: (apply q q2 (i+1) adr) )
	    |Node(_,_),Node(_,_) -> p2 :: (apply q q2 (i+1) adr)
	end
    |[],[] -> []
  in
  aux adr at nt

and algo_intelligent_4 t n k (ba,deb,fin) = (*calcul le placement optimal de n serveurs dans l'arbre t en laissant sortir k *) (* non optimal *)
  let tail = taille_tree t in
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0 in
  let repargmin = ref t in
  let argmin = ref t in
  for i = 1 to n do
    let d = (ba && i >= deb && i <=fin) in
    (*print_arbre (!argmin);
      print_newline ();*)
    (* calcul de n *)
    let mini = ref max_int in
    let choix = ref [] in
    let potentiel = ref [] in
    accessible := recalc_potentiel (!tree);
    let rec test_acces l = match l with
      |p::q ->
	let t_test,fils = ajout_server (!tree) p p in
	let b = tarbre_to_arbre t_test in
	let usl = opt_k b k in
	let c = puiss b in
	if c < !mini then
	  begin
	    mini := c;
	    argmin := t_test;
	    choix := p;
	    potentiel := fils
	  end;
	test_acces q
      |[] -> ()
    in
    let taux = copy_tree (!argmin) in
    test_acces (!accessible);
    let b = tarbre_to_arbre (!argmin) in
    let usl = opt_k b k in
    if d then (
      print_arbre_latex (!tree);
    );
    if i > 1 then
      begin
	let auxi = reapplique_4 (!choix) taux (!argmin) (ba,deb,fin)  in
	argmin := auxi;
      end;
    tree := !argmin;
    if d then (
      print_string "\paragraph{}\n fin recalcul des sous arbres\n";
      print_arbre_latex (!tree); print_newline ();
    );
    let a,b = remove_min_arbre (!tree) !choix in
    if b <> [-1] then 
      begin
	tree := a;
	accessible := b :: (enleve_cont_pref b (!accessible)); 
	mini := max_int;
	accessible := recalc_potentiel (!tree);
	test_acces (!accessible);
	let b = tarbre_to_arbre (!argmin) in
	let usl = opt_k b k in
	tree := !argmin;
      end;
    if d then (
      print_string "\paragraph{}\n fin deplacement minimum\n";
      print_arbre_latex (!tree); print_newline ();
    );
    let backup = copy_tree (!tree) in
    if (i < tail) then
      begin
    (* calcul de n+1  *)
	mini := max_int;
	let choix = ref [] in
	let potentiel = ref [] in
	accessible := recalc_potentiel (!tree);
	let taux = copy_tree (!argmin) in
	test_acces (!accessible);
	let b = tarbre_to_arbre (!argmin) in
	let usl = opt_k b k in
	if i > 1 then
	  begin
	    let auxi = reapplique_4 (!choix) taux (!argmin)  (ba,deb,fin) in
	    argmin := auxi;
	  end;
	tree := !argmin;
	if d then (
	print_string "\paragraph{}\n fin recalcul des sous arbres (n+1)\n";

	print_arbre_latex (!tree); print_newline ();
	);
	let a,b = remove_min_arbre (!tree) !choix in
	if b <> [-1] then 
	  begin
	    tree := a;
	    accessible := b::(enleve_cont_pref b (!accessible)); 
	    mini := max_int;
	    accessible := recalc_potentiel (!tree);
	    test_acces (!accessible);
	    let b = tarbre_to_arbre (!argmin) in
	    let usl = opt_k b k in
	    tree := !argmin;
	  end;

	accessible := recalc_potentiel (!tree);
	if d then (
	  print_string "\paragraph{}\n fin deplacement minimum (n+1) \n";	
	  print_arbre_latex (!tree); print_newline ();
	);
	(* fin calcul de n+1  *)
	(*print_arbre_latex (!tree);*)
	(*début retour à n *)
	mini := max_int;
	let rec test_enleve l = match l with
	  |[] -> ()
	  |p::q -> 
	    let t = remove_server (!tree) p in
	    let b = tarbre_to_arbre (t) in
	    let usl = opt_k b k in
	    let c = puiss b in
	    (*print_l p ; print_newline (); print_int c; print_newline ();*)
	    if c < (!mini) then
	      begin
		mini := c;
		argmin := t;
	      end;
	    test_enleve q
	in
	let lir = list_to_remove (!tree) in
	test_enleve (lir);
	(*print_string "lir\n";
	print_l_l lir;*)
	let b = tarbre_to_arbre (!argmin) in
	let usl = opt_k b k in
	tree := (!argmin);
	accessible := recalc_potentiel (!tree);
	if d then (
	  print_string "\paragraph{}\n fin enlever un serveur \n";
	  print_arbre_latex (!tree); print_newline ();
	);
      (*fin retour à n *)
      end;
    if (puisst backup) < (puisst (!tree)) then
      begin
	tree := backup ;
	argmin := backup
      end;
    if i = n then
      (repmin := !mini; 
       repargmin := !argmin);

  done;
  puisst (!repargmin),!repargmin;;

(* \Greedy 4 *)
let to_move t = 
  let rep = ref [] in
  let rec aux t adr = match t with
    |Server(fl,a,ent) -> if apply_nss fl then (rep := (List.rev adr) :: (!rep)) else apply_aux fl adr 0
    |Node(_,_) -> ()
  and apply_aux l adr i = match l with
    |p::q -> aux p (i :: adr) ; apply_aux q adr (i+1)
    |[] -> ()
  in
  aux t [];
  !rep;;
 
(* Greedy 5 *)
let rec reapplique_5 adr at nt triple =
  let rec aux adr a b = match adr with
    |p::q ->
      let Server(fl,n,ent) = a in
      let Server(fl1,n2,ent2) = b in
      Server(apply (fl) (fl1) 0 adr,n2,ent2)
    |[] -> b
  and apply l1 l2 i adr= match l1,l2 with
    |p::q,p2::q2 -> 
      let t::r = adr in
      if (t=i) then 
	(aux r p p2) :: (apply q q2 (i+1) adr)
      else
	begin
	  match p,p2 with
	    |Server(_,n1,_),Server(_,n2,_) ->
	      let aps = sortie_tree p2 in
	      let bef = sortie_tree p in
	      (*print_string "valeur de sortie du noeud "; print_l (adr) ; print_string ", avant : " ; print_int (bef); print_string ", après : "; print_int (aps); print_string "la valeur de mon arbre est : "; print_int (n2.w); print_newline ();*)
	      if sortie_tree p <> sortie_tree p2 then 
		begin
		  let ta = List.length (ens_pres p) in
		  let valu,rep = algo_intelligent_5 (remet_a_zero p2) ta (sortie_tree p2) triple in
	          rep :: (apply q q2 (i+1) adr )
		end
	      else (p2 :: (apply q q2 (i+1) adr) )
	    |Node(_,_),Node(_,_) -> p2 :: (apply q q2 (i+1) adr)
	end
    |[],[] -> []
  in
  aux adr at nt
and algo_intelligent_5 t n k (ba,deb,fin) = (*calcul le placement optimal de n serveurs dans l'arbre t en laissant sortir k *) (* non optimal *)
  let tail = taille_tree t in
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0 in
  let repargmin = ref t in
  let argmin = ref t in
  for i = 1 to n do
    let d = (ba && i >= deb && i <=fin) in
    (*print_arbre (!argmin);
      print_newline ();*)
    (* calcul de n *)
    let mini = ref max_int in
    let choix = ref [] in
    let potentiel = ref [] in
    accessible := recalc_potentiel (!tree);
    let rec test_acces l = match l with
      |p::q ->
	let t_test,fils = ajout_server (!tree) p p in
	let b = tarbre_to_arbre t_test in
	let usl = opt_k b k in
	let c = puiss b in
	if c < !mini then
	  begin
	    mini := c;
	    argmin := t_test;
	    choix := p;
	    potentiel := fils
	  end;
	test_acces q
      |[] -> ()
    in
    let taux = copy_tree (!argmin) in
    test_acces (!accessible);
    let b = tarbre_to_arbre (!argmin) in
    let usl = opt_k b k in
    if d then (
      print_arbre_latex (!tree);
    );
    if i > 1 then
      begin
	let auxi = reapplique_5 (!choix) taux (!argmin) (ba,deb,fin)  in
	argmin := auxi;
      end;
    tree := !argmin;
    if d then (
      print_string "\paragraph{}\n fin recalcul des sous arbres\n";
      print_arbre_latex (!tree); print_newline ();
    );
    let a,b = remove_min_arbre (!tree) !choix in
    if b <> [-1] then 
      begin
	tree := a;
	accessible := b :: (enleve_cont_pref b (!accessible)); 
	mini := max_int;
	accessible := recalc_potentiel (!tree);
	test_acces (!accessible);
	let b = tarbre_to_arbre (!argmin) in
	let usl = opt_k b k in
	tree := !argmin;
      end;
    if d then (
      print_string "\paragraph{}\n fin deplacement minimum\n";
      print_arbre_latex (!tree); print_newline ();
    );
    let backup = copy_tree (!tree) in
    if (i < tail) then
      begin
    (* calcul de n+1  *)
	mini := max_int;
	let choix = ref [] in
	let potentiel = ref [] in
	accessible := recalc_potentiel (!tree);
	let taux = copy_tree (!argmin) in
	test_acces (!accessible);
	let b = tarbre_to_arbre (!argmin) in
	let usl = opt_k b k in
	if i > 1 then
	  begin
	    let auxi = reapplique_4 (!choix) taux (!argmin)  (ba,deb,fin) in
	    argmin := auxi;
	  end;
	tree := !argmin;
	if d then (
	print_string "\paragraph{}\n fin recalcul des sous arbres (n+1)\n";

	print_arbre_latex (!tree); print_newline ();
	);
	let a,b = remove_min_arbre (!tree) !choix in
	if b <> [-1] then 
	  begin
	    tree := a;
	    accessible := b::(enleve_cont_pref b (!accessible)); 
	    mini := max_int;
	    accessible := recalc_potentiel (!tree);
	    test_acces (!accessible);
	    let b = tarbre_to_arbre (!argmin) in
	    let usl = opt_k b k in
	    tree := !argmin;
	  end;

	accessible := recalc_potentiel (!tree);
	if d then (
	  print_string "\paragraph{}\n fin deplacement minimum (n+1) \n";	
	  print_arbre_latex (!tree); print_newline ();
	);
	(* fin calcul de n+1  *)
	(*print_arbre_latex (!tree);*)
	(*début retour à n *)
	mini := max_int;
	let rec test_enleve l = match l with
	  |[] -> ()
	  |p::q -> 
	    let t = remove_server (!tree) p in
	    let b = tarbre_to_arbre (t) in
	    let usl = opt_k b k in
	    let c = puiss b in
	    (*print_l p ; print_newline (); print_int c; print_newline ();*)
	    if c < (!mini) then
	      begin
		mini := c;
		argmin := t;
	      end;
	    test_enleve q
	in
	let lir = list_to_remove (!tree) in
	test_enleve (lir);
	(*print_string "lir\n";
	print_l_l lir;*)
	let b = tarbre_to_arbre (!argmin) in
	let usl = opt_k b k in
	tree := (!argmin);
	accessible := recalc_potentiel (!tree);
	if d then (
	  print_string "\\paragraph{}\n fin enlever un serveur \n";
	  print_arbre_latex (!tree); print_newline ();
	);
      (*fin retour à n *)
      end;
    if (puisst backup) < (puisst (!tree)) then
      begin
	tree := backup ;
	argmin := backup
      end;
    let l = to_move (!argmin) in
    let mini = ref (max_int) in
    let rec applique li_to_move = match li_to_move with
      |[] -> ()
      |p::q ->
	begin
	  let t_test = remove_server (!tree) p in
	  accessible := recalc_potentiel (t_test);
	  test_put t_test (!accessible);
	  applique q
	end
    and test_put tr_test acces = match acces with
      |[] -> ()
      |p::q ->
	begin
	  let t_test,fils = ajout_server (tr_test) p p in
	  let b = tarbre_to_arbre t_test in
	  let usl = opt_k b k in
	  let c = puiss b in
	  if c < !mini then
	    begin
	      mini := c;
	      argmin := t_test;
	      choix := p;
	      potentiel := fils
	    end;
	  test_put tr_test q
	end
    in

    applique l;
    let usl = opt_k (tarbre_to_arbre (!argmin)) k in
    accessible := recalc_potentiel (!argmin);
    tree := !argmin;
    if d then (
      print_string "\\paragraph{}\n fin essaie de bouger les serveurs \n";
      print_arbre_latex (!tree); print_newline ();
    );
    if i = n then
      (repmin := !mini; 
      repargmin := !argmin);
  done;
  puisst (!repargmin),!repargmin;;
(* \Greedy 5 *)
(* Greedy 6 *)

let algo_intelligent_6 t n = (*calcul la solution du problème de placement des serveurs sous l'hypothèse précédante *)
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0 in
  let repargmin = ref t in
  for i = 1 to n do
    let mini = ref max_int in
    let argmin = ref t in
    let choix = ref [] in
    let potentiel = ref [] in

    accessible := recalc_potentiel (!tree);
    (*print_string "accessible :\n";
    print_l_l (!accessible);print_newline ();*)
    let rec test_acces l = match l with
      |p::q ->
	(*print_string "ajout serveur appellé avec :\n";
	print_arbre (!tree);print_newline ();
	print_l p; print_newline ();*)
	let t_test,fils = ajout_server (!tree) p p in
	(*print_string "fin ajout serveur\n";*)
	let b = tarbre_to_arbre t_test in
	let usl = opt b in
	let c = puiss b in
	if c < !mini then
	  begin
	    mini := c;
	    argmin := t_test;
	    choix := p;
	    potentiel := fils
	  end;
	test_acces q
      |[] -> ()
    in
    test_acces (!accessible);

    tree := !argmin;
    let backup = ref (copy_tree (!tree)) in

    let continue = ref true in
    while (!continue) do 
    (*print_string "new: ";print_arbre (!tree);print_newline ();*)

      accessible := recalc_potentiel (!tree);
      let l = to_move (!argmin) in
      let mini = ref (max_int) in
      let rec applique li_to_move = match li_to_move with
	|[] -> ()
	|p::q ->
	  begin
	    let t_test = remove_server (!tree) p in
	    accessible := recalc_potentiel (t_test);
	    test_put t_test (!accessible);
	    applique q
	  end
      and test_put tr_test acces = match acces with
	|[] -> ()
	|p::q ->
	  begin
	    let t_test,fils = ajout_server (tr_test) p p in
	    let b = tarbre_to_arbre t_test in
	    let usl = opt b  in
	    let c = puiss b in
	    if c < !mini then
	      begin
		mini := c;
		argmin := t_test;
		choix := p;
		potentiel := fils
	      end;
	    test_put tr_test q
	  end
      in
      applique l;

      if (puisst (!tree)) < (puisst (!backup)) then
	begin
	  argmin := !tree;
	  backup := !tree
	end
      else
	begin
	  continue := false
	end
    
    done;
    if i = n then
      (repmin := !mini; 
       repargmin := !argmin);

  done;
  let b = opt (tarbre_to_arbre (!repargmin)) in  
  puisst (!repargmin),!repargmin;;

(* \Greedy 6 *)
(* Greedy 7 *)
let algo_intelligent_7 t n = (*calcul la solution du problème de placement des serveurs sous l'hypothèse précédante *)
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0 in
  let repargmin = ref t in
  let compt_decr = ref 0 in

  for i = 1 to n do
    let mini = ref max_int in
    let argmin = ref t in
    let choix = ref [] in
    let potentiel = ref [] in

    accessible := recalc_potentiel (!tree);
    (*print_string "accessible :\n";
    print_l_l (!accessible);print_newline ();*)
    let rec test_acces l = match l with
      |p::q ->
	(*print_string "ajout serveur appellé avec :\n";
	print_arbre (!tree);print_newline ();
	print_l p; print_newline ();*)
	let t_test,fils = ajout_server (!tree) p p in
	(*print_string "fin ajout serveur\n";*)
	let b = tarbre_to_arbre t_test in
	let usl = opt b in
	let c = puiss b in
	if c < !mini then
	  begin
	    mini := c;
	    argmin := t_test;
	    choix := p;
	    potentiel := fils
	  end;
	test_acces q
      |[] -> ()
    in
    test_acces (!accessible);

    tree := !argmin;
    let backup = ref (copy_tree (!tree)) in

    let continue = ref (i>2) in
    let nb = ref 0 in 
    while (!continue && !nb < 1) do
      incr nb;
      accessible := recalc_potentiel (!tree);
      let l = to_move (!argmin) in
      let mini = ref (max_int) in
      let rec applique li_to_move = match li_to_move with
	|[] -> ()
	|p::q ->
	  begin
	    let t_test = remove_server (!tree) p in
	    let l2 = to_move (t_test) in
	    let rec applique_2 li_tm = match li_tm with
	      |p::q -> 
		begin
		  let t_test2 = remove_server (t_test) p in
		  let rec test_met tr ac = match ac with
		    |[] -> ()
		    |p::q ->
		      begin
			let t_test3,fils = ajout_server (tr) p p in

			accessible := recalc_potentiel (t_test3);
			test_put t_test3 (!accessible);
			test_met tr q
		      end
		  in
		  accessible := recalc_potentiel (t_test2);
		  test_met t_test2 (!accessible);
		  applique_2 q
		end
	      |[] -> ()
	    in
	    applique_2 l2;
	    applique q
	  end
      and test_put tr_test acces = match acces with
	|[] -> ()
	|p::q ->
	  begin
	    let t_test,fils = ajout_server (tr_test) p p in
	    let b = tarbre_to_arbre t_test in
	    let usl = opt b  in
	    let c = puiss b in
	    if c < !mini then
	      begin
		mini := c;
		argmin := t_test;
		choix := p;
		potentiel := fils
	      end;
	    test_put tr_test q
	  end
      in
      applique l;
      tree := !argmin;
      if (puisst (!tree)) < (puisst (!backup)) then
	begin
	  argmin := !tree;
	  backup := !tree;
	  if i = n then (incr compt_decr);
	end
      else
	begin
	  continue := false
	end
    
    done;
    if i = n then
      (repmin := !mini; 
       repargmin := !argmin);

  done;

  let b = opt (tarbre_to_arbre (!repargmin)) in  
  puisst (!repargmin),!repargmin,!compt_decr;;

(* \Greedy 7 *)

print_string "test L2088";; print_newline ();;
let etude_nb_decr nb_tree deb fin pas entree_max =
  let ta = ref deb in 
  let reps = "res_nb_tree_nbtree="^(string_of_int nb_tree)^"beg="^(string_of_int deb)^"end="^(string_of_int fin)^"step="^(string_of_int pas)^"max"^(string_of_int entree_max)^".txt" in
  let sout = open_out reps in
  while (!ta <= fin) do 
    begin
      let aver = ref 0 in
      let max = ref 0 in
      for i = 0 to (nb_tree -1) do
  	let tree = (cree_alea_binaire (!ta) (entree_max)) in
	let a,b,c = algo_intelligent_7 tree (!ta/2) in
	if c > (!max) then max := c ;
	aver := !aver + c ;
      done;
      Printf.fprintf sout "%d %d %f\n" (!ta) (!max) ((float_of_int (!aver)) /. (float_of_int (nb_tree)));
      flush_all ();
      ta := !ta + pas;
    end
  done;
  close_out sout;;

(* Fonctions mettant en concurrence les précédants algorithmes *)
let test_egalite nb_tree entree_max taille_max_arbre =
  let continue = ref true in
  let compt = ref 0 in
  let ta = ref ( 2 + Random.int (taille_max_arbre -2)) in  
  let tree = ref (cree_alea (!ta) (entree_max)) in
  let cex = ref (!tree) in
  let nb_cex = ref 0 in
  let sature = ref [] in
  while (!continue) do
    incr compt;
    flush_all ();
    let ma,i = algo_intelligent (!tree) (!ta/2) in
    let mb,c = brute_force (!tree) (!ta/2) in
    print_int (!compt);
    print_newline ();
    if (mb = ma) then () else (
      let mc,e = algo_intelligent_5 (!tree) (!ta/2) 0 (false,0,0) in
      if mc <> mb then (
	continue := false;
	print_arbre_latex (!tree);
	print_arbre_v (!tree);
	cex := !tree;
      )
    );
    ta := ( 2 + Random.int (taille_max_arbre -2));
    tree := cree_alea_binaire (!ta) (entree_max);
  done;
  (!continue,!cex,!nb_cex);;

let t = test_egalite 0 150 25;;
let test = Node([Node([Node([Node([Node([Node([],82);],83);Node([],56);],17);Node([],43);],87);Node([],96);],87);Node([Node([Node([Node([],79);Node([],23);],4);Node([],61);Node([Node([],9);],22);Node([Node([],44);],83);],96);],51);Node([],84);],66);;
brute_force test 9;;
algo_intelligent_4 test 9 0 (false,0,0);;
let cex2 = Node([Node([Node([Node([],11);Node([],3);],1);Node([Node([Node([],18);Node([],7);],0);Node([],5);],2);],6);Node([Node([Node([],10);],0);Node([],17);],15);],13);;
algo_intelligent_bis cex2 7;;
n_brute_force cex2 7;;
let n26,cex26 = brute_force cex2 6;;
print_arbre_latex cex26;;
print_arbre_latex cex2;;
let n27b,cex27b = algo_intelligent cex2 7;;
let n27g,cex27g = n_brute_force cex2 7;;
print_arbre_latex cex27g;;

let cex3 = Node([Node([Node([Node([Node([Node([],8);Node([Node([Node([],18);Node([],11);],12);],9);],4);Node([],5);],6);Node([Node([],15);],2);],15);Node([Node([Node([],1);],5);],10);],19);Node([Node([],10);],12);],3);;
n_brute_force cex3 9;;
algo_intelligent_ter cex3 9 0;;
let n38,cex38 = brute_force cex3 8;;
print_arbre_latex cex38;;
let n39b,cex39b = algo_intelligent_ter cex3 9 0;;
print_arbre_latex cex39b;;
let n39g,cex39g = brute_force cex3 9 ;;
print_arbre_latex cex39g;;

let cex4 = Node([Node([Node([Node([Node([],16);],2);Node([Node([Node([Node([],1);],5);],19);Node([Node([Node([],16);Node([],4);],2);Node([],10);],9);],6);],4);Node([Node([],7);],9);],14);Node([],15);],6);;
let n48g,cex48g = n_brute_force cex4 8;;
let n48b,cex48b = algo_intelligent_4 cex4 8 0 (false,0,0);;
let n47,cex47 = algo_intelligent_4 cex4 7 0 (false,0,0);;
let n47,cex47 = brute_force cex4 7;;
print_arbre_latex cex48g;;

let cex5 = Node
    ([Node
       ([Node ([], 141);
         Node
          ([Node
             ([Node ([Node ([], 145); Node ([], 6)], 132);
               Node
                ([Node ([Node ([Node ([], 100)], 64)], 105); Node ([], 62)],
                 120)],
              28);
            Node ([Node ([Node ([], 89)], 89); Node ([], 98)], 90)],
           125)],
        91);
      Node ([Node ([], 118); Node ([], 101)], 140)],
     123);;
algo_intelligent_5 cex5 10 0 (false,0,0);;
brute_force cex5 10;;
algo_intelligent_6 cex5 10;;
let n59,cex59 = brute_force cex5 9;;
let n510g,cex510g = brute_force cex5 10;;
let n510b,cex510b = algo_intelligent_5 cex5 10 0 (false,0,0);;
print_arbre_latex cex510g;;

let cex6 = Node([Node([Node([],11);Node([],10);Node([Node([Node([Node([],4)],10);Node([],8)],12)],14);Node([],12)],18);Node([Node([Node([],17);Node([],18)],2);Node([],17)],9);Node([Node([],16);Node([],1)],14)],5);;
let n610,cex610 = n_brute_force cex6 10;;
let n611g,cex611g = n_brute_force cex6 11;;
let n611b,cex611b = algo_intelligent_6 cex6 11;;
print_arbre_latex cex611g;;

let satur nb_tree entree_max taille_max_arbre = (* trouve un arbre qui ne verifie pas l'inclusions des serveurs saturés dans tous les arbres suivants *)
  let continue = ref true in
  let compt = ref 0 in
  let ta = ref (2+ Random.int (taille_max_arbre -2)) in  
  let tree = ref (cree_alea (!ta) (entree_max)) in
  print_arbre_v (!tree);print_newline ();
  let cex = ref (!tree) in
  let nb_cex = ref 0 in
  let sature = ref [] in
  while (!continue && !compt < nb_tree) do
    sature := [];
    incr compt;
    print_int (!compt); print_newline ();
    for i = 1 to (!ta) do

      let ma,b = brute_force (!tree) i in
      let e = ens_satur (b) i in
      sature := ajoutel (e) (!sature);
      (*print_string "satur :\n" ; print_l_l e;*)
      let f = ens_pres (b) in
      (*print_string "présent :\n" ; print_l_l f;*)
      if inclus (!sature) (f) then () else (cex := !tree ; nb_cex := i; continue := false)
    done;
    ta := (2+ Random.int (taille_max_arbre -2));
    tree := cree_alea (!ta) (entree_max);
    (*print_arbre_v (!tree);print_newline ()*)
  done;
  (!continue,!cex,!nb_cex);;


let courbe_satur nb_tree entree_max taille_max_arbre = (*trace le nombre moyen et minimal d'éléments saturées*)
  let tab_min = Array.make_matrix 20 20 (max_int,Node([],0)) in
  let tab_moy = Array.make_matrix 20 20 (0.) in
  for p = 1 to 20 do
    let compt = ref 1 in
    while (!compt < nb_tree) do
      let tree = ref (cree_alea (p) (entree_max)) in
      let satur = ref [] in
      for n = 1 to p do
	print_int p ; print_string " " ; print_int (!compt) ; print_string " "; print_int n;print_newline ();
	let ma,b = brute_force (!tree) n in
	satur := ajoutel (ens_satur b n) (!satur);
	let t = List.length (!satur) in
	if t < fst (tab_min.(p-1).(n-1)) then tab_min.(p-1).(n-1) <- (t,!tree);
	let nm = (tab_moy.(p-1).(n-1) *. (float_of_int (!compt -1)) +. (float_of_int t)) /. (float_of_int (!compt)) in
	tab_moy.(p-1).(n-1) <- nm
      done;
      incr compt
    done
  done;
  tab_min,tab_moy;;

let n0_adress t =
  let rec tot tree = match tree with
    |Node(fl,ent) -> ent + apply_tot fl
    |Server(fl,n,ent) -> ent + apply_tot fl
  and apply_tot fl = match fl with
    |p::q -> tot p + apply_tot q
    |[] -> 0
  in
  let w = tot t in
  let rep = ref [] in
  let rec aux t adr = match t with
    |Node(_,_) -> ()
    |Server(fl,a,e) -> rep := (List.rev adr,int_of_float ( (float_of_int w)/.(float_of_int a.w))) :: (!rep) ; apply_aux fl adr 0
  and apply_aux fl adr i = match fl with
    |p::q -> aux p (i::adr) ; apply_aux q adr (i+1)
    |[] -> ()
  in
  aux t [];
  !rep;;

let verif_n0 nb_tree entree_max taille_max_arbre =
  let compt = ref 0 in 
  let continue = ref true in
  let cex = ref (Node([],1)) in
  let rep = ref (!continue,!cex,[],0) in
  while (!compt < nb_tree && (!continue)) do
     incr compt;
    let ta = ref (cree_alea (taille_max_arbre) (entree_max)) in
    print_arbre_v (!ta);
    let expected = Array.make (taille_max_arbre) [] in
    print_int (!compt); print_newline ();
 
    for i = 1 to (taille_max_arbre-1) do
      let ma,b = brute_force (!ta) i in
      let usl = opt (tarbre_to_arbre b) in
      let l = n0_adress b in
      let rec aux l = match l with
	|(a,b)::q ->
	  begin
	    if b <> 0 then 
	      for j = (b-1) to (taille_max_arbre -1 ) do 
		expected.(j) <- ajoute a (expected.(j)) 
	      done;
	    aux q;
	  end
	|[] -> ()
      in
      aux l;
      if (not (inclus (expected.(i-1)) (ens_pres b))) then
	begin
	  continue := false;
	  let l2 = not_incl expected.(i-1) (ens_pres b) in
	  rep := (false,!ta,l2,i);
	end;
    done;
  done;
  !rep;;


let out_apres nb_tree entree_max taille_max_arbre= (*trouve des éléments qui ne vérifient pas l'inclusion dans l'étape suivante*)
  let rep  = Array.make (taille_max_arbre) (0,Node([],0)) in
  for i = 0 to (nb_tree -1) do
    let tree = ref (cree_alea (taille_max_arbre) entree_max) in
    let aux = ref [] in
    for j = 1 to (taille_max_arbre) do
      Printf.fprintf stdout ("i : %d , j : %d %d ") i j nb_tree;
      print_newline ();
      let (ma,b) = n_brute_force (!tree) j in
      let e = ens_pres (b) in
      let f = not_incl (!aux) e in
      let nf = List.length f in
      if nf > (fst (rep.(j-1))) then (rep.(j-1) <- (nf,!tree); print_int nf ; print_newline ()) ; 
      aux := e
    done;

    print_ar rep;
  done;
  print_ar rep;;

print_string "test L2250";; print_newline ();;
let main () =
  print_string "debut\n"; print_newline ();
  test_egalite (int_of_string (Sys.argv.(1))) (int_of_string (Sys.argv.(2))) (int_of_string (Sys.argv.(3)));;

print_string "test L2256";; print_newline ();;
(* \Fonctions avec élements saturés *)

let comparaison_courbe_opt_alea nb_tree entree_max taille_max_arbre bin =
  print_int 0;
  let rep = Array.make_matrix 7 taille_max_arbre 0. in
  for i = 0 to (nb_tree -1) do
    print_int i; print_newline ();
    let tree = ref (cree_alea (taille_max_arbre) entree_max) in
    for j = 1 to (taille_max_arbre) do
      let m1,_ = algo_intelligent !tree j in
      let m2,_ = algo_intelligent_bis !tree j in 
      let m3,_ = algo_intelligent_ter !tree j 0 in
      let m4,_ = algo_intelligent_4 !tree j 0 (false,0,0) in
      let m5,_ = algo_intelligent_5 !tree j 0 (false,0,0) in
      let m6,_ = algo_intelligent_6 !tree j in
      let mo,_ = brute_force !tree j in
      let r1 = (float_of_int (mo) /. float_of_int (m1)) in
      let r2 = (float_of_int (mo) /. float_of_int (m2)) in
      let r3 = (float_of_int (mo) /. float_of_int (m3)) in
      let r4 = (float_of_int (mo) /. float_of_int (m4)) in
      let r5 = (float_of_int (mo) /. float_of_int (m5)) in
      let r6 = (float_of_int (mo) /. float_of_int (m6)) in
      rep.(0).(j-1) <- rep.(0).(j-1) +. 1.;
      rep.(1).(j-1) <- rep.(1).(j-1) +. r1;
      rep.(2).(j-1) <- rep.(2).(j-1) +. r2;
      rep.(3).(j-1) <- rep.(3).(j-1) +. r3;
      rep.(4).(j-1) <- rep.(4).(j-1) +. r4;
      rep.(5).(j-1) <- rep.(5).(j-1) +. r5;
      rep.(6).(j-1) <- rep.(6).(j-1) +. r6;
    done;
  done;
  for i = 0 to 6 do
    for j = 1 to (taille_max_arbre) do
      rep.(i).(j-1) <- rep.(i).(j-1) /. (float_of_int nb_tree)
    done
  done;
  let out = open_out ("comp_courbe_opt.txt") in
  for j = 0 to (taille_max_arbre-1) do
    Printf.fprintf out ("%d %f %f %f %f %f %f %f\n") (j+1) (rep.(0).(j)) (rep.(1).(j)) (rep.(2).(j)) (rep.(3).(j)) (rep.(4).(j)) (rep.(5).(j)) (rep.(6).(j))
  done;
  close_out out;;


let rec is_binaire t = match t with
  |Node(fl,_) -> (List.length fl <= 2) && (apply_ib fl)
  |Server(fl,_,_) -> (List.length fl <= 2) && (apply_ib fl)
and apply_ib fl = match fl with
  |[] -> true
  |p::q -> is_binaire p && apply_ib (q);;

let compte_liste () = 
  let a = open_in "resbinaire" in
  let continue = ref true in
  let rep = Array.make 30 0 in
  let aux = Array.make 30 [] in
  while (!continue) do
    try
      begin
	let s = input_line a in
	let t = string_to_tarbre s in
	if (is_binaire t) then print_int 1 else print_int 0 ; print_newline ();
	let n = taille_tree (t) in
	rep.(n) <- rep.(n) + 1;
	aux.(n) <- t :: (aux.(n))
      end
    with
      |End_of_file -> continue := false
  done;
  close_in a;
rep,aux;;
let comparaison_courbe_opt nb_tree entree_max taille=
  print_int 0;
  let rep = Array.make_matrix 8 taille 10. in
  let a,cl = compte_liste () in
  let at,clt = a.(taille),cl.(taille) in
  let rec test li i= match li with
    |[] -> ()
    |tree::q -> 
      begin
	print_string "This is iteration ";
	print_int i ;
	print_string " / ";
	print_int a.(taille);
	print_string ".";
	print_newline ();
	for j = 1 to (taille) do
	  print_int i ;print_string " / ";print_int a.(taille);print_string "\tSize : " ; print_int j ; print_string " / " ; print_int taille ; print_string "." ; print_newline ();
	  let m1,_ = algo_intelligent tree j in
	  print_int i ;print_string " / ";print_int a.(taille);print_string "\t\tG1 : done"; print_newline ();
	  let m2,_ = algo_intelligent_bis tree j in 
	  print_int i ;print_string " / ";print_int a.(taille);print_string "\t\tG2 : done"; print_newline ();
	  let m3,_ = algo_intelligent_ter tree j 0 in
	  print_int i ;print_string " / ";print_int a.(taille);print_string "\t\tG3 : done"; print_newline ();
	  let m4,_ = algo_intelligent_4 tree j 0 (false,0,0) in
	  print_int i ;print_string " / ";print_int a.(taille);print_string "\t\tG4 : done"; print_newline ();
	  let m5,_ = algo_intelligent_5 tree j 0 (false,0,0) in
	  print_int i ;print_string " / ";print_int a.(taille);print_string "\t\tG5 : done"; print_newline ();
	  let m6,_ = algo_intelligent_6 tree j in
	  print_int i ;print_string " / ";print_int a.(taille);print_string "\t\tG6 : done"; print_newline ();
	  let mo,_ = brute_force tree j in
	  print_int i ;print_string " / ";print_int a.(taille);print_string "\t\tOpt : done"; print_newline ();
	  let m7 = mo in (*algo_intelligent_7 tree j in*)
	  print_int i ;print_string " / ";print_int a.(taille);print_string "\t\tG7 : done"; print_newline ();
	  let r1 = (float_of_int (mo) /. float_of_int (m1)) in
	  let r2 = (float_of_int (mo) /. float_of_int (m2)) in
	  let r3 = (float_of_int (mo) /. float_of_int (m3)) in
	  let r4 = (float_of_int (mo) /. float_of_int (m4)) in
	  let r5 = (float_of_int (mo) /. float_of_int (m5)) in
	  let r6 = (float_of_int (mo) /. float_of_int (m6)) in
	  let r7 = (float_of_int (mo) /. float_of_int (m7)) in
	  rep.(1).(j-1) <- min rep.(1).(j-1) r1;
	  rep.(2).(j-1) <- min rep.(2).(j-1) r2;
	  rep.(3).(j-1) <- min rep.(3).(j-1) r3;
	  rep.(4).(j-1) <- min rep.(4).(j-1) r4;
	  rep.(5).(j-1) <- min rep.(5).(j-1) r5;
	  rep.(6).(j-1) <- min rep.(6).(j-1) r6;
	  rep.(7).(j-1) <- min rep.(7).(j-1) r7;
	done;
	test q (i+1);
      end
  in
  test clt 0;
  (*for i = 0 to 7 do
    for j = 1 to (taille) do
      rep.(i).(j-1) <- rep.(i).(j-1) /. (float_of_int at)
    done
  done;*);;

let rec minl l m = match l with
  |p::q -> if (snd p) < m then minl q (snd p) else minl q m
  |[] -> m;;
let best l =
  let m = minl l max_int in
  let rep = ref [] in
  let rec auxi li = match li with
    |(a,b)::q -> if b = m then rep := a :: !(rep) ; auxi q
    |[] -> ()
  in
  auxi l;
  !rep,m;;
let max2 a b = if a < b then b else a;;
let soi = string_of_int;;

let mesure_grande_valeur nb_tree deb fin pas entree = 
  let cur = ref deb in
  let nb_ex = (fin - deb) / pas + 1 in
  let tmoy = Array.make_matrix 7 nb_ex 0. in
  let tmax = Array.make_matrix 7 nb_ex 0. in
  
  let ecart_to_best_moy = Array.make_matrix 7 nb_ex 0. in
  let ecart_to_best_max = Array.make_matrix 7 nb_ex 0. in
  
  let prop_best = Array.make_matrix 7 nb_ex 0. in
  
  let res_string_tmoy = "res_big_values_nbtree="^(string_of_int nb_tree)^"beg="^(string_of_int deb)^"end="^(string_of_int fin)^"step="^(string_of_int pas)^"max"^(string_of_int entree)^"tmoy.txt" in
  let res_string_tmax = "res_big_values_nbtree="^(string_of_int nb_tree)^"beg="^(string_of_int deb)^"end="^(string_of_int fin)^"step="^(string_of_int pas)^"max"^(string_of_int entree)^"tmax.txt" in
  let res_string_ecamoy = "res_big_values_nbtree="^(string_of_int nb_tree)^"beg="^(string_of_int deb)^"end="^(string_of_int fin)^"step="^(string_of_int pas)^"max"^(string_of_int entree)^"ecamoy.txt" in
  let res_string_ecamax = "res_big_values_nbtree="^(string_of_int nb_tree)^"beg="^(string_of_int deb)^"end="^(string_of_int fin)^"step="^(string_of_int pas)^"max"^(string_of_int entree)^"ecamax.txt" in
  let res_string_propwin = "res_big_values_nbtree="^(string_of_int nb_tree)^"beg="^(string_of_int deb)^"end="^(string_of_int fin)^"step="^(string_of_int pas)^"max"^(string_of_int entree)^"propwin.txt" in


  let outmoy = open_out (res_string_tmoy) in
  let outmax = open_out (res_string_tmax) in 
  let outecamoy = open_out (res_string_ecamoy) in
  let outecamax = open_out (res_string_ecamax) in
  let outpropwin = open_out (res_string_propwin) in
  while (!cur <= fin) do
    begin
      print_string ("TS : "^(soi (!cur))^" / "^ (soi fin)); print_newline ();

      let ind = ((!cur - deb) / pas) in
      for i = 1 to nb_tree do
	let j = (!cur) / 2 in

	let tree = cree_alea (!cur) entree in
	print_string ("TS : "^(soi (!cur))^" / "^ (soi fin) ^ " - NB : " ^ (soi i) ^ " / " ^ (soi nb_tree) ^ " G1 : ");
	let t11 = Sys.time () in
	let m1,_ = algo_intelligent tree j in
	let t12 = Sys.time () in
	print_string ("done") ; print_newline ();
	
	print_string ("TS : "^(soi (!cur))^" / "^ (soi fin) ^ " - NB : " ^ (soi i) ^ " / " ^ (soi nb_tree) ^ " G2 : ");
	let t21 = Sys.time () in
	let m2,_ = algo_intelligent_bis tree j in
	let t22 = Sys.time () in
	print_string ("done") ; print_newline ();
	
	print_string ("TS : "^(soi (!cur))^" / "^ (soi fin) ^ " - NB : " ^ (soi i) ^ " / " ^ (soi nb_tree) ^ " G3 : ");
	let t31 = Sys.time () in
	let m3,_ = max_int,0 (*algo_intelligent_ter tree j 0*) in
	let t32 = Sys.time () in
	print_string ("done") ; print_newline ();

	print_string ("TS : "^(soi (!cur))^" / "^ (soi fin) ^ " - NB : " ^ (soi i) ^ " / " ^ (soi nb_tree) ^ " G4 : ");
	let t41 = Sys.time () in
	let m4,_ =  max_int,0 (*algo_intelligent_4 tree j 0 (false,0,0)*) in
	let t42 = Sys.time () in
	print_string ("done") ; print_newline ();
	
	print_string ("TS : "^(soi (!cur))^" / "^ (soi fin) ^ " - NB : " ^ (soi i) ^ " / " ^ (soi nb_tree) ^ " G5 : ");
	let t51 = Sys.time () in
	let m5,_ =  max_int,0 (*algo_intelligent_5 tree j 0 (false,0,0)*) in
	let t52 = Sys.time () in
	print_string ("done") ; print_newline ();
	
	print_string ("TS : "^(soi (!cur))^" / "^ (soi fin) ^ " - NB : " ^ (soi i) ^ " / " ^ (soi nb_tree) ^ " G6 : ");
	let t61 = Sys.time () in
	let m6,_ =  max_int,0 (*algo_intelligent_6 tree j*) in
	let t62 = Sys.time () in
	print_string ("done") ; print_newline ();
	
	print_string ("TS : "^(soi (!cur))^" / "^ (soi fin) ^ " - NB : " ^ (soi i) ^ " / " ^ (soi nb_tree) ^ " G7 : ");
	let t71 = Sys.time () in
	let m7,_ =  max_int,0 (*algo_intelligent_7 tree j*) in
	let t72 = Sys.time () in
	print_string ("done") ; print_newline ();
	
	
	tmoy.(0).(ind) <- tmoy.(0).(ind) +. t12 -. t11;
	tmax.(0).(ind) <- max2 (tmax.(0).(ind)) (t12 -. t11);
	
	tmoy.(1).(ind) <- tmoy.(1).(ind) +. t22 -. t21;
	tmax.(1).(ind) <- max2 (tmax.(1).(ind)) (t22 -. t21);
	
	tmoy.(2).(ind) <- tmoy.(2).(ind) +. t32 -. t31;
	tmax.(2).(ind) <- max2 (tmax.(2).(ind)) (t32 -. t31);
	
	tmoy.(3).(ind) <- tmoy.(3).(ind) +. t42 -. t41;
	tmax.(3).(ind) <- max2 (tmax.(3).(ind)) (t42 -. t41);
	
	tmoy.(4).(ind) <- tmoy.(4).(ind) +. t52 -. t51;
	tmax.(4).(ind) <- max2 (tmax.(4).(ind)) (t52 -. t51);
	
	tmoy.(5).(ind) <- tmoy.(5).(ind) +. t62 -. t61;
	tmax.(5).(ind) <- max2 (tmax.(5).(ind)) (t62 -. t61);
	
	tmoy.(6).(ind) <- tmoy.(6).(ind) +. t72 -. t71;
	tmax.(6).(ind) <- max2 (tmax.(6).(ind)) (t72 -. t71);
	
	let best,value = best ([(1,m1);(2,m2);(3,m3);(4,m4);(5,m5);(6,m6);(7,m7)]) in
	let rec incr_best l = match l with
	  |p::q -> prop_best.(p-1).(ind) <- prop_best.(p-1).(ind) +. 1. ; incr_best q
	  |[] -> ()
	in
	incr_best best;
	
	ecart_to_best_moy.(0).(ind) <- ecart_to_best_moy.(0).(ind) +.((100. *. float_of_int (m1 - value)) /. (float_of_int value));
	ecart_to_best_max.(0).(ind) <- max2 (ecart_to_best_max.(0).(ind)) ((100. *. float_of_int (m1 - value)) /. (float_of_int value));

	ecart_to_best_moy.(1).(ind) <- ecart_to_best_moy.(1).(ind) +.((100. *. float_of_int (m2 - value)) /. (float_of_int value));
	ecart_to_best_max.(1).(ind) <- max2 (ecart_to_best_max.(1).(ind)) ((100. *. float_of_int (m2 - value)) /. (float_of_int value));

	ecart_to_best_moy.(2).(ind) <- ecart_to_best_moy.(2).(ind) +.((100. *. float_of_int (m3 - value)) /. (float_of_int value));
	ecart_to_best_max.(2).(ind) <- max2 (ecart_to_best_max.(2).(ind)) ((100. *. float_of_int (m3 - value)) /. (float_of_int value));

	ecart_to_best_moy.(3).(ind) <- ecart_to_best_moy.(3).(ind) +.((100. *. float_of_int (m4 - value)) /. (float_of_int value));
	ecart_to_best_max.(3).(ind) <- max2 (ecart_to_best_max.(3).(ind)) ((100. *. float_of_int (m4 - value)) /. (float_of_int value));

	ecart_to_best_moy.(4).(ind) <- ecart_to_best_moy.(4).(ind) +.((100. *. float_of_int (m5 - value)) /. (float_of_int value));
	ecart_to_best_max.(4).(ind) <- max2 (ecart_to_best_max.(4).(ind)) ((100. *. float_of_int (m5 - value)) /. (float_of_int value));

	ecart_to_best_moy.(5).(ind) <- ecart_to_best_moy.(5).(ind) +.((100. *. float_of_int (m6 - value)) /. (float_of_int value));
	ecart_to_best_max.(5).(ind) <- max2 (ecart_to_best_max.(5).(ind)) ((100. *. float_of_int (m6 - value)) /. (float_of_int value));

	ecart_to_best_moy.(6).(ind) <- ecart_to_best_moy.(6).(ind) +.((100. *. float_of_int (m7 - value)) /. (float_of_int value));
	ecart_to_best_max.(6).(ind) <- max2 (ecart_to_best_max.(6).(ind)) ((100. *. float_of_int (m7 - value)) /. (float_of_int value));


      done;
      for i = 0 to 6 do
	ecart_to_best_moy.(i).(ind) <- ecart_to_best_moy.(i).(ind) /. (float_of_int nb_tree) ;
	tmoy.(i).(ind) <- tmoy.(i).(ind) /.(float_of_int nb_tree) ;
	prop_best.(i).(ind) <- prop_best.(i).(ind) /. (float_of_int nb_tree) ;
      done;
    let ind = ((!cur - deb)/pas) in
    Printf.fprintf outmoy ("%d %f %f %f %f %f %f %f\n") (!cur) (tmoy.(0).(ind)) (tmoy.(1).(ind)) (tmoy.(2).(ind)) (tmoy.(3).(ind)) (tmoy.(4).(ind)) (tmoy.(5).(ind)) (tmoy.(6).(ind));
    Printf.fprintf outmax ("%d %f %f %f %f %f %f %f\n") (!cur) (tmax.(0).(ind)) (tmax.(1).(ind)) (tmax.(2).(ind)) (tmax.(3).(ind)) (tmax.(4).(ind)) (tmax.(5).(ind)) (tmax.(6).(ind));
    Printf.fprintf outecamoy ("%d %f %f %f %f %f %f %f\n") (!cur) (ecart_to_best_moy.(0).(ind)) (ecart_to_best_moy.(1).(ind)) (ecart_to_best_moy.(2).(ind)) (ecart_to_best_moy.(3).(ind)) (ecart_to_best_moy.(4).(ind)) (ecart_to_best_moy.(5).(ind)) (ecart_to_best_moy.(6).(ind));
    Printf.fprintf outecamax ("%d %f %f %f %f %f %f %f\n") (!cur) (ecart_to_best_max.(0).(ind)) (ecart_to_best_max.(1).(ind)) (ecart_to_best_max.(2).(ind)) (ecart_to_best_max.(3).(ind)) (ecart_to_best_max.(4).(ind)) (ecart_to_best_max.(5).(ind)) (ecart_to_best_max.(6).(ind));
    Printf.fprintf outpropwin ("%d %f %f %f %f %f %f %f\n") (!cur) (prop_best.(0).(ind)) (prop_best.(1).(ind)) (prop_best.(2).(ind)) (prop_best.(3).(ind)) (prop_best.(4).(ind)) (prop_best.(5).(ind)) (prop_best.(6).(ind));
    flush_all ();
    cur := !cur + pas;
    end
  done;

  close_out outmoy;
  close_out outmax;
  close_out outecamoy;
  close_out outecamax;
  close_out outpropwin;;

(*etude_nb_decr (int_of_string (Sys.argv.(1))) (int_of_string (Sys.argv.(2))) (int_of_string (Sys.argv.(3))) (int_of_string (Sys.argv.(4))) (int_of_string (Sys.argv.(5)));;*)

test_egalite 0 150 50;;
