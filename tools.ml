open Pervasives
open Def
open Print
open Printf




let rec appart elem l = match l with (* test whether elem \in l *)
  |[] -> false
  |p::q -> (p = elem) || appart elem q

let rec inclus a b = match a with (*test whether the list of elements a is included in list b*)
  |p::q -> (appart p b) && (inclus q b)
  |[] -> true

let rec remove elem l = match l with (*enleve elem de la liste l *)
  |p::q ->
    if p = elem then q
    else (p::(remove elem q))
  |[] -> failwith "not in the list";;

let rec remove_tt elem l = match l with (*removes elem from list l *)
  |p::q ->
    if p = elem then (remove_tt elem q)
    else (p::(remove_tt elem q))
  |[] -> []

let rec remove_l lelem l = match lelem with
  |p::q -> 
    remove_l (q) (remove_tt p l)
  |[] -> l

let rec ajoute a l = (*ajoute a à la liste s'il n'y ait pas déjà*)
  if appart a l then l else (a::l)

let rec ajoutel l1 l2 = match l1 with (*de même avec une liste*)
  |p::q -> ajoute (p) (ajoutel q l2)
  |[] -> l2


let print_array_float ar = (*imprime une matrice floatante*)
  let n = Array.length ar in
  let p = Array.length (ar.(0)) in
  for i = 0 to (p-1) do
    print_int (i+1) ; print_string " ";
    for j = 0 to (n-1) do
      print_float (ar.(j).(i)) ; print_string " ";
    done;
    print_newline ()
  done

let coupel l p = (*cuts a list l around the pth element (beginning, pth, end), the first element is at position 0*)
  let debut = ref [] in
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
  cut l p

let rec insert (c,n) l = match l with (* Inserts an element into a sorted (not increasing) list*)
  |(a,b)::q -> 
    if (a > c) then 
      ((a,b)::(insert (c,n) q))
    else
      ((c,n)::(a,b)::q)
  |[] -> [(c,n)]

let rec sup elem l = match l with (* removes every occurence of elem from l*)
  |p::q -> if p = elem then (sup elem q) else p::(sup elem q)
  |[] -> []
  
let rec minsert l1 l2 = match l1 with
  |[] -> l2
  |p::q -> insert p (minsert q l2)

let rec app f l = match l with
  |[] -> []
  |p::q -> minsert (f p) (app f q)

let rec get_entree t = (* given a Tree of Node, sums every input*)
  match t with
    |Node(fl,ent) -> ent + ap_get_entree fl
    |Server(_,_,_) -> failwith "Server not at his place"
and ap_get_entree l = 
  match l with
    |[] -> 0
    |p::q -> get_entree p + (ap_get_entree q)

let rec tarbre_to_arbre t = (* transforms a tarbre in arbre *)
  match t with 
    |Node(fl,ent) -> failwith "node reached 2"
    |Server(fl,n,ent) -> 
      let a,b = ap_ta_to_t fl in
      Noeud(a,n,ent+b)
and ap_ta_to_t l = 
  match l with
    |[] -> [],0
    |p::q ->
      begin
        let a,b = ap_ta_to_t q in
        match p with
          |Node(fl,ent) -> (a,b+ get_entree (p))
          |Server(fl,_,ent) -> ((tarbre_to_arbre p)::a,b)
      end

let rec cree_tarbre tarbre l adress= match tarbre with (* Allocates servers to the list of addresses l*)
  |Node(fl,ent) -> 
    if appart adress l then
      Server(ap_cree_tarbre fl l adress 0,{n = 0 ; w = 0},ent)
    else
      Node(fl,ent)
      |Server(_,_,_) -> failwith "server at wrong place"
and ap_cree_tarbre li l adress i = match li with
  |[] -> []
  |p::q -> (cree_tarbre p l (i::adress)) :: (ap_cree_tarbre q l adress (i+1))

let rec taille_tarbre t = match t with
  |Node(fl,ent) -> 1 + ap fl
  |Server(fl,_,_) -> 1 + ap fl
and  ap l = match l with
  |[] -> 0
  |p::q -> taille_tarbre p + ap q


let rec taille_tree t = match t with
  |Server(fl,_,_) -> 1 + apply_tt fl
  |Node(fl,_) -> 1 + apply_tt fl
and apply_tt l = match l with
  |p::q -> taille_tree p + apply_tt q
  |[] -> 0

  
let rec taille_subtree t adr = match adr with
  |[] -> taille_tree t
  |p::q -> 
    begin
      match t with
	|Server(fl,ent,a) -> taille_subtree (List.nth fl p) (q)
	|Node(fl,ent) -> taille_subtree (List.nth fl p) (q)
    end




let recalc_potentiel t = (*Returns the list of address of potential new spots to put a server (nodes that are the sons of servers.) *)
  let rep = ref [] in
  let rec aux tr adr= match tr with
    |Node(_,_) -> rep := (List.rev adr) :: !(rep)
    |Server(fl,a,e) -> apply_aux fl adr 0
  and apply_aux fl adr i = match fl with
    |p::q -> aux p (i::adr) ; apply_aux q adr (i+1)
    |[] -> ()
  in
  aux t [];
  !rep


let rec copy_tree tr = (* makes a copy of tr (type server tree)*)
  match tr with 
    |Node(a,b) -> Node(apply_copy_tree a,b)
    |Server(a,no,b) -> Server(apply_copy_tree a,{n = no.n ; w = no.w},b)
and apply_copy_tree l = 
  match l with
    |p::q -> (copy_tree p) :: (apply_copy_tree q)
    |[] -> []

let adresse_fils ad n = (*returns the address of all sons of element located at address ad *)
  if n = 0 then []
  else
    let rep = ref [] in
    for i = 0 to (n-1) do
      rep := (List.rev (i::(List.rev ad))):: (!rep)
    done;
    !rep


let rec ajout_server t ad adinit = (* rajoute un serveur dans t à l'adresse ad et renvoit l'adresse de tous ses fils *)
  match ad with 
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
      end

let lpop l = 
  l := List.tl !l

let max_elements_of_list l = (*for a list of pairs sorted non-decreasingly according to the left element of each pairs, returns the list of pairs with the largest left element*) 
  let aux = ref 0 in
  if !l = [] then (failwith "") else (aux := fst (List.hd !l));
  let rep = ref [] in
  let rec maxs li = match li with
    |(a,b)::q -> 
      if (a= !aux) then (rep := (a,b) :: !(rep) ; lpop l ; maxs q)
    |[] -> ()
  in
  maxs !l;
  rep,aux

let rec addk k l =  (*substract 1 to the first k element of list l*)
  match l with
    |[] -> []
    |(a,b)::q ->
      if (k = 0) then (a,b)::q else ( 
        b.w <- b.w - 1;
        (a-1,b)::(addk (k-1) q)
      )

(*let minf a b = if a < b then a else b*)


let rec opt tr = match tr with (* fonction qui calcule la répartition optimale des charges *)
  |Noeud([],n,entree) -> n.w <- entree ; insert (entree,n) []
  |Noeud(l,n,entree) ->
    n.w <- entree ;
    let long = app opt l in
    let lref = ref long in
    let lmax,maxl = max_elements_of_list lref in
    let deux = 
       ref (if (!lref) <> [] then (fst (List.hd (!lref))) else (-1))
    in
    (*print_newline ();*)
    while (!maxl > n.w) do
      let tl = List.length (!lmax) in
      (*print_int tl ; print_newline();*)
      let diff = min (float_of_int (!maxl - !deux)) ((float_of_int (!maxl - n.w)) /. ((float_of_int tl) +. 1.)) in
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
      let lmaxb,maxlb = max_elements_of_list lref in
      lmax := !lmaxb;
      maxl := !maxlb;
      deux := if (!lref) <> [] then (fst (List.hd (!lref))) else (-1);
    done;
    lref := minsert (!lmax) (!lref);
    insert (n.w,n) (!lref)

let rec puiss arbre =  (* returns the power of a tree (arbre)*)
  match arbre with
    |Noeud(fl,n,ent) -> 
      let a = n.w in
      a*a*a + ap_puiss fl
and ap_puiss l = 
  match l with
    |[] -> 0
    |p::q -> puiss p + (ap_puiss q)

let rec puisst arbre = match arbre with (* returns the power of a tarbre*)
  |Server(fl,n,ent) -> 
    let a = n.w in
    a*a*a + ap_puisst fl
  |Node(fl,ent) -> 
    ap_puisst fl
and ap_puisst l = match l with
  |[] -> 0
  |p::q -> puisst p + (ap_puisst q)
    
    
let rec no_son_server t = match t with
  |Server(fl,_,_) -> apply_nss fl
  |Node(fl,_) -> apply_nss fl
and apply_nss l = match l with (*Checks that the sons of a given node (or server) are not servers.*)
  |Server(_,_,_)::q -> false
  |Node(_,_)::q -> apply_nss q
  |[] -> true



let to_move t = (*Returns the list of address of the servers that can be removed (those that do not have any server-sons)*)
  let rep = ref [] in
  let rec aux t adr = match t with
    |Server(fl,a,ent) -> if apply_nss fl then (rep := (List.rev adr) :: (!rep)) else apply_aux fl adr 0
    |Node(_,_) -> ()
  and apply_aux l adr i = match l with
    |p::q -> aux p (i :: adr) ; apply_aux q adr (i+1)
    |[] -> ()
  in
  aux t [];
  !rep


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
  ote t adr


let rec opt_int tr = match tr with (* fonction qui calcule la répartition optimale des charges *)
  |Noeud([],n,entree) -> n.w <- entree ; insert (entree,n) []
  |Noeud(l,n,entree) ->
    n.w <- entree ;
    let long = app opt_int l in
    let lref = ref long in
    let lmax,maxl = max_elements_of_list lref in
    let deux = 
       ref (if (!lref) <> [] then (fst (List.hd (!lref))) else (-1))
    in
    (*print_newline ();*)
    while (!maxl > n.w) do
      let tl = List.length (!lmax) in
      (*print_int tl ; print_newline();*)
      let diff = min (float_of_int (!maxl - !deux)) ((float_of_int (!maxl - n.w)) /. ((float_of_int tl) +. 1.)) in
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
      let lmaxb,maxlb = max_elements_of_list lref in
      lmax := !lmaxb;
      maxl := !maxlb;
      deux := if (!lref) <> [] then (fst (List.hd (!lref))) else (-1);
    done;
    lref := minsert (!lmax) (!lref);
    insert (n.w,n) (!lref)
  

exception OverloadedNode of (node*int)


let rec tarbre_to_arbre_int t serv_table= (* transforms a tarbre in tarbre_int *)
  match t with 
    |Node(fl,ent) -> failwith "node reached 1"
    |Server(fl,n,ent) -> 
      let a = ap_ta_to_t fl serv_table in
      let i = ref 0 in
      let size = Array.length serv_table in
      while serv_table.(!i) < n.w do incr i; if !i=size then raise (OverloadedNode (n,n.w)) done;
      ServerInt(a,n,{s = !i; load = n.w})
and ap_ta_to_t l serv_table= 
  match l with
    |[] -> []
    |p::q ->
      begin
        let a = ap_ta_to_t q serv_table in
        match p with
          |Node(fl,ent) -> a
          |Server(fl,_,ent) -> (tarbre_to_arbre_int p serv_table)::a
      end


let energy_int arbre_int serv_table=
  let rec aux_energy_int energy tree =
    match tree with
      |ServerInt([],_,{s = i; load = _}) ->  energy + serv_table.(i)*serv_table.(i)*serv_table.(i)
      |ServerInt(a,_,{s = i; load = _}) -> (List.fold_left aux_energy_int (energy + serv_table.(i)*serv_table.(i)*serv_table.(i)) a)
  in aux_energy_int 0 arbre_int

let rec insert_new_sort subtree_int list_of_tree_int = (*For the discrete case, the first sort for the sons of a Server following:  (i>i' || (i=i' && l<l')) *)
   match list_of_tree_int with 
     | [] -> [subtree_int]
     | ServerInt(a,b,{s=i;load = l})::q ->
       match subtree_int with
         |ServerInt(_,_,{s=i';load=l'}) -> 
           if (i>i' || (i=i' && l<l')) then ServerInt(a,b,{s=i;load = l})::(insert_new_sort subtree_int q)
           else subtree_int::list_of_tree_int

let list_of_insert id =
  match id with
    | _ -> insert_new_sort


let rec new_sort id l1 l2 = match l1 with
  |[] -> l2
  |p::q -> list_of_insert id p (new_sort id q l2)


let rec slight_optim_discret id rev_usl_list_sons list_of_sons  serv_table remaining_space i= 
(* the list of sons of a node sorted according to new_sort is ((List.rev rev_usl_list_sons) @@ list_of_sons) *)
 match list_of_sons with
  | [] -> (List.rev rev_usl_list_sons, false, remaining_space, i)
  | ServerInt(b,m,{s = 0; load = l'}) :: q ->           
          let new_rev_usl = ServerInt(b,m,{s = 0; load = l'}) :: rev_usl_list_sons in
          slight_optim_discret id new_rev_usl q  serv_table remaining_space i
  | ServerInt(b,m,{s = i'; load = l'}) :: q -> 
      if (l' - serv_table.(i'-1)) <= remaining_space then (*First can we do something without impacting the overall energy (no mode change)*)
        begin
          let temp = new_sort id [ServerInt(b,m,{s = i'-1; load = serv_table.(i'-1)})] q in
          let sorted_sons = List.rev_append rev_usl_list_sons temp in
          (sorted_sons, true, (remaining_space - (l' - serv_table.(i'-1))), i)
        end
      else 
      if i+1 = (Array.length serv_table)  then (*If we cannot increase the speed of the father then do nothing for this son*)
        let new_rev_usl = ServerInt(b,m,{s = i'; load = l'}) :: rev_usl_list_sons in
        slight_optim_discret id new_rev_usl q  serv_table remaining_space i
      else 
        let new_energy = serv_table.(i+1)*serv_table.(i+1)*serv_table.(i+1) + serv_table.(i'-1)*serv_table.(i'-1)*serv_table.(i'-1) in
        let old_energy = serv_table.(i)*serv_table.(i)*serv_table.(i) + serv_table.(i')*serv_table.(i')*serv_table.(i') in
        if (new_energy > old_energy ) then (*Is it worth it to increase the speed of the father to decrease the speed of the son*)
          let new_rev_usl = ServerInt(b,m,{s = i'; load = l'}) :: rev_usl_list_sons in
          slight_optim_discret id new_rev_usl q  serv_table remaining_space i
        else
          if (l' - serv_table.(i'-1)) <= (remaining_space +serv_table.(i+1) - serv_table.(i)) then (* If there is enough room when we add one mode*)
            let temp = new_sort id [ServerInt(b,m,{s = i'-1; load = serv_table.(i'-1)})] q in
            let sorted_sons = List.rev_append rev_usl_list_sons temp in
            (sorted_sons, true, (remaining_space+serv_table.(i+1) - serv_table.(i) - (l' - serv_table.(i'-1)) ), i+1)
          else
            let new_rev_usl = ServerInt(b,m,{s = i'; load = l'}) :: rev_usl_list_sons in
            slight_optim_discret id new_rev_usl q  serv_table remaining_space i






let rec optim_discret id t serv_table =
  match t with
  |ServerInt([],_,_) -> t
  |ServerInt(a,n,{s = i; load = l}) -> 
    begin
      let sorted_sons = ref (new_sort id a []) in
      let ServerInt(_,_,{s=i';load =_}) = List.hd (!sorted_sons) in
      let cont =ref true in
      let cont_slight = ref true in
      let iref = ref (max i' i) in
      let remaining_space = ref (serv_table.(!iref) - l) in
      while !cont_slight do
      while ( !remaining_space >0 && !cont) do
        match !sorted_sons with
          | ServerInt(b,m,{s = i'; load = l'}) ::q ->
            begin
              if i'=0 then cont := false 
              else
                begin
                  if (l' - serv_table.(i'-1)) <= !remaining_space then
                    (sorted_sons := new_sort id [ServerInt(b,m,{s = i'-1; load = serv_table.(i'-1)})] q;
                    remaining_space := !remaining_space - (l' - serv_table.(i'-1)))
                  else
                    (sorted_sons := new_sort id [ServerInt(b,m,{s = i'; load = (l'-(!remaining_space))})] q;
                    remaining_space :=0)
              end
            end
          | _ -> failwith "list should not be empty"
      done;
      let a,b,c,d = slight_optim_discret id [] !sorted_sons  serv_table !remaining_space !iref in
      sorted_sons := a;
      cont_slight :=b;
      remaining_space := c;
      iref := d;
      cont:=!cont_slight
      done;
      let new_sons = List.map (function x -> optim_discret id x serv_table) !sorted_sons in
      ServerInt(new_sons, n, {s=(!iref); load = serv_table.(!iref)-(!remaining_space)})
    end




