open Printf
open Def
open Tools

let rec addn l t vmax = (*adds a node in tree t, this nodes is the son of node with address l, with input lower than vmax *)
  match l with
  |[] ->
    begin
      let Node(fl,ent) = t in
      let tfl = List.length fl in
      (Node(fl@([Node([],((float_of_int (Random.int (vmax*100)))/.100.))]),ent),tfl)
    end
  |p::q ->
    begin
      let Node(fl,ent) = t in
      let (de,mi,fi) = coupel fl p in
      let a,b = addn q mi vmax in
      (Node(de@([a])@fi,ent),b)
    end

let cree_alea n valmax = (* creates a random tree with n nodes and input smaller than valmax*)
  let valpossible = ref [[]] in
  let i = ref 0 in
  let arbre_init = ref (Node([],((float_of_int (Random.int (valmax*100)))/.100.))) in
  let add_node () =
    let taille = List.length (!valpossible) in
    let next = Random.int taille in
    let v = List.nth (!valpossible) next in
    let a,b = addn v (!arbre_init) valmax in
    arbre_init := a;
    valpossible :=  !valpossible @ [List.rev (b :: List.rev (v))];
  in
  for k = 0 to (n-2) do
    add_node ();
    incr i;
  done;
  !arbre_init

let rec addn_med l t vmax = (*adds a node in tree t, this nodes is the son of node with address l, with input lower than vmax *)
  match l with
  |[] ->
    begin
      let Node(fl,ent) = t in
      let tfl = List.length fl in
      let new_val = vmax / 3 in
      (Node(fl@([Node([],((float_of_int (Random.int ((vmax - new_val)*100)))/.100. +. float_of_int new_val))]),ent),tfl)
    end
  |p::q ->
    begin
      let Node(fl,ent) = t in
      let (de,mi,fi) = coupel fl p in
      let a,b = addn_med q mi vmax in
      (Node(de@([a])@fi,ent),b)
    end


let cree_med_alea n valmax = (* creates a random tree with n nodes and input smaller than valmax*)
  let valpossible = ref [[]] in
  let i = ref 0 in
  let new_val = valmax / 3 in
  let arbre_init = ref (Node([],((float_of_int (Random.int ((valmax - new_val)*100)))/.100. +. float_of_int new_val))) in
  let add_node () =
    let taille = List.length (!valpossible) in
    let next = Random.int taille in
    let v = List.nth (!valpossible) next in
    let a,b = addn_med v (!arbre_init) valmax in
    arbre_init := a;
    valpossible :=  !valpossible @ [List.rev (b :: List.rev (v))];
  in
  for k = 0 to (n-2) do
    add_node ();
    incr i;
  done;
  !arbre_init



let rec addn_big l t vmax = (*adds a node in tree t, this nodes is the son of node with address l, with input lower than vmax *)
  match l with
  |[] ->
    begin
      let Node(fl,ent) = t in
      let tfl = List.length fl in
      let new_val = vmax / 2 in
      (Node(fl@([Node([],((float_of_int (Random.int (new_val*100)))/.100. +. float_of_int new_val))]),ent),tfl)
    end
  |p::q ->
    begin
      let Node(fl,ent) = t in
      let (de,mi,fi) = coupel fl p in
      let a,b = addn_big q mi vmax in
      (Node(de@([a])@fi,ent),b)
    end


let cree_big_alea n valmax = (* creates a random tree with n nodes and input smaller than valmax*)
  let valpossible = ref [[]] in
  let i = ref 0 in
  let new_val = valmax / 2 in
  let arbre_init = ref (Node([],((float_of_int (Random.int (new_val*100)))/.100. +. float_of_int new_val))) in
  let add_node () =
    let taille = List.length (!valpossible) in
    let next = Random.int taille in
    let v = List.nth (!valpossible) next in
    let a,b = addn_big v (!arbre_init) valmax in
    arbre_init := a;
    valpossible :=  !valpossible @ [List.rev (b :: List.rev (v))];
  in
  for k = 0 to (n-2) do
    add_node ();
    incr i;
  done;
  !arbre_init


let cree_alea_binaire n valmax = (* creates a random binary tree with n nodes and input smaller than valmax*) 
  let valpossible = ref [[]] in
  let i = ref 0 in
  let arbre_init = ref (Node([],((float_of_int (Random.int (valmax*100)))/.100.))) in
  let add_node () =
    let taille = List.length (!valpossible) in
    let next = Random.int taille in
    let v = List.nth (!valpossible) next in
    let a,b = addn v (!arbre_init) valmax in
    if b = 1 then 
      valpossible := sup v !(valpossible);
    arbre_init := a;
    valpossible :=  !valpossible @ [List.rev (b :: List.rev (v))];
  in
  for k = 0 to (n-2) do
    add_node ();
    incr i;
  done;
  !arbre_init

let print_load_tree tree =
  let rec aux_print_load load subtree =
    match subtree with
      | Server _ -> failwith "execute after tree creation"
      | Node([],w) -> w +. load
      | Node(a,w) -> List.fold_left aux_print_load (w +. load) a
  in
  let load = aux_print_load 0. tree in
  printf "load of tree=%f\n" load
