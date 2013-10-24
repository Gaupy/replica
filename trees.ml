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
