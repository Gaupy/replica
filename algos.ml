open Def
open Print
open Tools


(* Algo that minimizes the energy with Sol(n-1) \subset Sol(n) *)

let algo_intelligent t n = 
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0. in
  let repargmin = ref t in 
  for i = 1 to n do 
    let mini = ref max_float in
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
	let _ = opt b in
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
  let _ = opt (tarbre_to_arbre (!repargmin)) in 
  !repmin,!repargmin

(* /End of algo that minimizes the energy with Sol(n-1) \subset Sol(n) *)


(* Based on the hypothesis H1: let sol1 \in Opt(n), \exists sol2 \in Opt(n+1), st \exists s \in sol1 and sol1 \setminus \{s \} \subset sol2.*)

let algo_gopi t n = (*Starting from an optimal solution with n servers, computes an optimal solution with n+1 servers: forall leaves of the server tree of the solution with n servers: - we remove the leaf, - we add two new servers and compute the solution. We keep the one that minimimzes the energy solution.*)
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0. in
  let repargmin = ref t in
  let compt_decr = ref 0 in

(*initialisation where we put the two first servers in the tree.*)
  for i = 1 to (min n 2) do
    let mini = ref max_float in
    let argmin = ref t in
    let choix = ref [] in
    let potentiel = ref [] in
      accessible := recalc_potentiel (!tree);
      let rec test_acces l = match l with
        |p::q ->
	let t_test,fils = ajout_server (!tree) p p in
          let b = tarbre_to_arbre t_test in
          let _ = opt b in
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
      if i = n then
      (repmin := !mini; 
       repargmin := !argmin);
  done;

  if n>2 then (*the algo only starts when we assign the 3rd server.*)
  begin
    for i = 3 to n do 
    let mini = ref max_float in
    let argmin = ref !tree in
    let choix = ref [] in
    let potentiel = ref [] in

    let backup = ref (copy_tree (!tree)) in (*This backup will be the tree with n-1 elements to which we will remove one of the servers before adding two new servers.*)
    let tree_temp = ref (copy_tree (!tree)) in (*This will be the temporary best solution found to which we compare the new solution.*)

    let continue = ref (i>2) in
    let nb = ref 0 in 
    while (!continue && !nb < 1) do
      incr nb;
      let l = to_move (!argmin) in
      let mini = ref (max_float) in
      let rec applique li_to_move = 
       match li_to_move with
        |[] -> ()
        |p::q ->
          begin
            let t_test = remove_server (!backup) p in
                  let t_test2 = copy_tree t_test in
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
                  applique q
          end
      and test_put tr_test acces = 
      match acces with
        |[] -> ()
        |p::q ->
          begin
            let t_test,fils = ajout_server (tr_test) p p in
            let b = tarbre_to_arbre t_test in
            let _ = opt b  in
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
      if (puisst (!tree)) < (puisst (!tree_temp)) then
        begin
          argmin := !tree;
          tree_temp := !tree;
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
  done
  end;
  let _ = opt (tarbre_to_arbre (!repargmin)) in  
  puisst (!repargmin),!repargmin(*,!compt_decr*)
  
(* \H1 *)


(* Greedy 7 *)
let greedy_7 t n = (*Given a solution to the problem with n servers, adds one server to minimize the power consumption, and then tries to permute every couple of servers to minimize the power consumption *)
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0. in
  let repargmin = ref t in

  for i = 1 to n do
    let mini = ref max_float in
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
	let _ = opt b in
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
      let mini = ref (max_float) in
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
	    let _ = opt b  in
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

  let _ = opt (tarbre_to_arbre (!repargmin)) in  
  puisst (!repargmin),!repargmin

(* \Greedy 7 *)



(* Greedy 8 *)
let add_k (mini,argmin) tree k = 
  let rec add_k_aux (mini,argmin) arbre j  =
    match j with
    | 0 -> 
      let b = tarbre_to_arbre arbre in
      let _ = opt b in
      let c = puiss b in
      if c < mini then ((c,arbre))
      		else ((mini, argmin))
    | _ -> 
      let accessible_list = recalc_potentiel arbre in
      let tata x y = 
        let t_test,_  = ajout_server arbre y y in
        add_k_aux x t_test (j-1) 
      in
      let a = List.fold_left tata (mini,argmin) accessible_list in
      a
  in add_k_aux (mini,argmin) tree k


let permute_k tree k = 
  let rec remove_k_aux (mini, argmin) arbre j =
    match j with 
    | 0 -> add_k (mini,argmin) arbre k
    | _ -> 
      let movable_list = to_move arbre in
      let tata x y = 
(*        let min,_ = x in*)
(*        if j = 3 then Printf.printf "valeur %d\n" min;*)
        let t_test = remove_server arbre y in
        remove_k_aux x t_test (j-1) 
      in
      List.fold_left tata (mini, argmin) movable_list
  in remove_k_aux (max_float,tree) tree k


let greedy_8 t n k = (*Given a solution to the problem with n servers, adds one server to minimize the power consumption, and then tries to permute every k-uple of servers to minimize the power consumption *)
(*  let accessible = ref [[]] in*)
  let tree = ref t in
  let repmin = ref 0. in
  let repargmin = ref t in

  for i = 1 to n do
    let mini = ref max_float in
    let argmin = ref t in
(*    let choix = ref [] in*)
(*    let potentiel = ref [] in*)

    let energy,t_tree = add_k (!mini,!argmin) !tree 1 in
    mini := energy;
    argmin:= t_tree;
    tree := !argmin;
    let backup = ref (copy_tree (!tree)) in

    let continue = ref (i > k) in
    let nb = ref 0 in 
    while (!continue && !nb < 1) do
      incr nb;
      let energy,t_tree = permute_k !backup k in
      tree := t_tree ;
      if (energy < !mini) then
	begin
	  argmin := t_tree;
	  backup := t_tree;
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

  let _ = opt (tarbre_to_arbre (!repargmin)) in  
  puisst (!repargmin),!repargmin
  
(* \Greedy 8 *)



(* Greedy 9 *)
let greedy_9 t n = (*Given a solution to the problem with n servers, adds one server to minimize the power consumption, and then tries to permute every couple of servers to minimize the power consumption *)
  let accessible = ref [[]] in
  let tree = ref t in
  let repmin = ref 0. in
  let repargmin = ref t in

  for i = 1 to n do
    let mini = ref (max_float) in
    let argmin = ref t in
    let choix = ref [] in
    let potentiel = ref [] in

let energy,t_tree = add_k (!mini,!argmin) !tree 1 in
    mini := energy;
    argmin:= t_tree;
    tree := !argmin;

    let backup = ref (copy_tree (!tree)) in

    let continue = ref (i>2) in
    let nb = ref 0 in 
    while (!continue && !nb < 2) do
      incr nb;
      accessible := recalc_potentiel (!tree);
      let l = to_move (!argmin) in
      let mini = ref (max_float) in
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
	    let _ = opt b  in
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

  let _ = opt (tarbre_to_arbre (!repargmin)) in  
  puisst (!repargmin),!repargmin

(* \Greedy 9 *)
