open Def
open Const
open Print
open PrinttoC
open Tools
open Trees
open Algos
open AlgosDiscret
open Format
open Printf

let () = Random.self_init ()


let parse_config file =
  let chan = open_in file in
  let rmax = int_of_string (input_line chan) in
  let tree_type = int_of_string (input_line chan) in 
  let size_of_tree = int_of_string (input_line chan) in
  let number_of_speeds = int_of_string (input_line chan) in
  let static = float_of_string (input_line chan) in
  let max_speed = int_of_string (input_line chan) in
  let regularity_speed = int_of_string (input_line chan) in
  let number_of_tests = int_of_string (input_line chan) in
  let expe_number = int_of_string (input_line chan) in
  let () = close_in chan in
  {
  rmax = rmax;
  tree_type = tree_type;
  size_of_tree = size_of_tree;
  number_of_speeds = number_of_speeds;
  static = static;
  max_speed = max_speed;
  regularity_speed = regularity_speed;
  number_of_tests = number_of_tests;
  expe_number = expe_number
  }


let script config_file =
  let param = parse_config config_file in
  let tab_of_speeds =
    match param.regularity_speed with
      | 1 -> reg_tab_speeds param.number_of_speeds param.max_speed
      | _ -> intel_tab_speeds param.number_of_speeds param.max_speed
    in 
  let energy = {static = param.static; speeds = tab_of_speeds } in 
  match param.expe_number with
    | 0 | 1 -> (*  *)
      begin
        for iter = 1 to (param.number_of_tests) do
          for vertex_number = 1 to (param.size_of_tree) do
            let tree = match param.tree_type with
              | _ -> cree_alea (vertex_number) (param.rmax)
            in 
              let first_matrix = make_prec_matrix_full_tree tree (vertex_number) in
              let number_heur = 3 in
              let energy_min = Array.make number_heur max_float in
              let server_min = Array.make number_heur (-1) in
              let prec_max = Array.make number_heur 0. in
                for j = 1 to (vertex_number) do
                  try 
                    let result_discret_heur0, matrix_heur0 = algo_discret 3 tree (vertex_number) j energy in
                      if result_discret_heur0 < energy_min.(0) then 
                        ( energy_min.(0) <- result_discret_heur0 ; server_min.(0)<- j ; prec_max.(0) <- max (prec_max.(0)) (diff_prec first_matrix matrix_heur0) );
                    let result_discret_heur1, matrix_heur1 = algo_discret 5 tree (vertex_number) j energy in
                      if result_discret_heur1 < energy_min.(1) then 
                        ( energy_min.(1) <- result_discret_heur1; server_min.(1)<- j; prec_max.(1) <- max (prec_max.(1)) (diff_prec first_matrix matrix_heur1) );
                    let result_discret_heur2, matrix_heur2 = algo_discret 7 tree (vertex_number) j energy in
                      if result_discret_heur2 < energy_min.(2) then 
                        ( energy_min.(2) <- result_discret_heur2; server_min.(2)<- j; prec_max.(2) <- max (prec_max.(2)) (diff_prec first_matrix matrix_heur2) );
                  with
                    | OverloadedNode(_,_) -> ()
                done;
                let st = sprintf "results/size=%d_idle=%d_expe=%d_iter=%d.dat" (vertex_number) (int_of_float energy.static) (param.expe_number) iter in 
                let oo = open_out st in
                fprintf oo "%d\n%d\n" param.expe_number number_heur;
                for i= 0 to number_heur -1 do
                  fprintf oo "%d\t%f\t%f\n" server_min.(i) energy_min.(i) prec_max.(i);
                done;
                fprintf oo "%d\t%d\n" param.number_of_speeds vertex_number; 
                print_set_of_speeds energy oo;
                print_square_matrix first_matrix oo;
                close_out oo
          done
        done
      end
    | 2 -> (*  *)
      begin
        let static = ref 0. in
	while !static <= energy.static do
          let new_energy ={static = !static; speeds = energy.speeds} in
          static := !static +. 1000.; (*the steps of the static energy are by 1000.*)
          for iter = 1 to (param.number_of_tests) do
            let vertex_number = (param.size_of_tree) in
            let tree = match param.tree_type with
              | _ -> cree_alea (vertex_number) (param.rmax)
            in 
            let first_matrix = make_prec_matrix_full_tree tree (vertex_number) in
            let number_heur = 3 in
            let energy_min = Array.make number_heur max_float in
            let server_min = Array.make number_heur (-1) in
            let prec_max = Array.make number_heur 0. in
            for j = 1 to (vertex_number) do
              try 
                let result_discret_heur0, matrix_heur0 = algo_discret 3 tree (vertex_number) j new_energy in
                  if result_discret_heur0 < energy_min.(0) then 
                    ( energy_min.(0) <- result_discret_heur0 ; server_min.(0)<- j ; prec_max.(0) <- max (prec_max.(0)) (diff_prec first_matrix matrix_heur0) );
                let result_discret_heur1, matrix_heur1 = algo_discret 5 tree (vertex_number) j energy in
                  if result_discret_heur1 < energy_min.(1) then 
                    ( energy_min.(1) <- result_discret_heur1; server_min.(1)<- j; prec_max.(1) <- max (prec_max.(1)) (diff_prec first_matrix matrix_heur1) );
                let result_discret_heur2, matrix_heur2 = algo_discret 7 tree (vertex_number) j new_energy in
                  if result_discret_heur2 < energy_min.(2) then 
                    ( energy_min.(2) <- result_discret_heur2; server_min.(2)<- j; prec_max.(2) <- max (prec_max.(2)) (diff_prec first_matrix matrix_heur2) );
              with
                | OverloadedNode(_,_) -> ()
            done;
            let st = sprintf "results/size=%d_idle=%d_expe=%d_iter=%d.dat" (vertex_number) (int_of_float new_energy.static) (param.expe_number) iter in 
            let oo = open_out st in
            fprintf oo "%d\n%d\n" param.expe_number number_heur;
            for i= 0 to number_heur -1 do
              fprintf oo "%d\t%f\t%f\n" server_min.(i) energy_min.(i) prec_max.(i);
            done;
            fprintf oo "%d\t%d\n" param.number_of_speeds vertex_number; 
            print_set_of_speeds new_energy oo;
            print_square_matrix first_matrix oo;
            close_out oo
          done
        done
      end
    | 3 -> (*  *)
      begin
          for iter = 1 to (param.number_of_tests) do
            let vertex_number = (param.size_of_tree) in
            let tree = match param.tree_type with
              | _ -> cree_alea (vertex_number) (param.rmax)
            in 
            let first_matrix = make_prec_matrix_full_tree tree (vertex_number) in
            let number_heur = 3 in
            let energy_min = Array.make number_heur max_float in
            let server_min = Array.make number_heur (-1) in
            let prec_max = Array.make number_heur 0. in
            for j = 1 to (vertex_number) do
              try 
                let result_discret_heur0, matrix_heur0 = algo_discret 3 tree (vertex_number) j energy in
                  if result_discret_heur0 < energy_min.(0) then 
                    ( energy_min.(0) <- result_discret_heur0 ; server_min.(0)<- j ; prec_max.(0) <- max (prec_max.(0)) (diff_prec first_matrix matrix_heur0) );
                let result_discret_heur1, matrix_heur1 = algo_discret 5 tree (vertex_number) j energy in
                  if result_discret_heur1 < energy_min.(1) then 
                    ( energy_min.(1) <- result_discret_heur1; server_min.(1)<- j; prec_max.(1) <- max (prec_max.(1)) (diff_prec first_matrix matrix_heur1) );
                let result_discret_heur2, matrix_heur2 = algo_discret 7 tree (vertex_number) j energy in
                  if result_discret_heur2 < energy_min.(2) then 
                    ( energy_min.(2) <- result_discret_heur2; server_min.(2)<- j; prec_max.(2) <- max (prec_max.(2)) (diff_prec first_matrix matrix_heur2) );
              with
                | OverloadedNode(_,_) -> ()
            done;
            let st = sprintf "results/size=%d_idle=%d_expe=%d_iter=%d.dat" (vertex_number) (int_of_float energy.static) (param.expe_number) iter in 
            let oo = open_out st in
            fprintf oo "%d\n%d\n" param.expe_number number_heur;
            for i= 0 to number_heur -1 do
              fprintf oo "%d\t%f\t%f\n" server_min.(i) energy_min.(i) prec_max.(i);
            done;
            fprintf oo "%d\t%d\n" param.number_of_speeds vertex_number; 
            print_set_of_speeds energy oo;
            print_square_matrix first_matrix oo;
            close_out oo
          done
      end
    | 4 | 5 -> (*  *)
      begin
        for iter = 1 to (param.number_of_tests) do
          for vertex_number = 1 to (param.size_of_tree) do
            let tree = match param.tree_type with
              | _ -> cree_alea (vertex_number) (param.rmax)
            in 
              let first_matrix = make_prec_matrix_full_tree tree (vertex_number) in
              let number_heur = 3 in
              let energy_min = Array.make number_heur max_float in
              let server_min = Array.make number_heur (-1) in
              let prec_max = Array.make number_heur 0. in
                for j = 1 to (vertex_number) do
                  try 
                    let result_discret_heur0, matrix_heur0 = algo_discret 4 tree (vertex_number) j energy in
                      if result_discret_heur0 < energy_min.(0) then 
                        ( energy_min.(0) <- result_discret_heur0 ; server_min.(0)<- j ; prec_max.(0) <- max (prec_max.(0)) (diff_prec first_matrix matrix_heur0) );
                    let result_discret_heur1, matrix_heur1 = algo_discret 0 tree (vertex_number) j energy in
                      if result_discret_heur1 < energy_min.(1) then 
                        ( energy_min.(1) <- result_discret_heur1; server_min.(1)<- j; prec_max.(1) <- max (prec_max.(1)) (diff_prec first_matrix matrix_heur1) );
                    let result_discret_heur2, matrix_heur2 = algo_discret 3 tree (vertex_number) j energy in
                      if result_discret_heur2 < energy_min.(2) then 
                        ( energy_min.(2) <- result_discret_heur2; server_min.(2)<- j; prec_max.(2) <- max (prec_max.(2)) (diff_prec first_matrix matrix_heur2) );
                  with
                    | OverloadedNode(_,_) -> ()
                done;
                let st = sprintf "results/size=%d_idle=%d_expe=%d_iter=%d.dat" (vertex_number) (int_of_float energy.static) (param.expe_number) iter in 
                let oo = open_out st in
                fprintf oo "%d\n%d\n" param.expe_number number_heur;
                for i= 0 to number_heur -1 do
                  fprintf oo "%d\t%f\t%f\n" server_min.(i) energy_min.(i) prec_max.(i);
                done;
                fprintf oo "%d\t%d\n" param.number_of_speeds vertex_number; 
                print_set_of_speeds energy oo;
                print_square_matrix first_matrix oo;
                close_out oo
          done
        done
      end
    | 6 -> (*  *)
      begin
        let static = ref 0. in
	while !static <= energy.static do
          let new_energy ={static = !static; speeds = energy.speeds} in
          static := !static +. 1000.; (*the steps of the static energy are by 1000.*)
          for iter = 1 to (param.number_of_tests) do
            let vertex_number = (param.size_of_tree) in
            let tree = match param.tree_type with
              | _ -> cree_alea (vertex_number) (param.rmax)
            in 
            let first_matrix = make_prec_matrix_full_tree tree (vertex_number) in
            let number_heur = 3 in
            let energy_min = Array.make number_heur max_float in
            let server_min = Array.make number_heur (-1) in
            let prec_max = Array.make number_heur 0. in
            for j = 1 to (vertex_number) do
              try 
                let result_discret_heur0, matrix_heur0 = algo_discret 3 tree (vertex_number) j new_energy in
                  if result_discret_heur0 < energy_min.(0) then 
                    ( energy_min.(0) <- result_discret_heur0 ; server_min.(0)<- j ; prec_max.(0) <- max (prec_max.(0)) (diff_prec first_matrix matrix_heur0) );
                let result_discret_heur1, matrix_heur1 = algo_discret 5 tree (vertex_number) j energy in
                  if result_discret_heur1 < energy_min.(1) then 
                    ( energy_min.(1) <- result_discret_heur1; server_min.(1)<- j; prec_max.(1) <- max (prec_max.(1)) (diff_prec first_matrix matrix_heur1) );
                let result_discret_heur2, matrix_heur2 = algo_discret 7 tree (vertex_number) j new_energy in
                  if result_discret_heur2 < energy_min.(2) then 
                    ( energy_min.(2) <- result_discret_heur2; server_min.(2)<- j; prec_max.(2) <- max (prec_max.(2)) (diff_prec first_matrix matrix_heur2) );
              with
                | OverloadedNode(_,_) -> ()
            done;
            let st = sprintf "results/size=%d_idle=%d_expe=%d_iter=%d.dat" (vertex_number) (int_of_float new_energy.static) (param.expe_number) iter in 
            let oo = open_out st in
            fprintf oo "%d\n%d\n" param.expe_number number_heur;
            for i= 0 to number_heur -1 do
              fprintf oo "%d\t%f\t%f\n" server_min.(i) energy_min.(i) prec_max.(i);
            done;
            fprintf oo "%d\t%d\n" param.number_of_speeds vertex_number; 
            print_set_of_speeds new_energy oo;
            print_square_matrix first_matrix oo;
            close_out oo
          done
        done
      end
    | 7 -> (*  *)
      begin
          for iter = 1 to (param.number_of_tests) do
            let vertex_number = (param.size_of_tree) in
            let tree = match param.tree_type with
              | _ -> cree_alea (vertex_number) (param.rmax)
            in 
            let first_matrix = make_prec_matrix_full_tree tree (vertex_number) in
            let number_heur = 3 in
            let energy_min = Array.make number_heur max_float in
            let server_min = Array.make number_heur (-1) in
            let prec_max = Array.make number_heur 0. in
            for j = 1 to (vertex_number) do
              try 
                let result_discret_heur0, matrix_heur0 = algo_discret 3 tree (vertex_number) j energy in
                  if result_discret_heur0 < energy_min.(0) then 
                    ( energy_min.(0) <- result_discret_heur0 ; server_min.(0)<- j ; prec_max.(0) <- max (prec_max.(0)) (diff_prec first_matrix matrix_heur0) );
                let result_discret_heur1, matrix_heur1 = algo_discret 5 tree (vertex_number) j energy in
                  if result_discret_heur1 < energy_min.(1) then 
                    ( energy_min.(1) <- result_discret_heur1; server_min.(1)<- j; prec_max.(1) <- max (prec_max.(1)) (diff_prec first_matrix matrix_heur1) );
                let result_discret_heur2, matrix_heur2 = algo_discret 7 tree (vertex_number) j energy in
                  if result_discret_heur2 < energy_min.(2) then 
                    ( energy_min.(2) <- result_discret_heur2; server_min.(2)<- j; prec_max.(2) <- max (prec_max.(2)) (diff_prec first_matrix matrix_heur2) );
              with
                | OverloadedNode(_,_) -> ()
            done;
            let st = sprintf "results/size=%d_idle=%d_expe=%d_iter=%d.dat" (vertex_number) (int_of_float energy.static) (param.expe_number) iter in 
            let oo = open_out st in
            fprintf oo "%d\n%d\n" param.expe_number number_heur;
            for i= 0 to number_heur -1 do
              fprintf oo "%d\t%f\t%f\n" server_min.(i) energy_min.(i) prec_max.(i);
            done;
            fprintf oo "%d\t%d\n" param.number_of_speeds vertex_number; 
            print_set_of_speeds energy oo;
            print_square_matrix first_matrix oo;
            close_out oo
          done
      end



let () = script Sys.argv.(1)




(*
let script config_file =
  let param = parse_config config_file in
  let tab_of_speeds =
    match param.regularity_speed with
      | 1 -> reg_tab_speeds param.number_of_speeds param.max_speed
      | _ -> intel_tab_speeds param.number_of_speeds param.max_speed
    in 
  let energy = {static = param.static; speeds = tab_of_speeds } in 
  for i = 1 to (param.number_of_tests) do
    let tree = match param.tree_type with
      | _ -> cree_alea (param.size_of_tree) (param.rmax)
    in 
    match param.expe_number with
    | 1 -> (*where we compare the three location heuristics (greedy, move 1, move 2) with only one server allocation heuristic *)
      begin
      let first_matrix = make_prec_matrix_full_tree tree (param.size_of_tree) in
      for j = 1 to (param.size_of_tree) do
        try 
        let result_discret_greedy, matrix_greedy = algo_discret 0 tree (param.size_of_tree) j energy in
        let result_discret_move_1, matrix_move_1 = algo_discret 1 tree (param.size_of_tree) j energy in
        let result_discret_move_2, matrix_move_2 = algo_discret 2 tree (param.size_of_tree) j energy in
        let st = sprintf "results/size=%d_idle=%d_expe=%d_iter=%d.dat" (param.size_of_tree) (energy.static) (param.expe_number) i in 
        let oo = open_out st in
          fprintf oo "%d \n %f \n %f \n %f \n %d \t %d \t %d \n" param.expe_number result_discret_greedy result_discret_move_1 result_discret_move_2 param.number_of_speeds param.size_of_tree j; (*result \n K T S \n*)
          print_set_of_speeds energy oo;
          print_square_matrix first_matrix oo;
(*          print_square_matrix_int matrix first_matrix oo;*)
          print_square_matrix matrix_greedy oo;
          print_square_matrix matrix_move_1 oo;
          print_square_matrix matrix_move_2 oo;
          close_out oo
        with
          | OverloadedNode(_,_) -> ()
      done;
      end
    | 2 -> (*where we compare the two server allocation heuristics with only one location heuristics (greedy)*)
      begin
      let first_matrix = make_prec_matrix_full_tree tree (param.size_of_tree) in
      for j = 1 to (param.size_of_tree) do
        try 
        let result_discret_greedy, matrix_greedy = algo_discret 0 tree (param.size_of_tree) j energy in
        let result_discret_greedy_2, matrix_greedy_2 = algo_discret 3 tree (param.size_of_tree) j energy in
        let result_discret_worse, _ = algo_discret 4 tree (param.size_of_tree) j energy in
        let st = sprintf "results/size=%d_serv=%d_iter=%d.dat" (param.size_of_tree) j i in 
        let oo = open_out st in
          fprintf oo "%d\n %f \n %f \n %f \n %d \t %d \t %d \n" param.expe_number result_discret_greedy result_discret_greedy_2 result_discret_worse param.number_of_speeds param.size_of_tree j; (*result \n K T S \n*)
          print_set_of_speeds energy oo;
          print_square_matrix first_matrix oo;
(*          print_square_matrix_int matrix first_matrix oo;*)
          print_square_matrix matrix_greedy oo;
          print_square_matrix matrix_greedy_2 oo;
          close_out oo
        with
          | OverloadedNode(_,_) -> ()
      done;
      end
    | 3 -> 
      begin
      let first_matrix = make_prec_matrix_full_tree tree (param.size_of_tree) in
      for j = 1 to (param.size_of_tree) do
        try 
        let result_discret_greedy, matrix_greedy = algo_discret 0 tree (param.size_of_tree) j energy in
        let result_discret_move_1, matrix_move_1 = algo_discret 1 tree (param.size_of_tree) j energy in
        let result_discret_greedy_2, matrix_greedy_2 = algo_discret 3 tree (param.size_of_tree) j energy in
        let st = sprintf "results/size=%d_serv=%d_iter=%d.dat" (param.size_of_tree) j i in 
        let oo = open_out st in
          fprintf oo "%d \n %f \n %f \n %f \n %d \t %d \t %d \n" param.expe_number result_discret_greedy result_discret_move_1 result_discret_greedy_2 param.number_of_speeds param.size_of_tree j; (*result \n K T S \n*)
          print_set_of_speeds energy oo;
          print_square_matrix first_matrix oo;
(*          print_square_matrix_int matrix first_matrix oo;*)
          print_square_matrix matrix_greedy oo;
          print_square_matrix matrix_move_1 oo;
          close_out oo
        with
          | OverloadedNode(_,_) -> ()
      done;
      end
    | 4 -> 
      begin
      let first_matrix = make_prec_matrix_full_tree tree (param.size_of_tree) in
      for j = 1 to (param.size_of_tree) do
        try 
        let result_discret_greedy, matrix_greedy = algo_discret 5 tree (param.size_of_tree) j energy in
        let result_discret_move_1, matrix_move_1 = algo_discret 6 tree (param.size_of_tree) j energy in
        let result_discret_greedy_2, matrix_greedy_2 = algo_discret 1 tree (param.size_of_tree) j energy in
        let st = sprintf "results/size=%d_serv=%d_iter=%d.dat" (param.size_of_tree) j i in 
        let oo = open_out st in
          fprintf oo "%d \n %f \n %f \n %f \n %d \t %d \t %d \n" param.expe_number result_discret_greedy result_discret_move_1 result_discret_greedy_2 param.number_of_speeds param.size_of_tree j; (*result \n K T S \n*)
          print_set_of_speeds energy oo;
          print_square_matrix first_matrix oo;
(*          print_square_matrix_int matrix first_matrix oo;*)
          print_square_matrix matrix_greedy oo;
          print_square_matrix matrix_move_1 oo;
          close_out oo
        with
          | OverloadedNode(_,_) -> ()
      done;
      end
  done

let () = script Sys.argv.(1) *)
