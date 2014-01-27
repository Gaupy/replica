open Def
open Print
open PrinttoC
open Tools
open Algos


(*Warning, energy should always start with speed 0.*)

let algo_discret id t size_t n energy =
   match id with
    | 0 -> 
      let _, t_cont = algo_intelligent t n in 
      let t_temp = tarbre_to_arbre_int t_cont energy in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
      let t_discrete = optim_discret 0 (t_temp) energy in
        (energy_int t_discrete energy, matrix)
    | 1 -> 
      let _ ,t_cont = algo_gopi t n in
      let t_temp = tarbre_to_arbre_int t_cont energy in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
      let t_discrete = optim_discret 0 (t_temp) energy in
        (energy_int t_discrete energy, matrix)
    | 2 -> 
      let _ ,t_cont = greedy_7 t n in
      let t_temp = tarbre_to_arbre_int t_cont energy in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
      let t_discrete = optim_discret 0 (t_temp) energy in
        (energy_int t_discrete energy, matrix)
    | 3 -> 
      let _, t_cont = algo_intelligent t n in 
      let t_temp = tarbre_to_arbre_int t_cont energy in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
      let t_discrete = optim_discret 1 (t_temp) energy in
        (energy_int t_discrete energy, matrix)
    | 4 -> 
      let _, t_cont = algo_intelligent t n in 
      let t_temp = tarbre_to_arbre_int t_cont energy in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
        (energy_int t_temp energy, matrix)
    | 5 -> 
      let _, t_cont = algo_intelligent t n in 
      let t_temp = tarbre_to_arbre_int t_cont energy in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
      let t_discrete_v1 = optim_discret 0 (t_temp) energy in
      let t_discrete_v2 = optim_discret_second_turn 0 t_discrete_v1 energy in
        (energy_int t_discrete_v2 energy, matrix)
    | 6 -> 
      let _, t_cont = algo_intelligent t n in 
      let t_temp = tarbre_to_arbre_int t_cont energy in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
      let t_discrete_v1 = optim_discret 0 (t_temp) energy in
      let t_discrete_v2 = optim_discret_second_turn 1  t_discrete_v1 energy in
        (energy_int t_discrete_v2 energy, matrix)
    | 7 -> 
      let _, t_cont = algo_intelligent t n in 
      let t_temp = tarbre_to_arbre_int t_cont energy in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
      let t_discrete_v1 = optim_discret 1 (t_temp) energy in
      let t_discrete_v2 = optim_discret_second_turn 0 t_discrete_v1 energy in
        (energy_int t_discrete_v2 energy, matrix)
    | 8 -> 
      let _, t_cont = algo_intelligent t n in 
      let t_temp = tarbre_to_arbre_int t_cont energy in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
      let t_discrete_v1 = optim_discret 1 (t_temp) energy in
      let t_discrete_v2 = optim_discret_second_turn 1 t_discrete_v1 energy in
        (energy_int t_discrete_v2 energy, matrix)
    | _ -> 
      let _, t_cont = algo_intelligent t n in 
      let t_temp = tarbre_to_arbre_int t_cont energy in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
      let t_discrete = optim_discret 1 (t_temp) energy in
        (energy_int t_discrete energy, matrix)

