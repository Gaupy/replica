open Def
open Print
open PrinttoC
open Tools
open Algos


(*Warning, tab_of_speeds should always start with speed 0.*)

let algo_discret id t size_t n tab_of_speeds =
   match id with
    | 0 -> 
      let _, t_cont = algo_intelligent t n in 
      let t_temp = tarbre_to_arbre_int t_cont tab_of_speeds in
      let matrix = make_prec_matrix_server_tree_int t_temp size_t in
      let t_discrete = optim_discret 0 (t_temp) tab_of_speeds in
        (energy_int t_discrete tab_of_speeds, matrix)
    | 1 -> 
      let _ ,t_cont = greedy_7 t n in
      let t_discrete = optim_discret 0 (tarbre_to_arbre_int t_cont tab_of_speeds) tab_of_speeds in
        (energy_int t_discrete tab_of_speeds, (Array.make_matrix 1 1 0))
    | _ -> 
      let _ ,t_cont = greedy_8 t n 3 in
      let t_discrete = optim_discret 0 (tarbre_to_arbre_int t_cont tab_of_speeds) tab_of_speeds in
        (energy_int t_discrete tab_of_speeds, (Array.make_matrix 1 1 0 ))
