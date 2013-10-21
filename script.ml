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

(*Creation of a regular table of speeds*)
let reg_tab_speeds n  max_speed= 
  let sol = Array.make n 0 in
  for i=1 to n-1 do
    sol.(i) <- i* (max_speed /(n-1))
  done;
  sol

(*Creation of an irregular table of speeds. There can be equal speeds.*)
let irreg_tab_speeds n  max_speed= 
  let sol = Array.make n 0 in
  for i=1 to n-1 do
    sol.(i) <- Random.int max_speed
  done;
  Array.fast_sort compare sol;
  sol


let parse_config file =
  let chan = open_in file in
  let rmax = int_of_string (input_line chan) in
  let tree_type = int_of_string (input_line chan) in 
  let size_of_tree = int_of_string (input_line chan) in
  let number_of_speeds = int_of_string (input_line chan) in
  let max_speed = int_of_string (input_line chan) in
  let regularity_speed = int_of_string (input_line chan) in
  let number_of_tests = int_of_string (input_line chan) in
  let () = close_in chan in
  {
  rmax = rmax;
  tree_type = tree_type;
  size_of_tree = size_of_tree;
  number_of_speeds = number_of_speeds;
  max_speed = max_speed;
  regularity_speed = regularity_speed;
  number_of_tests = number_of_tests 
  }


let script config_file =
  let param = parse_config config_file in
  let tab_of_speeds =
    match param.regularity_speed with
      | 1 -> reg_tab_speeds param.number_of_speeds param.max_speed
      | _ -> irreg_tab_speeds param.number_of_speeds param.max_speed
    in 
  for i = 1 to (param.number_of_tests) do
    let tree = match param.tree_type with
      | _ -> cree_alea (param.size_of_tree) (param.rmax)
    in 
      let first_matrix = make_prec_matrix_full_tree tree (param.size_of_tree) in
      for j = 1 to (param.size_of_tree) do
        try 
        let result_discret_intel, matrix = algo_discret 0 tree (param.size_of_tree) j tab_of_speeds in
        let st = sprintf "results/size=%d_serv=%d_iter=%d.dat" (param.size_of_tree) j i in 
        let oo = open_out st in
          fprintf oo "%d \n %d \t %d \t %d \n" result_discret_intel param.number_of_speeds param.size_of_tree j; (*result \n K T S \n*)
          print_set_of_speeds tab_of_speeds oo;
          print_square_matrix first_matrix oo;
          print_square_matrix matrix oo;
          close_out oo
        with
          | OverloadedNode(_,_) -> ()
      done;
  done

let () = script Sys.argv.(1)
