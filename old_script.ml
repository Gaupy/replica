open Def
open Print
open PrinttoC
open Tools
open Trees
open Brute
open Algos
open AlgosDiscret
open Format
open Printf

let () = Random.self_init ()

let test_egalite nb_tree entree_max taille_max_arbre =
  let continue = ref true in
  let compt = ref 0 in
  let ta = ref ( 2 + Random.int (taille_max_arbre -2)) in  
  let tree = ref (cree_alea (!ta) (entree_max)) in
  let cex = ref (!tree) in
  let nb_cex = ref 0 in
  while (!continue) do
    incr compt;
    flush_all ();
    let ma,i = greedy_7 (!tree) (!ta/2) in
    let mb,c = greedy_8 (!tree) (!ta/2) 3 in
    if (!compt mod 1 == 0) then (Printf.printf "%d -> %d\n" (!compt) (!ta));
    if (mb = ma) then () else (
	continue := false;
	print_arbre_latex (!tree);
	print_arbre_v (!tree);
	cex := !tree;
    );
    ta := ( 2 + Random.int (taille_max_arbre -2));
    tree := cree_alea (!ta) (entree_max);
  done;
  (!continue,!cex,!nb_cex)

(*let t = test_egalite 0 150 45*)

let reg_tab_speeds n  max_speed= 
  let sol = Array.make n 0 in
  for i=1 to n-1 do
    sol.(i) <- i* (max_speed /n)
  done;
  sol


let test_caml_to_c entree_max taille_max_arbre number_of_speeds=
  let st = sprintf "results/random%d.dat" 1 in
  let oo = open_out st in
  let ta = ref ( 2 + Random.int (taille_max_arbre -2)) in  
  let tree = ref (cree_alea (!ta) (entree_max)) in
  let tab_of_speeds = reg_tab_speeds number_of_speeds (2*entree_max) in
  let first_matrix = make_prec_matrix_full_tree !tree !ta in
  let result_discret_intel, matrix = algo_discret 0 !tree !ta (!ta/2) tab_of_speeds in
   fprintf oo "%d \n %d \t %d \t %d \n" result_discret_intel number_of_speeds !ta (!ta/2); (*result \n K T S \n*)
   print_set_of_speeds tab_of_speeds oo;
   print_square_matrix first_matrix oo;
   print_square_matrix matrix oo;
   close_out oo


let rec test_interminable entree_max taille_max_arbre number_of_speeds=
    try test_caml_to_c entree_max taille_max_arbre number_of_speeds with
    | OverloadedNode(_,_) -> test_interminable entree_max taille_max_arbre number_of_speeds


let _ = test_interminable 50 30 10



(*let cex = Node([Node([Node([],1481);Node([Node([Node([],2019);Node([],1381);],549);],3199);],2512);Node([Node([],2995);Node([Node([],1740);],2744);],3750);Node([],1179);Node([Node([Node([],1771);],1052);Node([Node([Node([],1735);],1513);Node([Node([Node([],1823);],2624);Node([],3102);],88);],2465);],3326);Node([],2600);],346)*)

(*let compare_discret_continue tree n  =*)
(*  let tab_of_speeds = reg_tab_speeds 700 8000 in*)
(*  let first_matrix = make_prec_matrix_full_tree tree 23 in*)
(*  print_square_matrix first_matrix;*)
(*  let result_discret_intel,_ = algo_discret 0 tree 23 n tab_of_speeds in*)
(*(*  let result_discret_greedy7 = algo_discret 1 tree n tab_of_speeds in*)*)
(*(*  let result_discret_greedy8 = algo_discret 2 tree n tab_of_speeds in*)*)
(*  printf "intel = %d \n" result_discret_intel*)
(*(*  printf "intel = %d, greedy7 =%d, greedy8= %d \n" result_discret_intel result_discret_greedy7 result_discret_greedy8*)*)

(*let _ = compare_discret_continue cex 11*)



(*
(*Counter-example for greedy_8 vs greedy_7*)

let cex = Node([Node([Node([],1481);Node([Node([Node([],2019);Node([],1381);],549);],3199);],2512);Node([Node([],2995);Node([Node([],1740);],2744);],3750);Node([],1179);Node([Node([Node([],1771);],1052);Node([Node([Node([],1735);],1513);Node([Node([Node([],1823);],2624);Node([],3102);],88);],2465);],3326);Node([],2600);],346)

let trouve_n a =
  let continue = ref true in
  let compt = ref 0 in
  while (!continue) do
    incr compt;
    flush_all ();
    let ma,i = greedy_7 cex (!compt) in
    let mb,c = greedy_8 cex (!compt) 3 in 
    if (mb = ma) then () else (
	continue := false;
	print_int !compt; print_newline ();
	print_string ("Energie de Greedy7 ="); print_int ma; print_newline ();
	print_string ("Energie de Greedy8 ="); print_int mb; print_newline ();
	print_arbre_latex (i);print_newline ();
	print_arbre_latex (c);print_newline ();
    );
    done
(*let t = trouve_n 1*)

let bli c =
 let a,i = greedy_7 cex 10 in (print_int a; print_newline (); print_arbre_latex (i);print_newline ());
 let b,_ = greedy_8 cex 10 3 in print_int b
 
 let t = bli 1


(*Counter-example for algo_gopi vs greedy_7*)

let cex = Node([Node([Node([Node([Node([Node([],37);Node([],81);],35);Node([Node([],121);],142);],46);],133);Node([Node([Node([],135);Node([],100);],36);Node([Node([Node([Node([],120);],115);Node([],65);],5);Node([],144);],76);Node([],69);Node([],145);],96);Node([Node([Node([Node([Node([],45);Node([],84);],6);],124);Node([],84);Node([],108);],57);],18);],113);Node([Node([],0);],36);Node([Node([Node([Node([],34);Node([],146);],58);],63);Node([],143);Node([],42);],70);Node([],23);],5)

let trouve_n a =
  let continue = ref true in
  let compt = ref 0 in
  while (!continue) do
    incr compt;
    flush_all ();
    let ma,i,_ = greedy_7 cex (!compt) in
    let mb,c,_ = algo_gopi cex (!compt) in 
    if (mb = ma) then () else (
	continue := false;
	print_int !compt; print_newline ();
	print_string ("Energie de Greedy7 ="); print_int ma; print_newline ();
	print_string ("Energie de Algo Gopi ="); print_int mb; print_newline ();
	print_arbre_latex (i);print_newline ();
	print_arbre_latex (c);print_newline ();
    );
    done
let t = trouve_n 1

let bli a =
 let _,c,_ = algo_gopi cex 18 in print_arbre_latex (c)
 
 let t = bli *)
