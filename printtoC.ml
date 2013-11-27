open Def
open Printf

let make_prec_matrix_server_tree_int tree size_of_tree = (*This should be called on a tree result of "tarbre_to_arbre_int", so the input is still correct.*)
  let sol = Array.make_matrix size_of_tree size_of_tree 0. in
  let i = ref 0 in
  let rec aux_tree list_of_subtrees list_of_index=
    match list_of_subtrees with 
      | [] -> ()
      | ServerInt( lt, id, prop) :: b -> 
        begin 
          List.iter (function x -> sol.(x).(!i)<- 1.) list_of_index;
          sol.(!i).(!i)<- prop.load;
          incr i;
          aux_tree lt ((!i-1)::list_of_index);
          aux_tree b list_of_index
  	end
  in 
    aux_tree [tree] [];
    sol


let print_square_matrix matrix oo =
  let n = Array.length matrix.(0) in
  for i = 0 to n-1 do
    for j= 0 to n-1 do
      fprintf oo "%f \t" matrix.(i).(j)
    done;
  fprintf oo "\n"
  done

(*Patch that needs to be corrected!*)
let print_square_matrix_int matrix1 matrix2 oo =
  let result = matrix1 in
  let n = Array.length matrix1.(0) in
  for i = 0 to n-1 do
    if matrix1.(i).(i) = 0. then
      let j = ref (i-1) in
      while matrix1.(i).(!j) = 0. do
       decr j
      done;
      if matrix1.(i).(!j) = 0. then failwith "bug ici, remplacer i par j";
        result.(!j).(!j) <- result.(!j).(!j) +. matrix2.(i).(i)
    else     
    result.(i).(i) <- matrix2.(i).(i)
  done;
    print_square_matrix result oo

let print_set_of_speeds speeds_array oo= 
  let n = Array.length speeds_array in
  for i= 0 to n-1 do
    fprintf oo "%f \t" speeds_array.(i)
  done;
  fprintf oo "\n"



let make_prec_matrix_full_tree tree size_of_tree = (*This should be called on a tree result of "tarbre_to_arbre_int", so the input is still correct.*)
  let sol = Array.make_matrix size_of_tree size_of_tree 0. in
  let i = ref 0 in
  let rec aux_tree list_of_subtrees list_of_index=
    match list_of_subtrees with 
      | [] -> ()
      | Node( lt, inp) :: b -> 
        begin 
          List.iter (function x -> sol.(x).(!i)<- 1.) list_of_index;
          sol.(!i).(!i)<- inp;
          incr i;
          aux_tree lt ((!i-1)::list_of_index);
          aux_tree b list_of_index
  	end
      | Server(_,_,_) :: _-> failwith "This should not be here"
  in 
    aux_tree [tree] [];
    sol
