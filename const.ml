open Arg

type parameter = {
  rmax : float;
  tree_type : int;
  size_of_tree : int;
  number_of_speeds : int;
  max_speed : float;
  regularity_speed : int;
  number_of_tests : int
 }

let default = {
  rmax = 10.;
  tree_type = 0;
  size_of_tree = 15;
  number_of_speeds = 4;
  max_speed = 15.;
  regularity_speed = 1;
  number_of_tests = 50
}
