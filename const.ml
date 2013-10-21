open Arg

type parameter = {
  rmax : int;
  tree_type : int;
  size_of_tree : int;
  number_of_speeds : int;
  max_speed : int;
  regularity_speed : int;
  number_of_tests : int
 }

let default = {
  rmax = 1000;
  tree_type = 0;
  size_of_tree = 15;
  number_of_speeds = 4;
  max_speed = 1500;
  regularity_speed = 1;
  number_of_tests = 50
}
