To obtain some results:
===============

Those two scripts do the whole work:
 * $./full_script.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
 * $./full.sh $1 $2 $3 $4 $5 $6 $7 $8 $9

This following scripts also does the whole work, but will ask you what you want for the different values.
 * $./simple.sh

If you want to decompose the work:
 * $./caml.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
runs the heuristics
 * $./lin_prog.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
runs the linear program
 * $./plots.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
creates the result file and plots the results.

Those scripts should be run in this order, but if you interrupt one, the following scripts will be able to do what they were supposed to do on the already computed files.
Note that if you want to add more data points after one run of the execution, you can rerun it it will add all data points (as long as all parameters except the number of tests are equal).

Finally, the result is stored in 
  - exploitation_results/static_energy_$5_nodes=$3_rmax=$1_treetype=$2_nspeed=$4_maxspeed=$6_typespeed=$7.pdf (for expe 0 and 1)
  - static_energy_$5_nodes=$3_rmax=$1_treetype=$2_nspeed=$4_maxspeed=$6_typespeed=$7.pdf (for expe 2)
  - load_nodes=$3_rmax=$1_treetype=$2_nspeed=$4_maxspeed=$6_typespeed=$7_static=$5.pdf (for expe 3)
where one should replace
-------------

* $1(: int) by rmax (the maximum size of a request)
* $2(: int) by tree_type (for the moment the only possibility is 0: a random tree)
* $3(: int) by size_of_tree (the number of nodes in the tree)
* $4(: int) by number_of_speeds (the number of speeds, count speed 0)
* $5(: int) by static (the static energy)
* $6(: int) by max_speed (the maximum speed)
* $7(: int) by regularity_speeds (1 for equally distances speeds, 0 for the speeds that use the ratio (to the max_speed) equal to the Intel Xscale machine)
* $8(: int) by number_of_tests
* $9(: int) by the experiment number (in 0 and 1 we study the impact of the size of the tree, in 2 we study the impact of the static energy, in 3 the impact of the load of the tree).

example of configurations:
-------------
* ./full.sh 100 0 20 4 10000 150 1 100 3

