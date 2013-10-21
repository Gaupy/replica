To obtain some results:
===============

./lp.sh $1 $2 $3 $4 $5 $6 $7

where one should replace
-------------

* $1 by rmax (the maximum size of a request)
* $2 by tree_type (for now there is only 0 a random tree)
* $3 by size_of_tree (the number of nodes in the tree)
* $4 by number_of_speeds (the number of speeds, count speed 0)
* $5 by max_speed (the maximum speed)
* $6 by regularity_speeds (1 for equally distances speeds, 0 for random speed between 0 and max_speed)
* $7 by number_of_tests

example of configurations:
-------------
* ./lp.sh 100 0 20 4 150 1 100
* ./lp.sh 1000 0 21 4 1500 1 50



once this is done, you can go to ../exploitation_results and execute: 
./lp.sh $1 $2 $3 $4 $5 $6 $7

The file result_$3.avg gives the average over the $7 tests.
