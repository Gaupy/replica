To obtain some results:
===============

./run.sh $1 $2 $3 $4 $5 $6 $7 $8 $9

where one should replace
-------------

* $1 by rmax (the maximum size of a request)
* $2 by tree_type (for now there is only 0 a random tree)
* $3 by size_of_tree (the number of nodes in the tree)
* $4 by number_of_speeds (the number of speeds, count speed 0)
* $5 by static (the static energy)
* $6 by max_speed (the maximum speed)
* $7 by regularity_speeds (1 for equally distances speeds, 0 for random speed between 0 and max_speed)
* $8 by number_of_tests
* $9 by the experiment number

example of configurations:
-------------
* ./run.sh 100 0 20 4 10000 150 1 100 3



once this is done, you can go to ../exploitation_results and execute: 
./lp.sh $1 $2 $3 $4 $5 $6 $7 $8 $9

The file result_$3.avg gives the average over the $7 tests.
