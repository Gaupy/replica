#!/bin/sh
#read -p 'rmax: ' var1 
#read -p 'tree_type: ' var2
#read -p 'size_of_tree: ' var3
#read -p 'number of speeds: ' var4
#read -p 'static energy: ' var5
#read -p 'max_speed: ' var6
#read -p 'regularity of speeds: ' var7
#read -p 'number of tests: ' var8
#read -p 'expe number: ' var9
./caml.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
./lin_prog.sh $1 $2 $3 $4 $5 $6 $7 $8 $9
./plots.sh  $1 $2 $3 $4 $5 $6 $7 $8 $9
