#!/bin/sh
read -p 'rmax: ' var1 
read -p 'tree_type: ' var2
read -p 'size_of_tree: ' var3
read -p 'number of speeds: ' var4
read -p 'static energy: ' var5
read -p 'max_speed: ' var6
read -p 'regularity of speeds: ' var7
read -p 'number of tests: ' var8
read -p 'expe number: ' var9
./caml.sh ${var1} ${var2} ${var3} ${var4} ${var5} ${var6} ${var7} ${var8} ${var9}
./lin_prog.sh ${var1} ${var2} ${var3} ${var4} ${var5} ${var6} ${var7} ${var8} ${var9}
./plots.sh ${var1} ${var2} ${var3} ${var4} ${var5} ${var6} ${var7} ${var8} ${var9}
