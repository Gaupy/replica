#!/bin/sh
cd ..
echo "$1\n$2\n$3\n$4\n$5\n$6\n$7" > conf
make opt
./comp conf
cd results
gcc ../LP/lp.c -o test -lglpk -fno-stack-protector 
for r in `seq 1 $7`; do
#	ssh slsu0-01.dsi-ext.ens-lyon.fr
	for s in `seq 1 $3`; do
		./test $3 $s $r
		cplex -c "r pbm_size=$3_serv=${s}_iter=${r}_general.lp" "opt"|grep "Objective ="| cut -d " " -f 10 >> result_$3_${s}_${r}.temp
#		echo "\t" >> result_$3_${s}_${r}.temp
		cplex -c "r pbm_size=$3_serv=${s}_iter=${r}_located.lp" "opt"|grep "Objective ="| cut -d " " -f 10 >> result_$3_${s}_${r}.temp
#		cplex -c "r pbm_size=$3_serv=${s}_iter=${r}_located.lp" "opt"|grep "MIP - Integer infeasible."| cut -d " " -f 10 >> result_$3_${s}_${r}.temp
	done
done
#wait
for r in `seq 1 $7`; do
	for s in `seq 1 $3`; do
		cat result_$3_${s}_${r}.temp >> result_$3_$4_$6.score
#		echo "\n" >> result_$3_$4_$6.score
	done
done
rm ../conf *.temp *.lp *.dat
