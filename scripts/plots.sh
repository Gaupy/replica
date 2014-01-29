#!/bin/sh
case $9 in
	0|4 )
	cd ../results
	for iter in `seq 1 $8`; do
		for nodes in `seq 1 $3`; do
			cat result_${nodes}_${5}_${9}_${iter}.temp >> result_$3_$4_$5_$7_$9.score
			rm result_${nodes}_${5}_${9}_${iter}.temp
		done
	done
	cd ../exploitation_results
	echo "$1\n$2\n$3\n$4\n$5\n$6\n$7\n$8\n$9" > conf
	make
	./comp conf < ../results/result_$3_$4_$5_$7_$9.score
	gnuplot maxnodes_$3_rmax=$1_treetype=$2_nspeed=$4_maxspeed=$6_typespeed=$7_static=$5.p
#	echo "You have tried an expe_number that is not yet implemented."
	;;
	1|5 )
	cd ../results
	for iter in `seq 1 $8`; do
		for nodes in `seq 1 $3`; do
			cat result_${nodes}_${5}_${9}_${iter}.temp >> result_$3_$4_$5_$7_$9.score
			rm result_${nodes}_${5}_${9}_${iter}.temp
		done
	done
	cd ../exploitation_results
	echo "$1\n$2\n$3\n$4\n$5\n$6\n$7\n$8\n$9" > conf
	make
	./comp conf < ../results/result_$3_$4_$5_$7_$9.score
	gnuplot maxnodes_$3_rmax=$1_treetype=$2_nspeed=$4_maxspeed=$6_typespeed=$7_static=$5.p
#	echo "You have tried an expe_number that is not yet implemented."
	;;
	2|6 )
	cd ../results
	for iter in `seq 1 $8`; do
		for static in `seq 0 1000 $5`; do
			cat result_${3}_${static}_${9}_${iter}.temp >> result_$3_$4_$5_$7_$9.score
			rm result_${3}_${static}_${9}_${iter}.temp
		done
	done
	cd ../exploitation_results
	echo "$1\n$2\n$3\n$4\n$5\n$6\n$7\n$8\n$9" > conf
	make
	./comp conf < ../results/result_$3_$4_$5_$7_$9.score
	gnuplot static_energy_$5_nodes=$3_rmax=$1_treetype=$2_nspeed=$4_maxspeed=$6_typespeed=$7.p
	;;
	3|7 )
	cd ../results
	for iter in `seq 1 $8`; do
		cat result_${3}_${5}_${9}_${iter}.temp >> result_$3_$4_$5_$7_$9.score
		rm result_${3}_${5}_${9}_${iter}.temp
	done
	cd ../exploitation_results
	echo "$1\n$2\n$3\n$4\n$5\n$6\n$7\n$8\n$9" > conf
	make
	./comp conf < ../results/result_$3_$4_$5_$7_$9.score
	gnuplot load_nodes=$3_rmax=$1_treetype=$2_nspeed=$4_maxspeed=$6_typespeed=$7_static=$5.p
	;;
	* ) 
	echo "You have tried an expe_number that is not yet implemented."
	;;
esac
