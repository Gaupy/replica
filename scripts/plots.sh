#!/bin/sh
case $9 in
	0 )
	cd ../exploitation_results
	echo "$1\n$2\n$3\n$4\n$5\n$6\n$7\n$8\n$9" > conf
	make
	./comp conf < ../results/result_$3_$4_$5_$7_$9.score
	gnuplot maxnodes_$3_rmax=$1_treetype=$2_nspeed=$4_maxspeed=$6_typespeed=$7_static=$5.p
#	echo "You have tried an expe_number that is not yet implemented."
	;;
	1 )
	cd ../exploitation_results
	echo "$1\n$2\n$3\n$4\n$5\n$6\n$7\n$8\n$9" > conf
	make
	./comp conf < ../results/result_$3_$4_$5_$7_$9.score
	gnuplot maxnodes_$3_rmax=$1_treetype=$2_nspeed=$4_maxspeed=$6_typespeed=$7_static=$5.p
#	echo "You have tried an expe_number that is not yet implemented."
	;;
	2 )
	cd ../exploitation_results
	echo "$1\n$2\n$3\n$4\n$5\n$6\n$7\n$8\n$9" > conf
	make
	./comp conf < ../results/result_$3_$4_$5_$7_$9.score
	gnuplot static_energy_$5_nodes=$3_rmax=$1_treetype=$2_nspeed=$4_maxspeed=$6_typespeed=$7.p
	;;
	3 )
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
