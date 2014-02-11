#!/bin/sh
cd ..
cd results
case $9 in
	0|4|1|5 )
	for iter in `seq 1 $8`; do
		for nodes in `seq 1 $3`; do
			rm size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}.dat
			rm pbm_size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp
			rm result_${nodes}_${5}_$4_${9}_${iter}.temp
		done
	done
	rm *.log
	;;
	2|6 )
	for iter in `seq 1 $8`; do
		r=$(echo "$5/20" | bc)
		for static in `seq 0 $r $5`; do
			rm size=${3}_idle=${static}_expe=$9_iter=${iter}.dat
			rm pbm_size=${3}_idle=${static}_expe=$9_iter=${iter}_general.lp
			rm result_${3}_${static}_$4_${9}_${iter}.temp
		done
	done
	rm *.log
	;;
	3|7 )
	for iter in `seq 1 $8`; do
		rm size=${3}_idle=$5_speeds=$4_expe=$9_iter=${iter}.dat
		rm pbm_size=${3}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp
		rm result_${3}_${5}_$4_${9}_${iter}.temp
	done
	rm *.log
	;;
	8 )
	for iter in `seq 1 $8`; do
		r=$(echo "$3/20" | bc)
		for nodes in `seq $r $r $3`; do
			rm size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}.dat
			rm pbm_size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp
			rm result_${nodes}_${5}_${9}_${iter}.temp
		done
	done
	;;
	* ) 
	echo "You have tried an expe_number that is not yet implemented."
	;;
esac
