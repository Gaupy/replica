#!/bin/sh
cd ../results
gcc ../LP/new_lp.c -o test -lglpk -fno-stack-protector 
case $9 in
	0 )
	for iter in `seq 1 $8`; do
		for nodes in `seq 1 $3`; do
			./test ${nodes} $5 $9 ${iter}
			rm size=${nodes}_idle=${5}_expe=$9_iter=${iter}.dat
			if [ "${nodes}" -lt 31 ]; then
				cplex -c "r pbm_size=${nodes}_idle=$5_expe=$9_iter=${iter}_general.lp" "opt"|grep "Objective ="| cut -d "=" -f 2 >> result_${nodes}_${5}_${9}_${iter}.temp
			else
				echo "" >> result_${nodes}_${5}_${9}_${iter}.temp
			fi
			rm pbm_size=${nodes}_idle=$5_expe=$9_iter=${iter}_general.lp
		done
	done
	rm *.log
	;;
	1 )
	for iter in `seq 1 $8`; do
		for nodes in `seq 1 $3`; do
			./test ${nodes} $5 $9 ${iter}
			rm size=${nodes}_idle=${5}_expe=$9_iter=${iter}.dat
			if [ "${nodes}" -lt 26 ]; then
				cplex -c "r pbm_size=${nodes}_idle=$5_expe=$9_iter=${iter}_general.lp" "opt"|grep "Objective ="| cut -d "=" -f 2 >> result_${nodes}_${5}_${9}_${iter}.temp
			else
				echo "" >> result_${nodes}_${5}_${9}_${iter}.temp
			fi
			rm pbm_size=${nodes}_idle=$5_expe=$9_iter=${iter}_general.lp
		done
	done
	rm *.log
	;;
	2 )
	for iter in `seq 1 $8`; do
		for static in `seq 0 1000 $5`; do
			./test ${3} ${static} $9 ${iter}
			rm size=${3}_idle=${static}_expe=$9_iter=${iter}.dat
			if [ "${3}" -lt 26 ]; then
				cplex -c "r pbm_size=${3}_idle=${static}_expe=$9_iter=${iter}_general.lp" "opt"|grep "Objective ="| cut -d "=" -f 2 >> result_${3}_${static}_${9}_${iter}.temp
			else
				echo "" >> result_${3}_${static}_${9}_${iter}.temp
			fi
			rm pbm_size=${3}_idle=${static}_expe=$9_iter=${iter}_general.lp
		done
	done
	rm *.log
	;;
	3 )
	for iter in `seq 1 $8`; do
		./test ${3} $5 $9 ${iter}
		rm size=${3}_idle=${5}_expe=$9_iter=${iter}.dat
		if [ "${3}" -lt 26 ]; then
			cplex -c "r pbm_size=${3}_idle=$5_expe=$9_iter=${iter}_general.lp" "opt"|grep "Objective ="| cut -d "=" -f 2 >> result_${3}_${5}_${9}_${iter}.temp
		else
			echo "" >> result_${3}_${5}_${9}_${iter}.temp
		fi
		rm pbm_size=${3}_idle=$5_expe=$9_iter=${iter}_general.lp
	done
	rm *.log
	;;
	* ) 
	echo "You have tried an expe_number that is not yet implemented."
	;;
esac
