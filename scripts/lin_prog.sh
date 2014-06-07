#!/bin/sh
cd ../results
gcc ../LP/new_lp.c -o test -lglpk -fno-stack-protector 
case $9 in
	0|4 )
	for iter in `seq 1 $8`; do
		for nodes in `seq 1 $3`; do
			./test ${nodes} $5 $4 $9 ${iter}
			rm size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}.dat
			if [ "${nodes}" -lt 31 ]; then
				cplex -c "r pbm_size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp" "opt"|grep "Objective ="| cut -d "=" -f 2 >> result_${nodes}_${5}_${4}_${9}_${iter}.temp
			else
				echo "" >> result_${nodes}_${5}_${4}_${9}_${iter}.temp
			fi
			rm pbm_size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp
		done
	done
	rm *.log
	;;
	1|5 )
	for iter in `seq 1 $8`; do
		for nodes in `seq 1 $3`; do
			./test ${nodes} $5 $4 $9 ${iter}
			rm size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}.dat
			if [ "${nodes}" -lt 26 ]; then
				cplex -c "r pbm_size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp" "opt"|grep "Objective ="| cut -d "=" -f 2 >> result_${nodes}_${5}_${4}_${9}_${iter}.temp
			else
				echo "" >> result_${nodes}_${5}_${4}_${9}_${iter}.temp
			fi
			rm pbm_size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp
		done
	done
	rm *.log
	;;
	2|6 )
	for iter in `seq 1 $8`; do
		r=$(echo "$5/20" | bc)
		for static in `seq 0 $r $5`; do
			./test ${3} ${static} $4 $9 ${iter}
			rm size=${3}_idle=${static}_speeds=$4_expe=$9_iter=${iter}.dat
			if [ "${3}" -lt 26 ]; then
				cplex -c "r pbm_size=${3}_idle=${static}_expe=$9_iter=${iter}_general.lp" "opt"|grep "Objective ="| cut -d "=" -f 2 >> result_${3}_${static}_${4}_${9}_${iter}.temp
			else
				echo "" >> result_${3}_${static}_${4}_${9}_${iter}.temp
			fi
			rm pbm_size=${3}_idle=${static}_expe=$9_iter=${iter}_general.lp
		done
	done
	rm *.log
	;;
	3|7 )
	for iter in `seq 1 $8`; do
		./test ${3} $5 $4 $9 ${iter}
		rm size=${3}_idle=$5_speeds=$4_expe=$9_iter=${iter}.dat
		if [ "${3}" -lt 26 ]; then
			cplex -c "r pbm_size=${3}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp" "opt"|grep "Objective ="| cut -d "=" -f 2 >> result_${3}_${5}_${4}_${9}_${iter}.temp
		else
			echo "" >> result_${3}_${5}_${4}_${9}_${iter}.temp
		fi
		rm pbm_size=${3}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp
	done
	rm *.log
	;;
	8 )
	for iter in `seq 1 $8`; do
		r=$(echo "$3/20" | bc)
		for nodes in `seq $r $r $3`; do
			./test ${nodes} $5 $4 $9 ${iter}
#			rm size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}.dat
			rm pbm_size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp
			echo "">> result_${nodes}_${5}_${4}_${9}_${iter}.temp
		done
	done
	;;
	11 )
	for iter in `seq 1 $8`; do
		r=$(echo "$3/20" | bc)
		for nodes in `seq $r $r $3`; do
			./test ${nodes} $5 $4 $9 ${iter}
			rm size=${nodes}_idle=${5}_speeds=$4_expe=$9_iter=${iter}.dat
			rm pbm_size=${nodes}_idle=$5_speeds=$4_expe=$9_iter=${iter}_general.lp
			echo "">> result_${nodes}_${5}_${4}_${9}_${iter}.temp
		done
	done
	;;
	* ) 
	echo "You have tried an expe_number that is not yet implemented."
	;;
esac
