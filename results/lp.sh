#!/bin/sh
cd ..
echo "$1\n$2\n$3\n$4\n$5\n$6\n$7" > conf
make opt
./comp conf
cd results
gcc ../LP/lp.c -o test -lglpk -fno-stack-protector 
for r in `seq 1 $7`; do
	for s in `seq 1 $3`; do
		./test $3 $s $r >> result_$3_$s_$r.temp &
	done
done
for r in `seq 1 $7`; do
	for s in `seq 1 $3`; do
		cat result_$3_$s_$r.temp >> result_$3_$4_$6.score;
	done
done
rm ../conf *.temp
