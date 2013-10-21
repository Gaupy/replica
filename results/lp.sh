#!/bin/sh
cd ..
echo "$1
$2
$3
$4
$5
$6
$7" > conf
make opt
./comp conf
cd results
gcc ../LP/lp.c -o test -lglpk -fno-stack-protector 
for r in `seq 1 $7`; do
	for s in `seq 1 $3`; do
		./test $3 $s $r >> result_$3.score
	done
done
rm ../conf
