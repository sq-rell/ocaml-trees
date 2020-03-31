#!/bin/bash

HOLD=goodstudents.txt

for x in `cat $HOLD`
do
	echo " " > array/$x.txt
	for y in `cat $HOLD`
	do
		echo student $y
		t=`dune exec ./apply_eight.exe ../../subs/tokens/$x/hw6.ml ../../subs/tokens/$y/hw6.ml 2> /dev/null`
		echo $t >> array/$x.txt
		echo $t
	done


done
