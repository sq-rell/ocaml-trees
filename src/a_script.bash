#!/bin/bash

HOLD=goodstudents.txt

echo " " > $HOLD


x=1

while [ $x -le 71 ]
do
	nextFile=../../subs/tokens/$x/hw$1.ml
	if test -f "$nextFile"
	then
		if dune exec ./counts.exe $nextFile 2> /dev/null | grep 9 > /dev/null
		then
			echo $x >> $HOLD
		fi
	fi
	
	x=$(( $x + 1 ))
done
