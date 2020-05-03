#!/bin/bash



interest=remove

HOLD=array/hw4/$interest.csv

DATA=../../subs/tokens/

HW_FILE=hw4.ml

max_students=71

rm $HOLD

x=1
while [ $x -le $max_students ]
do
	
	while ! test -e $DATA$x/$HW_FILE
	do
		x=$(( $x + 1 ))
		echo >> $HOLD 
	done
	
	echo student $x
	cd ../src
	line=""
	y=1
	while [ $y -le $max_students ]
	do
	
		while ! test -e $DATA$y/$HW_FILE
		do
			y=$(( $y + 1 ))
			line=$line,
		done
	
		t=`dune exec ./apply_eight.exe $DATA$x/$HW_FILE $DATA$y/$HW_FILE $interest 2> /dev/null`

		if ((t==0)) && ((y<x)) && [[ $t != "" ]]
		then
			line=`sed "${y}q;d" ../helpers/$HOLD`
			y=$(( $max_students + 1 ))
		else
			line=$line$t,
			y=$(( $y + 1 ))
		fi
	done
	
	echo $line
	
	cd ../helpers
	echo $line >> $HOLD
	
	
	
	x=$(( $x + 1 ))

done
