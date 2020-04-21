#!/bin/bash



interest=remove

HOLD=array/hw4/$interest.csv


rm $HOLD

x=1
while [ $x -le 71 ]
do
	
	while ! test -e ../../subs/tokens/$x/hw4.ml
	do
		x=$(( $x + 1 ))
		echo >> $HOLD 
	done
	
	echo student $x
	cd ../src
	line=""
	y=1
	while [ $y -le 71 ]
	do
	
		while ! test -e ../../subs/tokens/$y/hw4.ml
		do
			y=$(( $y + 1 ))
			line=$line,
		done
	
		t=`dune exec ./apply_eight.exe ../../subs/tokens/$x/hw4.ml ../../subs/tokens/$y/hw4.ml $interest 2> /dev/null`

		if ((t==0)) && ((y<x)) && [[ $t != "" ]]
		then
			line=`sed "${y}q;d" ../helpers/$HOLD`
			y=72
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
