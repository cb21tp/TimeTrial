#!/bin/bash

while IFS=$'\t' read -r fileName fileIn fileOut timepoints reps start end interval; do
	echo "$fileName submitted on `date`"
	Rscript Run_JTK_CYCLE.R $fileName $fileIn $fileOut $timepoints $reps $start $end $interval
	sleep 20
done <"masterFilePath_JTKcycle.txt"

exit
