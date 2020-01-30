#!/bin/bash

while IFS=$'\t' read -r fileName fileIn fileOut interval period peak1 peak2; do
	echo "$fileName submitted on `date`"
	Rscript Run_RAIN.R $fileName $fileIn $fileOut $interval $period $peak1 $peak2
	sleep 20
done <"masterFilePath_RAIN.txt"

exit
