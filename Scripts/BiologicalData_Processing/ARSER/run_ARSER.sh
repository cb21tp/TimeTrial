#!/bin/bash

while IFS=$'\t' read -r fileName fileIn fileOut; do
	echo "$fileName submitted on `date`"
	Rscript runArser.R $fileName $fileIn $fileOut
	sleep 20
done <"masterFilePath_Arser.txt"

exit
