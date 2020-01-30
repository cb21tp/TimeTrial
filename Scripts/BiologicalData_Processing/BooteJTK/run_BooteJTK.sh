#!/bin/bash

while IFS=$'\t' read -r Name FilePath Period Phase Asym OutputName Reps Wave; do
	echo "$fileName submitted on `date`"
	python ~/Desktop/CyclingMethodsReview/DataAnalysis/Scripts/Methods/BooteJTK-master/BooteJTK-CalcP.py -f=${FilePath} -p=${Period} -s=${Phase} -a=${Asym} -x=${OutputName} -r=${Reps} -z=25 -w=${Wave} 
	sleep 20
done <"masterFilePath_BooteJTK.txt"

exit
