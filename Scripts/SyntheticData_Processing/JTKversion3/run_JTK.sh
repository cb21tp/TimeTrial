#!/bin/bash
while IFS=$'\t' read fileName fileIn fileOut timepoints reps start end interval
do
    here=`pwd`
    JOB=`msub - << EOJ

#MSUB -A p30673 
#MSUB -l nodes=1:ppn=1 
#MSUB -l walltime=04:00:00 
#MSUB -q short
#MSUB -N ${fileName}
#MSUB -m a
#MSUB -M elanness-cohn2017@u.northwestern.edu
#MSUB -j oe

## set your working directory 
cd $here

## job commands; run_Sw1pers is the MATLAB .m file, specified without the .m extension
module load R/3.3.1
Rscript Run_JTK_CYCLE.R $fileName $fileIn $fileOut $timepoints $reps $start $end $interval
EOJ
`
# print out the job id for reference later
echo "JobID = $JOB for parameters $fileName submitted on `date`"
done < masterFilePath_JTK.txt
exit

# make this file executable and then run from the command line
# chmod u+x runSwiper.sh
# ./runSwiper.sh
