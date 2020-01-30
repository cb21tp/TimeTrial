#!/bin/bash
while IFS=$'\t' read Name FilePath Period Phase Asym OutputName Reps Wave
do
    JOB=`msub - << EOJ
#!/bin/bash
#MSUB -A p30673
#MSUB -q short
#MSUB -l walltime=4:00:00
#MSUB -M elanness-cohn2017@u.northwestern.edu
#MSUB -m ae
#MSUB -j oe
#MSUB -N BooteJTK_Run_Quest
#MSUB -l nodes=1:ppn=1

# load the version of python you want to use
module load python/anaconda

# Set your working directory 
# This sets it to the directory you're submitting from -- change as appropriate
cd $PBS_O_WORKDIR

# After you change directories with the command above, all files below 
# are then referenced with respect to that directory
python  /home/emn6548/TimeTrial/Scripts/SyntheticData_Processing/BooteJTK-master/BooteJTK-CalcP.py -f=${FilePath} -p=${Period} -s=${Phase} -a=${Asym} -x=${OutputName} -r=${Reps} -z=25 -w=${Wave} 
EOJ
`

# print out the job id for reference later
echo "JobID = ${JOB} for parameters ${Name} submitted on `date`"
done < masterFilePaths_BooteJTK_Concat.txt
exit   

# make this file executable and then run from the command line
# chmod u+x submit.sh
# ./submit.sh