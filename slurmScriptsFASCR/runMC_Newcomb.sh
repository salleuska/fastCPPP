#!/bin/bash
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=spaganin@hsph.harvard.edu
#SBATCH -o out/MCnewcomb_%j.out                 # File to which STDERR will be written, including job ID
#SBATCH --cpus-per-task=1
#SBATCH --array=1-550
######################

## newcomb example
Rscript 2_runMonteCarloCPPP.R \
--task=$SLURM_ARRAY_TASK_ID \
--dataPath="newcomb/light.txt" \
--dirExample=newcomb \
--runOriginal=TRUE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=FALSE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE 

