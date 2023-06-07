#!/bin/bash
#SBATCH --mail-type=ALL                       
#SBATCH -o out/MCnewcomb_%j.out                 # File to which STDERR will be written, including job ID
#SBATCH --cpus-per-task=1
#SBATCH --array=1-550
######################

## newcomb example
Rscript 2_runMonteCarloCPPP.R \
--task=$SLURM_ARRAY_TASK_ID \
--dataPath="sec6_examples/newcomb/light.txt" \
--dirExample=sec6_examples/newcomb \
--runOriginal=TRUE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=FALSE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE 

