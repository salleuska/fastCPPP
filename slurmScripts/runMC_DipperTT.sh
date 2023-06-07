#!/bin/bash
#SBATCH --mail-type=ALL                       

#SBATCH -o out/MCdipperTT_%j.out                 # File to which STDERR will be written, including job ID
#SBATCH --cpus-per-task=1
#SBATCH --array=1-550
######################

## dipperTT example
Rscript 2_runMonteCarloCPPP.R \
--task=$SLURM_ARRAY_TASK_ID \
--dirExample=sec6_examples/dipperTT \
--dataPath="sec6_examples/dipperTT/dipperData.RData" \
--runOriginal=FALSE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=FALSE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE 
