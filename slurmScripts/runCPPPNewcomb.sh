#!/bin/bash
#SBATCH --mail-type=ALL                       
#SBATCH -o out/newcomb_%j.out                 # File to which STDERR will be written, including job ID
######################

## newcomb example
Rscript 1_runCPPP.R \
--dirExample=sec6_examples/newcomb \
--dataPath="sec6_examples/newcomb/light.txt" \
--runOriginal=FALSE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE \

Rscript 1_runCPPP.R \
--dirExample=sec6_examples/newcomb \
--dataPath="sec6_examples/newcomb/light.txt" \
--runOriginal=FALSE \
--nCalibrationReplicates=1000 \
--nIterMCMC=5000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE 