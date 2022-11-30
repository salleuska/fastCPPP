#!/bin/bash
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=spaganin@hsph.harvard.edu
#SBATCH -o newcomb_%j.out                 # File to which STDERR will be written, including job ID
######################

## newcomb example
Rscript 1_runCPPPExample.R  \
--dirExample=newcomb \
--dataPath="newcomb/light.txt" \
--runOriginal=TRUE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE \

Rscript 1_runCPPPExample.R  \
--dirExample=newcomb \
--dataPath="newcomb/light.txt" \
--runOriginal=FALSE \
--nCalibrationReplicates=1000 \
--nIterMCMC=5000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE 