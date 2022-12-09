#!/bin/bash
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=spaganin@hsph.harvard.edu
#SBATCH -o out/newcombBad_%j.out                 # File to which STDERR will be written, including job ID
######################

## newcomb example
Rscript 1_runCPPP.R  \
--dirExample=newcombBadMixing \
--dataPath="newcomb/light.txt" \
--runOriginal=FALSE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE
