#!/bin/bash
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=spaganin@hsph.harvard.edu
#SBATCH -o out/dipperTT_%j.out                 # File to which STDERR will be written, including job ID
######################

Rscript 1_runCPPPExample.R  \
--dirExample=dipperTT \
--runOriginal=TRUE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE

Rscript 1_runCPPPExample.R  \
--dirExample=dipperTT \
--runOriginal=FALSE \
--nCalibrationReplicates=1000 \
--nIterMCMC=10000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE