#!/bin/bash
#SBATCH --mail-type=ALL                       

#SBATCH -o out/simulated_%j.out                 # File to which STDERR will be written, including job ID
######################

Rscript 1_runCPPP.R  \
--dirExample=sec6_examples/capRecapSimulated \
--dataPath="sec6_examples/capRecapSimulated/simulatedCR.RData" \
--runOriginal=FALSE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE
