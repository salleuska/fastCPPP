#!/bin/bash
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=spaganin@hsph.harvard.edu
#SBATCH -o out/simulated_%j.out                 # File to which STDERR will be written, including job ID
######################

Rscript 1_runCPPP.R  \
--dirExample=capRecapSimulated \
--dataPath="capRecapSimulated/simulatedCR.RData" \
--runOriginal=FALSE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE
