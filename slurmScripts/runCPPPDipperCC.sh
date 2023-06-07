#!/bin/bash
#SBATCH --mail-type=ALL                       

#SBATCH -o out/dipperCC_%j.out                 # File to which STDERR will be written, including job ID
######################

Rscript 1_runCPPP.R  \
--dirExample=sec6_examples/dipperCC \
--dataPath="sec6_examples/dipperCC/dipperData.RData" \
--runOriginal=FALSE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE

Rscript 1_runCPPP.R \
--dataPath="sec6_examples/dipperCC/dipperData.RData" \
--dirExample=sec6_examples/dipperCC \
--runOriginal=FALSE \
--nCalibrationReplicates=1000 \
--nIterMCMC=10000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE
