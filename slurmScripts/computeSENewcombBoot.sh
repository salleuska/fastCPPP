#!/bin/bash
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=spaganin@hsph.harvard.edu
#SBATCH -o out/newcomb_varianceBoot_%j.out                 # File to which STDERR will be written, including job ID
######################

# Rscript 3_computePluginSE.R \
# --filename="newcomb/results_nCRep_1000_nIter_1000.rds" \
# --indexStat=2 

## Compute bootstrap variance estimates and coverage
Rscript 3_computeBootstrapSE.R \
--filename="newcomb/results_nCRep_1000_nIter_1000.rds" \
--indexStat=2 \
--bootIters=100