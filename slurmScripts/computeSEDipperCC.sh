#!/bin/bash
#SBATCH --mail-type=ALL                       
#SBATCH --mail-user=spaganin@hsph.harvard.edu
#SBATCH -o out/dipperCC_variacePlugin_%j.out                 # File to which STDERR will be written, including job ID
######################

Rscript 3_computePluginSE.R \
--filename="dipperCC/results_nCRep_1000_nIter_1000.rds" \
--indexStat=1

# ## Compute bootstrap variance estimates and coverage
# Rscript 3_computeBootstrapSE.R \
# --filename="dipperCC/results_nCRep_1000_nIter_1000.rds" \
# --indexStat=1 \
# --bootIters=100