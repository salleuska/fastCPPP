#!/bin/bash
#SBATCH --mail-type=ALL                       

#SBATCH -o out/newcomb_variancePlugin_%j.out                 # File to which STDERR will be written, including job ID
######################

Rscript 3_computePluginSE.R \
--filename="sec6_examples/newcomb/results_nCRep_1000_nIter_1000.rds" \
--indexStat=2 

# ## Compute bootstrap variance estimates and coverage
# Rscript 3_computeBootstrapSE.R \
# --filename="sec6_examples/newcomb/results_nCRep_1000_nIter_1000.rds" \
# --indexStat=2 \
# --bootIters=100