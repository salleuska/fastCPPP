#############################################
## Newcomb data - bad mixing
#############################################
#############################################
## 1) Compute CPPP 
#############################################

## Compute CPPP
Rscript 1_runCPPP.R \
--dirExample=sec6_examples/newcombBadMixing \
--dataPath="sec6_examples/newcombBadMixing/light.txt" \
--runOriginal=TRUE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE \

## Compute "naive" CPPP
## using the same number of MCMC samples as in the original MCMC 

# Rscript 1_runCPPP.R \
# --dirExample=sec6_examples/newcombBadMixing \
# --dataPath="sec6_examples/newcombBadMixing/light.txt" \
# --runOriginal=FALSE \
# --nCalibrationReplicates=1000 \
# --nIterMCMC=5000 \
# --returnSamples=TRUE \
# --returnDiscrepancies=TRUE \
# --calcDisc=TRUE 

#############################################
## 2) Compute CPPP variance via Monte Carlo 
#############################################

Rscript 2_runMonteCarloCPPP.R \
--task=1 \
--dataPath="newcomb/light.txt" \
--dirExample=sec6_examples/newcombBadMixing \
--runOriginal=TRUE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=FALSE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE 

#############################################
## 3) Compute CPPP variance estimates
#############################################
## Compute plug-in variance estimate and coverage

Rscript 3_computePluginSE.R \
--filename="sec6_examples/newcombBadMixing/results_nCRep_1000_nIter_1000.rds" \
--indexStat=2 

## Compute bootstrap variance estimates and coverage
Rscript 3_computeBootstrapSE.R \
--filename="sec6_examples/newcombBadMixing/results_nCRep_1000_nIter_1000.rds" \
--indexStat=2 \
--bootIters=100

#############################################
## 4) Plot results
#############################################

Rscript 4_plotResults.R --dirExample="sec6_examples/newcombBadMixing" --plotTitle="Newcomb example - bad mixing"
