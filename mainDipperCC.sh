#############################################
## Dipper data - CC model
#############################################
#############################################
## 1) Compute CPPP 
#############################################

## Compute CPPP
Rscript 1_runCPPP.R \
--dirExample=dipperCC \
--dataPath="dipperCC/dipperData.RData" \
--runOriginal=TRUE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE \

## Compute "naive" CPPP
## using the same number of MCMC samples as in the original MCMC 

# Rscript 1_runCPPP.R \
# --dirExample=dipperCC \
# --dataPath="dipperCC/dipperData.RData" \
# --runOriginal=TRUE \
# --nCalibrationReplicates=1000 \
# --nIterMCMC=10000 \
# --returnSamples=TRUE \
# --returnDiscrepancies=TRUE \
# --calcDisc=TRUE 

#############################################
## 2) Compute CPPP variance via Monte Carlo 
#############################################

Rscript 2_runMonteCarloCPPP.R \
--task=1 \
--dataPath="dipperCC/dipperData.txt" \
--dirExample=dipperCC \
--runOriginal=TRUE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=FALSE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE 

#############################################
## 3) Compute CPPP variance estimates
#############################################
## Compute Monte Carlo baseline (baseline)
Rscript 3_computeMCSE.R \
--dirExample="dipperCC/montecarlo/" \
--indexStat=1


## Compute plug-in variance estimate and coverage
Rscript 3_computePluginSE.R \
--filename="dipperCC/results_nCRep_1000_nIter_1000.rds" \
--indexStat=1 

## Compute bootstrap variance estimates and coverage
Rscript 3_computeBootstrapSE.R \
--filename="dipperCC/results_nCRep_1000_nIter_1000.rds" \
--indexStat=1 \
--bootIters=100

#############################################
## 4) Plot results
#############################################

Rscript 4_plotResults.R --dirExample="dipperCC" --plotTitle="Dipper example - C/C model"
