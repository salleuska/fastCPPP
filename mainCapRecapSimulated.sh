#############################################
## Simulated example - capture recapture
#############################################
#############################################
## 1) Compute CPPP 
#############################################

## Compute CPPP
Rscript 1_runCPPP.R \
--dirExample=sec6_examples/capRecapSimulated \
--dataPath="sec6_examples/capRecapSimulated/simulatedCR.RData" \
--runOriginal=TRUE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE \

#############################################
## 2) Compute CPPP variance via Monte Carlo 
#############################################

Rscript 2_runMonteCarloCPPP.R \
--task=1 \
--dataPath="sec6_examples/capRecapSimulated/light.txt" \
--dataPath="sec6_examples/capRecapSimulated/simulatedCR.RData" \
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
--dirExample="sec6_examples/capRecapSimulated/montecarlo/" \
--indexStat=1


## Compute plug-in variance estimate and coverage
Rscript 3_computePluginSE.R \
--filename="sec6_examples/capRecapSimulated/results_nCRep_1000_nIter_1000.rds" \
--indexStat=1 

## Compute bootstrap variance estimates and coverage
Rscript 3_computeBootstrapSE.R \
--filename="sec6_examples/capRecapSimulated/results_nCRep_1000_nIter_1000.rds" \
--indexStat=1 \
--bootIters=100

#############################################
## 4) Plot results
#############################################

Rscript 4_plotResults.R --dirExample="sec6_examples/capRecapSimulated" --plotTitle="Simulated example - T/T model"
