#!/usr/bin/env Rscript
args <- R.utils::commandArgs(asValue=TRUE)
#######################
## set up args for the  script 
## -- task (slurmID)
## -- dirExample
## -- dataPath
## -- runOriginal
## -- nCalibrationReplicates
## -- nIterMCMC
## -- returnSamples
## -- returnDiscrepancies
## -- calcDisc
#######################
## This script perform calulation of the CPPP multiple times to obtain
## monte carlo estiamtes; the script is set up to run on a cluster using slurm 
#######################
library(nimble)

## Initialize discrepancy functions
discrepancyFunctions <- NULL
discrepancyFunctionsArgs <- NULL

##  read CPPP code
source("codeCPPP.R")

##  read registeredDiscrepancy
source("registeredDiscrepancies.R")
##---------------------------------------- ##
if(is.null(args$dirExample)){
	stop("provide directory path for the example to run")
} else {
	dirExample <- args$dirExample
}

if(is.null(args$dataPath)){
  stop("provide directory path for the data")
} else {
  dataPath <- args$dataPath
}

if(is.null(args$nCalibrationReplicates)){
	nCalibrationReplicates <- 1000 
	warning("Default to ", nCalibrationReplicates, " calibration replicates")
} else {
	nCalibrationReplicates <- as.numeric(args$nCalibrationReplicates)
}

if(is.null(args$nIterMCMC)) {
	nIterMCMC <- 200
	warning("Default to ", nIterMCMC, " calibration replicates")
} else {
	nIterMCMC <- as.numeric(args$nIterMCMC)
}

task <- as.numeric(args$task)

## Settings for MCMC
MCMCcontrol <- list(niter   = nIterMCMC + nIterMCMC/10,
                    nburnin = nIterMCMC/10, 
                    thin 	= 1)

## Flags
returnSamples 		  <- if(is.null(args$returnSamples)) TRUE 		else as.logical(args$returnSamples) 
returnDiscrepancies <- if(is.null(args$returnDiscrepancies)) TRUE 	else as.logical(args$returnDiscrepancies) 
calcDisc 			      <- if(is.null(args$calcDisc)) TRUE 				else as.logical(args$calcDisc) 
parallel 			      <- if(is.null(args$parallel)) FALSE 			else as.logical(args$parallel) 
nCores              <- if(is.null(args$nCores)) 1 					else as.numeric(args$nCores) 

## run original mcmc model? 
runOriginal 		<- if(is.null(args$runOriginal)) FALSE 			else as.logical(args$runOriginal) 

## Source model 
source(paste0(dirExample, "/model.R"))


## Set loglkelihood as default discrepancy measure to run
if(is.null(discrepancyFunctions)){
  discrepancyFunctions <- list(logLikDiscFunction)
  discrepancyFunctionsArgs <- list(list(dataNames = names(data)))
} else {
  ## add default discrepancy (logLikelihood)
  discrepancyFunctions <- append(discrepancyFunctions, logLikDiscFunction)
  discrepancyFunctionsArgs[[length(discrepancyFunctionsArgs) + 1]] <- list(dataNames = names(data))
}

resDisc <- list()
set.seed(task)
## run calibration
out <- runCalibration(## this comes from model.R file
                       model = model,
                      dataNames = dataNames,
                      paramNames = paramNames,
                      origMCMCSamples = origMCMCSamples,         
                      mcmcConfFun = mcmcConfFun,
                      discrepancyFunctions = discrepancyFunctions, 
                      discrepancyFunctionsArgs = discrepancyFunctionsArgs,
                      ## this comes from args or defaults
                      nCalibrationReplicates = nCalibrationReplicates,
                      MCMCcontrol = MCMCcontrol,                  
                      returnSamples = returnSamples,
                      returnDiscrepancies = returnDiscrepancies,
                      calcDisc = calcDisc, 
                      parallel = parallel, 
                      nCores   = nCores) 

saveRDS(out, file = paste0(dirExample, "/montecarlo/res_", task, ".rds"))


