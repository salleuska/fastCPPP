## This script perform calulation of the CPPP
## for a given example
#######################
args <- R.utils::commandArgs(asValue=TRUE)

## set up args for the  script 
## --dirExample               ## directory for input/output
## --dataPath                 ## path to data
## --runOriginal              ## if TRUE then run first original MCMC (details in the model scripts)
## --nCalibrationReplicates   ## number of calibration replicates
## --nIterMCMC                ## number of mcmc samples per calibration replicates
## --returnSamples            ## if TRUE return all samples
## --returnDiscrepancies      ## if TRUE return all computed discrepancies
## --calcDisc                 ## if TRUE compute PPP
#######################
## Example
# args <- list()
# args$dirExample="newcomb" 
# args$dataPath="newcomb/light.txt" 
# args$runOriginal=TRUE 
# args$nCalibrationReplicates=1000 
# args$nIterMCMC=1000 
# args$returnSamples=TRUE 
# args$returnDiscrepancies=TRUE 
# args$calcDisc=TRUE 
#######################
##---------------------------------------- ##
library(nimble)

## Initialize discrepancy functions
discrepancyFunctions <- NULL
discrepancyFunctionsArgs <- NULL

##  read CPPP code
source("calculateCPPP.R")

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

## Settings for MCMC
MCMCcontrol <- list(niter   = nIterMCMC,
                    nburnin = 0, 
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


## Set log-likelihood as default discrepancy measure to run
if(is.null(discrepancyFunctions)){
  discrepancyFunctions <- list(logLikDiscFunction)
  discrepancyFunctionsArgs <- list(list(dataNames = names(data)))
} else {
  ## add default discrepancy (logLikelihood)
  discrepancyFunctions <- append(discrepancyFunctions, logLikDiscFunction)
  discrepancyFunctionsArgs[[length(discrepancyFunctionsArgs) + 1]] <- list(dataNames = names(data))
}


## run calibration
time <- system.time(
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
)
out$time <- time
## save results
saveRDS(out, file = paste0(dirExample, "/results_nCRep_", nCalibrationReplicates, "_nIter_", nIterMCMC, ".rds"))


