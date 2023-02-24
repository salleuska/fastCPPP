##------------------------------------##
## Newcomb - speed of light data 
## Bad mixing example
##------------------------------------##
modelCode <- nimbleCode({
	for(i in 1:n){
		y[i] ~ dnorm(mu, sd = sigma)
	}

	## noninformative priors
	mu ~ dflat()
	log(sigma) ~ dflat()
})
##------------------------------------##
## Read newcomb data
data <- list(y = read.table(dataPath)$V1)
inits <- list(mu = 0, log_sigma = 2)
constants <- list(n = length(data$y))

## model

model <- nimbleModel(code 		= modelCode, 
					 data 		= data, 
					 inits 		= inits, 
					 constants 	= constants)

cModel 	<- compileNimble(model)

## empty configuration
conf <- configureMCMC(model, nodes = NULL)

## Configure mixing so that the mixing is bad
conf$addSampler(target = "mu", 
                type = "RW", 
                control = list(adaptive = FALSE, scale = 10))

conf$addSampler(target = "log_sigma", 
                type = "RW", 
                control = list(adaptive = FALSE, scale = 0.6, 
                  log = TRUE))


mcmc    <- buildMCMC(conf, monitors = c("mu", "log_sigma"))
cMcmc   <- compileNimble(mcmc, project = model)
samples <- runMCMC(cMcmc, niter = 5000, nburnin = 1000)


## run MCMC on original data if needed
if(runOriginal){
	samples <- runMCMC(cMcmc, niter = 5000, nburnin = 1000)

  ## check mixing - purposely bad
  # plot(samples[, "mu"], type = "l")
  # plot(samples[, "log_sigma"], type = "l")

  # coda::effectiveSize(samples[, "mu"])
  # coda::effectiveSize(samples[, "log_sigma"])

	saveRDS(samples, file = paste0(dirExample, "/MCMCSamples.rds"))
}

## Set values to run CPPP
origMCMCSamples <- readRDS(paste0(dirExample, "/MCMCSamples.rds"))
paramNames 		<- colnames(origMCMCSamples)
dataNames  		<- names(data)
mcmcConfFun	 	<- NULL

##------------------------------------##
## Discrepancies for newcomb data
##------------------------------------##
## 1. minimum observation
minObservation <- nimbleFunction(
  contains = discrepancyFunction_BASE,
  setup = function(model, discrepancyFunctionsArgs){
    dataNames <- discrepancyFunctionsArgs[['dataNames']]
  },
  run = function(){
    dataVals <- values(model, dataNames)
    stat <- min(dataVals)
    returnType(double(0))  # must return a double(0)
    return(stat)
  }
)

## 2. test for asymmetry? need sort function
## |y_(61) - mu| - |y_(6) - mu|

## make a function to call R sort function in nimble
sortR <- nimbleRcall(prototype 	= function(x = double(1)){}, 
					 Rfun 		= "sort", 
					 returnType = double(1))


asymmetryDisc <- nimbleFunction(
  contains = discrepancyFunction_BASE,
  setup = function(model, discrepancyFunctionsArgs){
    dataNames <- discrepancyFunctionsArgs[['dataNames']]
   	meanParam <- discrepancyFunctionsArgs[['meanParam']]
  },
  run = function(){
    dataVals    <- values(model, dataNames)
    meanVal     <- values(model, meanParam)[1]
    sortedVals  <- sortR(dataVals)
    stat 	      <- abs(sortedVals[61] - meanVal) - abs(sortedVals[6] - meanVal)
    returnType(double(0))  # must return a double(0)
    return(stat)
})

discrepancyFunctions <- list(minObservation, asymmetryDisc)
discrepancyFunctionsArgs <- list(list(dataNames = names(data)), 
								 list(dataNames = names(data), 
									  meanParam = "mu"))

