# library(nimble)

## define hierarchical model
modelCode <- nimbleCode({
    phi ~ dunif(0,1)
    p ~ dunif(0,1)
    for (i in 1:nind) {
        x[i, first[i]] <- 1
        for (t in (first[i] + 1):k) {
            mu_x[i, t] <- phi * x[i, t-1]
            mu_y[i, t] <- p * x[i, t]
            x[i, t] ~ dbin(mu_x[i, t], 1)
            y[i, t] ~ dbin(mu_y[i, t], 1)
        }
    }
})


## ----------------##
 # dirExample <- "dipperCC"
## ----------------##
## load data
## ----------------##
load(dataPath)
## define model constants, data, and initial values
constants <- list(k=k, nind=nind, first=first)
data <- list(y=y)
inits <- list(phi=0.6, p=0.9, x=x_init)

## ----------------##
## create model object
## ----------------##
model <- nimbleModel(modelCode, 
                     data = data, 
                     constants = constants, 
                     inits = inits)

cModel  <- compileNimble(model)

mcmcConf    <- configureMCMC(model)
mcmc        <- buildMCMC(mcmcConf)
cMcmc       <- compileNimble(mcmc, project = model)

## run MCMC on original data if needed
if(runOriginal){

  set.seed(2021)
  samples     <- runMCMC(cMcmc, niter = 10000, nburnin = 1000)

  saveRDS(samples, file = paste0(dirExample, "/MCMCSamples.rds"))
}

origMCMCSamples <- readRDS(paste0(dirExample, "/MCMCSamples.rds"))
paramNames    <- colnames(origMCMCSamples)
dataNames     <- c(model$expandNodeNames("x")[model$isStoch('x')],
                   model$expandNodeNames("y")[model$isStoch('y')])
mcmcConfFun   <- NULL

## Same results of Table 6 in BCM paper
# apply(origMCMCSamples, 2, mean)
# apply(origMCMCSamples, 2, sd)
##############################
## Define discrepancy function - 
library(R2ucare) ## for function marray that allows to get
# get number of released individuals (R), 
# the m-array (m) and 
# the number of individuals never seen again (never)
# mm <- marray(dip.hist,dip.freq)

source("registeredDiscrepancies.R")
source("codeCPPP.R")

getMArray <- function(history, indFreq) return(marray(history, indFreq)$m[,,1])
getReleases <- function(history, indFreq) return(marray(history, indFreq)$R[,1])

## make a function to call R sort function in nimble
getMArrayR <- nimbleRcall(prototype   = function(history = double(2), indFreq = double(1)){}, 
                           Rfun       = "getMArray", 
                           returnType = double(2))
getReleasesR <- nimbleRcall(prototype   = function(history = double(2), indFreq = double(1)){}, 
                           Rfun       = "getReleases", 
                           returnType = double(1))

## discrepancy based on observed captures and expected captures

discFreemanTukey <- nimbleFunction(
  contains = discrepancyFunction_BASE,
  setup = function(model, discrepancyFunctionsArgs){
    dataNames <- discrepancyFunctionsArgs[['dataNames']]
    survProbName  <- discrepancyFunctionsArgs[['survProbName']]
    captureProbName  <- discrepancyFunctionsArgs[['captureProbName']]
  },
  run = function(){
    ## Getting data as matrix!
    dataVals    <- model[[dataNames]]
    # dataVals    <- values(model, dataNames)
    survProbVal  <- values(model, survProbName)
    captureProbVal  <- values(model, captureProbName)

    # get number of individuals and observations times
    nind <- dim(dataVals)[1]
    nTimes <- dim(dataVals)[2]
    
    ## if the survival or capture probabilty is constant just make a vector
    if(length(survProbVal) == 1) survProbVal <- rep(survProbVal, nTimes - 1)
    if(length(captureProbVal) == 1) captureProbVal <- rep(captureProbVal, nTimes)


    ## get info form onserved datas
    indFreq <- rep(1, nind)
    obsCaptures <- getMArrayR(dataVals, indFreq)
    R <- getReleasesR(dataVals, indFreq)

    # ## get expected captures
    # expectedCaptures <- matrix(0, nrow = 6, ncol = 6)
    # diag(expectedCaptures) <- R * phi*p[2:7]
    # for(i in 1:5){
    #   for(j in (i +1):6){
    #     logPhiXptilde <- 0
    #     for(k in (i + 1):j) logPhiXptilde <- logPhiXptilde + log(phi[k]) + log(1 - p[k])
    #     expProb <- (phi[i]*p[j + 1])*exp(logPhiXptilde)
    #     expectedCaptures[i, j] <- R[i] * expProb
    #   }
    # }

    ## get statistics observed - expected
    ## sum(sqrt(y_ij) - sqrt(e_ij))^2
    stat <- 0
    ## add first values on the diagonal
    for(l in 1:(nTimes -1))  stat <- stat + (sqrt(obsCaptures[l,l]) - sqrt(R[l] * survProbVal[l]*captureProbVal[l + 1]))^2
    
    ## add non-diagonal values
    for(i in 1:5){
      for(j in (i +1):6){
        logPhiXptilde <- 0
        for(k in (i + 1):j) {
          logPhiXptilde <- logPhiXptilde + log(survProbVal[k]) + log(1 - captureProbVal[k])
        }
        expProb <- (survProbVal[i]*captureProbVal[j + 1])*exp(logPhiXptilde)
        expectedCaptures <- R[i] * expProb
        stat <- stat + (sqrt(obsCaptures[i,j]) - sqrt(expectedCaptures))^2
      }
    }

    returnType(double(0))  # must return a double(0)
    return(stat)
})


discrepancyFunctions <- list(discFreemanTukey)
discrepancyFunctionsArgs <- list(list(dataNames = "y", survProbName = "phi", captureProbName = "p"))


####################################################################################
## Check main function
# returnSamples           <- TRUE      
# returnDiscrepancies     <- TRUE    
# calcDisc                <- TRUE          
# parallel                <- FALSE
# nCores                  <- 1
# nCalibrationReplicates  <- 20
# MCMCcontrol             <- list(niter = 200, nburnin = 0, thin = 1)

# undebug(runCalibration)
# options(error = recover)
# options(error = NULL)
# out <- runCalibration(model = model,    ## I am assuming this uncompiled but I do not have checks in the runCalibration code
#                       dataNames = dataNames,
#                       paramNames = paramNames,
#                       origMCMCSamples = origMCMCSamples,         
#                       mcmcConfFun = mcmcConfFun,
#                       discrepancyFunctions = discrepancyFunctions, 
#                       discrepancyFunctionsArgs = discrepancyFunctionsArgs,
#                       nCalibrationReplicates = nCalibrationReplicates,
#                       MCMCcontrol = MCMCcontrol,                  
#                       returnSamples = returnSamples,
#                       returnDiscrepancies = returnDiscrepancies,
#                       calcDisc = calcDisc, 
#                       parallel = parallel, 
#                       nCores   = nCores) 



# names(out)


####################################################################################
## checks for discrepancy
# discImpl <- discFreemanTukey(model, discrepancyFunctionsArgs = list(dataNames = "y",
#                                                                     survProbName = "phi",
#                                                                     captureProbName = "p")) 
# # undebug(discImpl$run)
# discImpl$run()

# # nimbleOptions(pauseAfterWritingFiles = FALSE)
# CdiscImpl <- compileNimble(discImpl, project = model)
# CdiscImpl$run()

# dataNames <- "y"
# paramNames <- colnames(origMCMCSamples)

## simulate from posterior predictive
# simNodes <- unique(c(model$expandNodeNames(dataNames), 
#       model$getDependencies(paramNames, includeData = FALSE, self=FALSE)))

# modelCalcDisc <- calcDiscrepancies(model = model,
#                                    dataNames = dataNames, 
#                                    paramNames = paramNames,
#                                    simNodes   = simNodes,
#                                    discrepancyFunctions = list(discFreemanTukey), 
#                                    discrepancyFunctionsArgs = list(list(dataNames = "y",
#                                                                     survProbName = "phi",
#                                                                     captureProbName = "p")))

# # out <- modelCalcDisc$run(origMCMCSamples[1:10, ])
# # out

# cModelCalcDisc <- compileNimble(modelCalcDisc, project = model)
# out <- cModelCalcDisc$run(origMCMCSamples)

# ## PPP in BMC paper = 0.069
# ## see table 7, C/C model, prior 1 (idependent uniform priors)
# ## PPP in Hjort paper = 0.60 - they don't know why there is a small difference
# mean(out[,,2] > out[,,1])

##############################################################
## Sanity check for computing expected number of animals
# dataNames <- "y"
# dataVals    <- model[[dataNames]]
# nind <- dim(dataVals)[1]
# indFreq <- rep(1, nind)

# mArray <- getMArray(dataVals, indFreq)
# R <- getReleases(dataVals, indFreq)

# phi <- rep(model[["phi"]], 6)
# p <- rep(model[["p"]], 7)

# expectedCaptures <- matrix(0, nrow = 6, ncol = 6)
# diag(expectedCaptures) <- R * phi*p[2:7]
# for(i in 1:5){
#   for(j in (i +1):6){
#     logPhiXptilde <- 0
#     for(k in (i + 1):j) logPhiXptilde <- logPhiXptilde + log(phi[k]) + log(1 - p[k])
#     expProb <- (phi[i]*p[j + 1])*exp(logPhiXptilde)
#     expectedCaptures[i, j] <- R[i] * expProb
#   }
# }

# # res
# sum((sqrt(mArray) - sqrt(expectedCaptures))^2)

## Sanity check with formulas 
# surv <- paste0("phi_", 1:6)
# obs <- paste0("p_", 1:7)
# probOutChar <- matrix("", nrow = 6, ncol = 6)
# diag(probOutChar) <- paste0(surv, " x " , obs[2:7])
# for(i in 1:5){
#   for(j in (i +1):6){
#     phiXptilde <- ""
#     for(k in (i + 1):j) phiXptilde <- paste0(phiXptilde, " x ", surv[k], " x (1 - ", obs[k], ")")
#     probOutChar[i, j] <- paste0(surv[i], "x ", obs[j + 1], "x ", phiXptilde)
#   }
# }
