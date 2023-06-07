##-----------------------------------------#
## Computational methods for fast Bayesian model assessment via calibrated posterior p-values.
## Sally Paganin
## last update: Nov 2022
## R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics"
## nimble version 0.12
##-----------------------------------------#


## This function calculate observed and replicated discrepancies for 
## a given model, mcmc samples, and list of discrepancies with arguments
##
## model                        nimbleModel 
## dataNames                    names of data nodes
## paramNames                   names of parameter nodes
## discrepancyFunction          Could be one nimbleFunction or a list of them
## discrepancyFunctionsArgs     List of list
##
## If it is only one discrepancyFunction, discrepancyFunctionsArgs is a list of arguments
## If there are multiple discrepancies I need a list of lists


calcDiscrepancies <- nimbleFunction(
    setup = function(model,
                     dataNames,
                     paramNames,
                     simNodes,
                     discrepancyFunctions, ## Could be one nimbleFunction or a list of them
                     discrepancyFunctionsArgs){

        ## Expand dataNames and paramNames
        dataNodes <- model$expandNodeNames(dataNames)
        paramNodes <- model$expandNodeNames(paramNames)
        ## Get parameter and data dependencies
        paramDependencies <- model$getDependencies(paramNames)
        dataDependencies  <- model$getDependencies(dataNames)
        ## nodes to use for posterior predictive
        simNodes <- model$expandNodeNames(simNodes)
        simNodes <- model$topologicallySortNodes(simNodes)

        ## Support multiple discrepancies 
        discrepancyFunctionList <- nimbleFunctionList(discrepancyFunction_BASE)
        
        if(!is.list(discrepancyFunctions))
            discrepancyFunctions <- list(discrepancyFunctions)

        ## This should be a list of lists
        if(!is.list(discrepancyFunctionsArgs))
            discrepancyFunctionsArgs <- list(discrepancyFunctionsArgs)

        ## This checks that all elements of 'discrepancyFunctionsArgs' are a list
        if(!all(sapply(discrepancyFunctionsArgs[1:length(discrepancyFunctionsArgs)], is.list))) 
            stop("Elements of ", discrepancyFunctionsArgs, "have to be a list")

        ## This check that 'discrepancyFunctionsArgs' and 'discrepancyFunctions' have the same lenght
        if(length(discrepancyFunctions) != length(discrepancyFunctionsArgs))
                stop("length of ", discrepancyFunctions, "must match legth of ", discrepancyFunctionsArgs, "list.")

        for(i in seq_along(discrepancyFunctions))
            discrepancyFunctionList[[i]] <- discrepancyFunctions[[i]](model, discrepancyFunctionsArgs[[i]])

        nDiscrepancyFuns <- length(discrepancyFunctions)

    },
    run = function(MCMCOutput = double(2)){

        origDataValues  <- values(model, dataNodes)
        origParamValues <- values(model, paramNodes)
        nSamples <- dim(MCMCOutput)[1]

        ## results[j, i, k]: 
        ## j = discrepancy index, 
        ## i = MCMC iteration, 
        ## k = 1 for observed or 2 for simulated
        results <- array(dim = c(nDiscrepancyFuns, nSamples, 2) )

        for(i in 1:nSamples){
            ## put MCMC values from sampled iteration in the model
            values(model, paramNodes) <<- MCMCOutput[i, ] ## Question: Should we use MCMCOutput[i, paramIndices]?

            ## calculate 
            model$calculate(paramDependencies)

            ## Possibly later we will allow obsDiscrepancy to be optional
            for(j in 1:nDiscrepancyFuns) {
                obsDiscrepancy <- discrepancyFunctionList[[j]]$run()
                results[j, i, 1] <- obsDiscrepancy
            }

            ## simulate from posterior predictive
            model$simulate(simNodes, includeData = TRUE)
            model$calculate(dataDependencies)
            
            for(j in 1:nDiscrepancyFuns) {
                simDiscrepancy <- discrepancyFunctionList[[j]]$run()
                results[j, i, 2] <- simDiscrepancy
            }

            ## Careful here with latent states!
            ## Return the model to original state (i.e. state upon entry to this function)
            values(model, dataNodes) <<- origDataValues
            model$calculate(dataDependencies)
        }
    values(model, paramNodes) <<- origParamValues    
    model$calculate(paramDependencies)
    
    returnType(double(3))
    return(results)
    }
)


## This function calculate a PPP using discrepancy functions
## - get observedDiscrepancy from MCMC samples
## - for each MCMC sample
        ## 1) put values from iteration i in the model        
        ## 2) simulate data from the model posterior predictive
        ## 3) calculate discrepancy using simulated data

## Seems like this should be a nimbleFunction
calculatePPP <- function(MCMCSamples,
                        calcDiscrepanciesFun, 
                        returnDiscrepancies = TRUE)
{
   
	## These need to have been set up!
    discrepancies <- calcDiscrepanciesFun$run(MCMCSamples) 

    numDiscrepancies <- dim(discrepancies)[1]
    PPP <- numeric(numDiscrepancies)
    for(i in 1:numDiscrepancies)
        PPP[i] <- mean(discrepancies[i, , 2] >= discrepancies[i, , 1])

    if(returnDiscrepancies)
        list(PPP = PPP, discrepancies = discrepancies)
    else
        list(PPP = PPP, discrepancies = NULL ) ## Or should we return PPP itself?
}


############################################################################
## Calibration replicates
############################################################################
## This function run the MCMC calibration to obtain the CPPP
## - performs checks 
## - calculate observedPPP using original MCMC samples and model
## - for each calibration replicate
        ## 1) put values from iteration r in the model        
        ## 2) simulate data from the model posterior predictive
        ## 3) run mcmc to obtain replicated MCMC samples        
        ## 4) calculate replicatedPPP

## nimbleFunction to set some nodes and then simulate
## To be used for creating a new calibration replicate 
## (not for the iterations of ppp for that replicate)

## nodes are the nodes you want to copy values into
## simNodes nodes that you want to simulate

setAndSimNodes <- nimbleFunction(
    setup = function(model, nodes, simNodes){
        nodes <- model$expandNodeNames(nodes)
        simNodes <- model$expandNodeNames(simNodes)
        simNodes <- model$topologicallySortNodes(simNodes)
    },
    run = function(x = double(1)){
        values(model, nodes) <<- x
        model$simulate(simNodes, includeData = TRUE)
    })

runCalibration <- function(model,                   ## nimbleModel compiled
                           dataNames,               ## names of data nodes to simulate into
                           paramNames,              ## names of data nodes to simulate into
                           origMCMCSamples,         ## originalMCMC samples
                           mcmcConfFun = NULL,
                           discrepancyFunctions = NULL,
                           discrepancyFunctionsArgs = list(),
                           nCalibrationReplicates,
                           MCMCcontrol = list(niter = 500,
                                          thin = 1,
                                          nburnin = 0),                  
                           returnSamples = TRUE,                            
                           returnDiscrepancies = TRUE,
                           calcDisc = TRUE, 
                           parallel = FALSE, 
                           nCores = 1) {



    ## if dataNames is null use nodes in the model with data flag 
    if(is.null(dataNames)) {
        print("Defaulting `dataNames` to model nodes flagged as data.")
        dataNames <- model$getNodeNames(dataOnly = TRUE)
    } 

    ## chech that dataNames are in the model and are stochastics
    testDataNames <- all(model$expandNodeNames(dataNames) %in% model$getNodeNames(stochOnly=TRUE))
    if(testDataNames == FALSE){
        stop(paste("dataNames", dataNames,
            "is not the name a stochastic node in the model."))
    }


    if(is.null(origMCMCSamples)){
        stop(paste("provide origMCMCSamples."))

    }

    ## if paramNames is null use columns of originMCMCSamples (must be provided) 
    if(is.null(paramNames)) {
        print("Defaulting `paramNames` to `origMCMCMSamples' column names")
        paramNames <- colnames(origMCMCSamples)
    } 
    ## chech that paramNames are in the model and are stochastics
    testParamNames <- all(model$expandNodeNames(paramNames) %in%
        model$getNodeNames())
    if(testParamNames == FALSE){
        stop(paste("paramNames are not node names in", model, "."))
    }

    mcmcConfFun <- if(is.null(mcmcConfFun))   function(model) configureMCMC(model, monitors = paramNames, print = FALSE) else mcmcConfFun 

    ## number of calibration replicates (to estimate ppp null distribution)
    if(is.null(nCalibrationReplicates)){
        nCalibrationReplicates <- 100
        print(paste("Defaulting to ", nCalibrationReplicates, " simulated PPP values."))
    }

    ## Default MCMC settings for ppp calculation
    niter   <- if(is.null(MCMCcontrol$niter))   200     else MCMCcontrol$niter
    thin    <- if(is.null(MCMCcontrol$thin))    1       else MCMCcontrol$thin
    nburnin <- if(is.null(MCMCcontrol$nburnin)) 0       else MCMCcontrol$nburnin

    
    # if(!inherits(model, "RmodelBaseClass")){
    #     stop("model is not an Rmodel")
    # }

    # SP: 
    if(is(model$CobjectInterface, "uninitializedField")){ ## The user provided n uncompiled a model that has not been compiled.   (We might need to check first if the user provided a compiled model!)
        compileNimble(model)
    }


    ## check for parallel computation

    if(parallel){
        libError <-  try(library('parallel'), silent = TRUE)
        if(inherits(libError, 'try-error') && nCores > 1){
            warning("The 'parallel' package must be installed to use multiple cores for CPPP calculation. Defaulting to one core.")
            parallel <- FALSE
            nCores <- 1
        }   
    }


    ##------------------------------------------------------------##
    ## Get discrepancies/PPP from original MCMC samples
    ##------------------------------------------------------------##
	simNodes <- unique(c(model$expandNodeNames(dataNames), 
    	model$getDependencies(paramNames, includeData = FALSE, self=FALSE)))
 
    ## calculate discrepancy 
    modelCalcDisc <- calcDiscrepancies(model                    = model,
                                        dataNames               = dataNames,
                                        paramNames              = paramNames,
                                        simNodes 				= simNodes,
                                        discrepancyFunctions    = discrepancyFunctions,
                                        discrepancyFunctionsArgs = discrepancyFunctionsArgs)

    cModelCalcDisc <- compileNimble(modelCalcDisc, project = model)


    originalOutput <- calculatePPP(MCMCSamples             = origMCMCSamples,
                                    calcDiscrepanciesFun    = cModelCalcDisc, 
                                    returnDiscrepancies     = returnDiscrepancies)

    observedPPP <- originalOutput$PPP
    if(returnDiscrepancies) observedDiscrepancies <- originalOutput$discrepancies

    ##------------------------------------------------------------##
    ## Set up calibration replicates
    ##------------------------------------------------------------##
    ## create conf and mcmc to use from the model provided
    mcmcConf <- mcmcConfFun(model)
    ## SP: Should we make sure that paramNames are monitored??
    mcmcUncompiled <- buildMCMC(mcmcConf)

    ##
    cMcmc <- compileNimble(mcmcUncompiled, project = model, resetFunctions = TRUE)


    ##-------------------##
    ## SP: this is reapeated - same nodes are needed to simulate from the pp to compute discrepancies
    simNodes <- unique(c(model$expandNodeNames(dataNames), model$getDependencies(paramNames, includeData = FALSE, self=FALSE)))
	##-------------------##
    
    setAndSimPP <- setAndSimNodes(model = model, 
                                  nodes = paramNames, 
                                  simNodes = simNodes)
    
    cSetAndSimPP <- compileNimble(setAndSimPP, project = model)

    ## if nCalibrationReplicates is less than nrow(origMCMCSamples), 
    ## we want to evenly space or randomly choose rows
    rowsToUse <- floor( seq(1, nrow(origMCMCSamples), length=nCalibrationReplicates) )
    resultsList <- vector(mode = 'list', length = nCalibrationReplicates)

    ## use the same objects
    for(r in 1:nCalibrationReplicates){

        thisResult <- list(samples = NULL, PPP = NULL)
        ok <- TRUE

        ## 1) put values in model and simulate  data from the model posterior predictive
        check <- try(cSetAndSimPP$run(origMCMCSamples[rowsToUse[r], ]))  
        if(inherits(check, 'try-error')) {
            warning(paste0("problem setting samples from row ", rowsToUse[r]))
            ok <- FALSE
        }

        if(ok) {
            ## 3) run mcmc to obtain replicated MCMC samples
            replicatedMCMCsamples <- try(runMCMC(cMcmc, niter = niter, nburnin = nburnin, thin = thin))
            # use either mcmc$run(niter) or runMCMC(mcmc, niter)
            if(inherits(replicatedMCMCsamples, 'try-error')) {
                warning("put nice warning here")
                ok <- FALSE
            }
        }
        
        if(ok) {
            if(returnSamples) {
                thisResult$samples <- replicatedMCMCsamples ## At this step, we could replace the scheme with MCMCReplicate object (or MCMCresult from compareMCMCs.  We want something like this.  Even just a list of lists.
            }
            ## 4) calculate PPP
            if(calcDisc) {
                thisResult$PPP <- try(calculatePPP(MCMCSamples              = replicatedMCMCsamples,
                                                   calcDiscrepanciesFun     = cModelCalcDisc, 
                                                   returnDiscrepancies      = returnDiscrepancies)) ## contains discrepancies and PPP
            }
        }

        resultsList[[r]] <- thisResult
    }

	out <- list()
    
    out$obsPPP <- observedPPP 
    out$repPPP <- sapply(resultsList, function(x) x$PPP$PPP)

    if(returnDiscrepancies){
       
        out$obsDisc <- observedDiscrepancies
        out$repDisc <- sapply(resultsList, function(x) x$PPP$discrepancies, simplify  = FALSE)
    }

    if(returnSamples){
        out$replicatedMCMCSamples   <- sapply(resultsList, function(x) x$samples, simplify  = FALSE)
    }


    return(out)
}
