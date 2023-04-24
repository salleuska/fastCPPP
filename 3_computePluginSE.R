## This script compute the cppp variance using the plug-in estimator 
## defined in section 5 of the paper
## It also compute the coverage for the intervals

library(mcmcse) ## mcmcse  

args <- R.utils::commandArgs(asValue=TRUE)
################################################
## Possible arguments for the script
## --filename	 	## path to cppp discrepancy files
## --indexStat		## index for the discrepancy to use (the runCalibration() function allows for multiple discrepancies)

# args <- list()
# args$filename="sec6_examples/newcomb/results_nCRep_1000_nIter_1000.rds" 
# args$indexStat <- 2

#######################

## number of times we want to repeat the computation
N <- 500

## note: to estimate the variance for different combination of mcmc samples and calibration replicates
## we run one large CPPP computation, with large number of MCMC samples and calibration replicates (e.g. 1000)
## Example: for m = 100, r = 50 -  we consider 50 calibration replicates and the first 100 mcmc samples
## to account for the fact that we are sampling the c.r., we average results over N repetitions

## set-up
dirExample <- dirname(args$filename)

nReplicates <- as.numeric(strsplit(args$filename, "_")[[1]][3])
nIter 		<- as.numeric(strsplit(args$filename, "_|\\.")[[1]][5])

indexStat <- as.numeric(args$indexStat)
res <- readRDS(args$filename)

## observed discrepancies
nDisc <- length(res$obsPPP)

## get the most precise cppp estimate
cpppEst <- mean(res$repPPP[indexStat, ] <= res$obsPPP[indexStat])
## combination of replicates and MCMC iters
## defined via the total computational cost
compCost <- c(5000, 10000, 20000, 50000)
M <- c(50, 100, 200, 500)

rTOT <- length(res$repDisc)

###############################
## Variance trasfer method
###############################
## Compute differences between discrepancy values at each iteration
deltaObs <- res$obsDisc[indexStat,,2] - res$obsDisc[indexStat,,1]
## original number of MCMC
mOrig <- length(deltaObs)

## objects for output
estimatesCPPP <- array(0, dim = c(length(compCost), length(M)))
variancePlugin <- array(0, dim = c(length(compCost), length(M)))
averageCoverage <- array(0, dim = c(length(compCost), length(M)))

## monte carlo variance estimates (for check)
mcVarEstimatesCPPP <- array(0, dim = c(length(compCost), length(M)))
mcVarPlugin <- array(0, dim = c(length(compCost), length(M)))

tmpMean <- numeric(N)
tmpVarApprox <- numeric(N)
tmpVariancePlugin <- numeric(N)
tmpAverageESS <- numeric(N)
coverage <- numeric(N)

fMat <- array(0, dim = c(N, 100))

## ESS transfer estimate: tauTransfer/m
averageESSTransfer <- array(0, dim = c(length(compCost), length(M)))

obsPPP <- res$obsPPP[indexStat]

cat("Computing plug-in variance estimate \n")

## for test
# c <- m <- j <- 1

for(c in 1:length(compCost)){
	R <- compCost[c]/M
	for(m in 1:length(M)){		
	cat(paste0("comp cost = ", compCost[c], " MCMCsamples = ", M[m], " Cal. Rep = ", R[m], " \n"))
		for(j in 1:N){ 
			discList <- res$repDisc[sample(1:rTOT, R[m], replace = F)]
			## use "continuity correction" to get the ppp
			pppVec <- unlist(lapply(discList, function(x) 
				(sum(x[indexStat,1:M[m],2] >= x[indexStat,1:M[m],1]) + 0.5)/(M[m] + 1)))
			
 			tmpMean[j] <- mean(pppVec <= obsPPP)

			tauTransfer <- numeric(R[m])
			essEstRep 	<- numeric(R[m])
			deltaQ 		<- quantile(deltaObs, prob = pppVec)

			for(i in 1:R[m]){
	            indSeqTransfer 		<- as.numeric(I(deltaObs <= deltaQ[i]))
				if(length(unique(indSeqTransfer)) == 1) {
					## If there is no variance, tau is enormous
					tauTransfer[i] <- 1e+10
				} else {
					## 
					varianceMCMC 	<- try(mcse(indSeqTransfer, r = 3)$se^2, silent = TRUE)
#					varianceMCMC 	<- try(mcse(indSeqTransfer, r = 2)$se^2, silent = TRUE)
					
					varianceInd 	<- var(indSeqTransfer)
					tauTransfer[i] 	<- (varianceMCMC * length(deltaObs))/varianceInd
				}
			}

			## Variance approximation

			## 1. first term E_Y[V[K|Y]]
			## compute mean and variance of Ktilde
			meanVec <- M[m]*pppVec
			varVec <- M[m]*pppVec*(1 - pppVec)*tauTransfer

			fVec <- pnorm(M[m]*obsPPP + 0.5, mean = meanVec, sd = sqrt(varVec), lower.tail = TRUE)
			## indentical approximation
			## fVec <- pnorm(obsPPP + 0.5/M[m], mean = meanVec/M[m], sd = sqrt(varVec/M[m]^2), lower.tail = TRUE)
			term1 <- mean(fVec*(1-fVec))
	
			## 2. second term V_Y[E[K|Y]]
			cpppHat <- mean(pppVec <= obsPPP)
			term2 <- cpppHat*(1 - cpppHat)	

			tmpVariancePlugin[j] <- (term1 + term2)/R[m]
			

			## ESS (transfer method)
			tmpAverageESS[j] <- mean(tauTransfer/M[m])
			## Coverage

			CILow <- cpppHat -1.96*sqrt(tmpVariancePlugin[j])
			CIUp <- cpppHat +1.96*sqrt(tmpVariancePlugin[j])
			# CILow <- cpppHat -1*sqrt(tmpVariancePlugin[j])
			# CIUp <- cpppHat +1*sqrt(tmpVariancePlugin[j])

			coverage[j] <- cpppEst <= CIUp & cpppEst >= CILow

			if(j %% 10 == 0) cat("sample number - ", j, "\n")
		}
		
		estimatesCPPP[c,m] <- mean(tmpMean)
		mcVarEstimatesCPPP[c,m] <- var(tmpMean)
		averageCoverage[c, m] <- mean(coverage)
		variancePlugin[c,m] 	<- mean(tmpVariancePlugin)
		mcVarPlugin[c,m] 	<- var(tmpVariancePlugin)

		averageESSTransfer[c,m] <- mean(tmpAverageESS)
	}
}

cat("End computing plug-in variance estimate \n")

########
dimnames(estimatesCPPP) <- list(compCost, M)
dimnames(variancePlugin) <- list(compCost, M)
dimnames(mcVarPlugin) <- list(compCost, M)
dimnames(mcVarEstimatesCPPP) <- list(compCost, M)
dimnames(averageESSTransfer) <- list(compCost, M)
dimnames(averageCoverage) <- list(compCost, M)

resVariance <- list(cpppEst = cpppEst, 
					estimatesCPPP = estimatesCPPP, 
					mcVarEstimatesCPPP = mcVarEstimatesCPPP,	
					variancePlugin = variancePlugin,
					mcVarPlugin= mcVarPlugin, 
					averageESSTransfer = averageESSTransfer,
					averageCoverage = averageCoverage
)

cat("Saving plug-in variance estimate \n")

saveRDS(resVariance, file = paste0(dirExample, "/variancePlugin.rds" ))














