library(maotai) ## MBB
library(mcmcse) ## mcmcse  

args <- R.utils::commandArgs(asValue=TRUE)
################################################
## Possible arguments for the script
## --dirExample	 	## directory for input/output
## --indexStat		## index for the discrepancy to use (the runCalibration() function allows for multiple discrepancies)
## --bootIters		## number of bootstrap iterations for the boostrap variance estimators
#######################

## number of times we want to repeat the computation
N <- 100
## note: to estimate the variance for different combination of mcmc samples and calibration replicates
## we run one large CPPP computation, with large number of MCMC samples and calibration replicates (e.g. 1000)
## Example: for m = 100, r = 50 -  we consider 50 calibration replicates and the first 100 mcmc samples
## to account for the fact that we are sampling the c.r., we average results over N repetitions

## set-up
dirExample <- dirname(args$filename)

nReplicates <- as.numeric(strsplit(args$filename, "_")[[1]][3])
nIter 		<- as.numeric(strsplit(args$filename, "_|\\.")[[1]][5])

indexStat <- as.numeric(args$indexStat)
bootIters <- as.numeric(args$bootIters)
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

## monte carlo variance estimates (for check)
mcVarEstimatesCPPP <- array(0, dim = c(length(compCost), length(M)))
mcVarPlugin <- array(0, dim = c(length(compCost), length(M)))

tmpMean <- numeric(N)
tmpVarApprox <- numeric(N)
tmpVarApproxTransfer <- numeric(N)

fMat <- array(0, dim = c(N, 100))

## ESS transfer estimate: tauTransfer/m
averageESSTransfer <- array(0, dim = c(length(compCost), length(M)))

obsPPP <- res$obsPPP[indexStat]

cat("Computing plug-in variance estimate \n")

# c <- m <- j <- 1
for(c in 1:length(compCost)){
	R <- compCost[c]/M
	for(m in 1:length(M)){		
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

			## variance approximation

			## first term E_Y[V[K|Y]]
			## compute mean and variance of Ktilde
			meanVec <- M[m]*pppVec
			varVec <- M[m]*pppVec*(1 - pppVec)*tauTransfer

			fVec <- pnorm(M[m]*obsPPP + 0.5, mean = meanVec, sd = sqrt(varVec), lower.tail = TRUE)
			## indentical approximation
			## fVec <- pnorm(obsPPP + 0.5/M[m], mean = meanVec/M[m], sd = sqrt(varVec/M[m]^2), lower.tail = TRUE)
			term1 <- mean(fVec*(1-fVec))
	
			## second term V_Y[E[K|Y]]
			cpppHat <- mean(pppVec <= obsPPP)
			term2 <- cpppHat*(1 - cpppHat)	

			tmpVarApproxTransfer[j] <- (term1 + term2)/R[m]
			
			if(j %% 10 == 0) cat("sample number - ", j, "\n")

		}
		
		estimatesCPPP[c,m] <- mean(tmpMean)

		mcVarEstimatesCPPP[c,m] <- var(tmpMean)

		variancePlugin[c,m] 	<- mean(tmpVarApproxTransfer)
		mcVarPlugin[c,m] 	<- var(tmpVarApproxTransfer)

		averageESSTransfer[c,m] <- mean(tauTransfer/M[m])
	}
}

cat("End computing plug-in variance estimate \n")

########
dimnames(estimatesCPPP) <- list(compCost, M)
dimnames(variancePlugin) <- list(compCost, M)
dimnames(mcVarPlugin) <- list(compCost, M)
dimnames(mcVarEstimatesCPPP) <- list(compCost, M)
dimnames(averageESSTransfer) <- list(compCost, M)

resVariance <- list(cpppEst = cpppEst, 
					estimatesCPPP = estimatesCPPP, 
					mcVarEstimatesCPPP = mcVarEstimatesCPPP,	
					variancePlugin = variancePlugin,
					mcVarPlugin= mcVarPlugin, 
					averageESSTransfer = averageESSTransfer
)

cat("Saving plug-in variance estimate \n")

saveRDS(resVariance, file = paste0(dirExample, "/variancePlugin.rds" ))














