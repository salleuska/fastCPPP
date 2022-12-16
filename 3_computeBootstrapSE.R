## This script compute the cppp variance using the bootstrapt estimators 
## defined in section 5 of the paper
## It also compute the coverage for the intervals

library(maotai) ## MBB
library(mcmcse) ## mcmcse  

args <- R.utils::commandArgs(asValue=TRUE)
################################################
## Possible arguments for the script
## --dirExample	 	## directory for input/output
## --indexStat		## index for the discrepancy to use (the runCalibration() function allows for multiple discrepancies)
## --bootIters      ## numer of bootstrap replications
#######################
# args <- list()
# args$filename="newcomb/results_nCRep_1000_nIter_1000.rds" 
# args$indexStat=2 
# args$bootIters=100

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

bootIters <- as.numeric(args$bootIters)
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
## variance estimate via bootstrap
###############################
## Compute differences between discrepancy values at each iteration
deltaObs <- res$obsDisc[indexStat,,2] - res$obsDisc[indexStat,,1]
## original number of MCMC
mOrig <- length(deltaObs)

varianceMBB <- array(0, dim = c(length(compCost), length(M)))
varianceNormal <- array(0, dim = c(length(compCost), length(M)))

averageCoverageMBB <- array(0, dim = c(length(compCost), length(M)))
averageCoverageNormal <- array(0, dim = c(length(compCost), length(M)))

mcVarMBB <- array(0, dim = c(length(compCost), length(M)))
mcVarNormal <- array(0, dim = c(length(compCost), length(M)))

#N <- 100

tmpVarMBB <- numeric(N)
tmpNormal <- numeric(N)

coverageMBB <- numeric(N)
coverageNormal <- numeric(N)


cpppBootNormal <- numeric(bootIters)
cpppBootMBB    <- numeric(bootIters)

obsPPP <- res$obsPPP[indexStat]

cat("Start boostrap variance estimates \n")

# r <- m <- k <- 1
for(r in 1:length(compCost)){	
	R <- compCost[r]/M
	for(m in 1:length(M)){
	cat(paste0("comp cost = ", compCost[r], " MCMCsamples = ", M[m], " Cal. Rep = ", R[m], " \n"))
		for(k in 1:N){

			discList <- res$repDisc[sample(1:rTOT, R[m], replace = F)]

			for(j in 1:bootIters){

				indexBoot <- sample(1:R[m], R[m], replace = T)

				bootDiscList <- discList[indexBoot]
				## use "continuity correction"
				pppVec <- unlist(lapply(bootDiscList, function(x) 
					(sum(x[indexStat,1:M[m],2] >= x[indexStat,1:M[m],1]) + 0.5)/(M[m] + 1)))


				## 1) normal approximation - sample kVec ~ N(pppVec, sqrt(varApprox))
				tauTransfer <- numeric(R[m])
				deltaQ 		<- quantile(deltaObs, prob = pppVec)

				for(i in 1:R[m]){
		            indSeqTransfer 		<- as.numeric(I(deltaObs <= deltaQ[i]))
					if(length(unique(indSeqTransfer)) == 1) {
						## If there is no variance, tau is enormous
						tauTransfer[i] <- 1e+10
					} else {
						varianceMCMC 	<- try(mcse(indSeqTransfer, r = 3)$se^2, silent = TRUE)
						
						varianceInd 	<- var(indSeqTransfer)
						tauTransfer[i] 	<- (varianceMCMC * length(deltaObs))/varianceInd
					}
				}

				# ## normal approximation for K
				meanVec <- M[m]*pppVec
				varVec <- M[m]*pppVec*(1 - pppVec)*tauTransfer

				kappaBoot <- rnorm(R[m], mean = M[m]*pppVec, sd = sqrt(varVec))
				cpppBootNormal[j] <- mean(kappaBoot <= M[m]* obsPPP)
				
				## 2) moving block bootstrap

				mbbIndex <- boot.mblock(M[m], b = max(2, round(M[m]/10)))
				mbbPppVec <- unlist(lapply(bootDiscList, function(x) 
					(sum(x[indexStat,mbbIndex,2] >= x[indexStat,mbbIndex,1]) + 0.5)/(M[m] + 1)))

				cpppBootMBB[j] <- mean(mbbPppVec <= obsPPP)

				if(j %% 10 == 0) cat("boot iter - ", j, "\n")
			}

			tmpVarMBB[k] <- var(cpppBootMBB)
			tmpNormal[k] <- var(cpppBootNormal)


			CILowMBB <- quantile(cpppBootMBB, 0.025)
			CIUpBB <- quantile(cpppBootMBB, 0.975)

			coverageMBB[k] <- cpppEst <= CIUpBB & cpppEst >= CILowMBB

			CILowNormal <- quantile(cpppBootNormal, 0.025)
			CIUpNormal <- quantile(cpppBootNormal, 0.975)

			coverageNormal[k] <- cpppEst <= CIUpNormal & cpppEst >= CILowNormal


		}

		varianceMBB[r,m] <- mean(tmpVarMBB)
		varianceNormal[r,m] <- mean(tmpNormal)

		mcVarMBB[r,m] <- var(tmpVarMBB)
		mcVarNormal[r,m] <- var(tmpNormal)

		averageCoverageNormal[r, m] <- mean(coverageNormal)
		averageCoverageMBB[r, m] <- mean(coverageMBB)

	}
}

cat("End estimation boostrap variance \n")

dimnames(varianceMBB) <- list(compCost, M)
dimnames(varianceNormal) <- list(compCost, M)
dimnames(mcVarMBB) <- list(compCost, M)
dimnames(mcVarNormal) <- list(compCost, M)


resBootVariance <- list(cpppEst = cpppEst, 
						varianceMBB =varianceMBB, 
						mcVarMBB = mcVarMBB,
						varianceNormal = varianceNormal, 
						mcVarNormal = mcVarNormal, 
						averageCoverageMBB = averageCoverageMBB, 
						averageCoverageNormal = averageCoverageNormal)

cat("Saving boostrap variance estimates \n")

saveRDS(resBootVariance, file = paste0(dirExample, "/varianceBootstrap.rds" ))



