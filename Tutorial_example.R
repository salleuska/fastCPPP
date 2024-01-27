res <- readRDS("sec6_examples/newcomb/results_nCRep_50_nIter_100.rds")
## The discrepancy used in the paper has index 2
indexStat <- 2
## CPPP
cppp <- mean(res$repPPP[indexStat, ] <= res$obsPPP[indexStat])

## number of calibration rep
r <- dim(res$repPPP)[2]
## number of MCMC samples per calibration rep
m <- 100
#######
library(mcmcse)

obsPPP <- res$obsPPP[2]

## dealtaObs is a vector of differences of the observed discrepancy
## computed for samples from the posterior predictive
## and the discrepancy computed on the original data
deltaObs <- res$obsDisc[indexStat,,2] - res$obsDisc[indexStat,,1]
pppVec <- res$repPPP[2,]

## Here we use each ppp_r (calibration replicate)
## to individuate a quantil on the original 
## chain deltaObs

deltaQ <- quantile(deltaObs, prob = pppVec)

tauTransfer <- numeric(length(deltaQ))

for(i in 1:length(deltaQ)) {
	indSeqTransfer <- as.numeric(I(deltaObs <= deltaQ[i]))
	if(length(unique(indSeqTransfer)) == 1) {
		## If there is no variance, tau is enormous
		tauTransfer[i] <- 1e+10
	} else {
		## 
		varianceMCMC 	<- try(mcse(indSeqTransfer, r = 3)$se^2, silent = TRUE)

		varianceInd 	<- var(indSeqTransfer)
		tauTransfer[i] 	<- (varianceMCMC * length(deltaObs))/varianceInd	
	}
}

## 1. first term E_Y[V[K|Y]]
## compute mean and variance of Ktilde
meanVec <- m*pppVec
varVec <- m*pppVec*(1 - pppVec)*tauTransfer

fVec <- pnorm(m*obsPPP + 0.5, mean = meanVec, sd = sqrt(varVec), lower.tail = TRUE)

term1 <- mean(fVec*(1-fVec))

## 2. second term V_Y[E[K|Y]]
cpppHat <- mean(pppVec <= obsPPP)
term2 <- cpppHat*(1 - cpppHat)	

variancePluginEst <- (term1 + term2)/r

variancePluginEst
