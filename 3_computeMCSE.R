## This script compute the cppp variance using the montecarlo samples
## to provide a baseline estimate

args <- R.utils::commandArgs(asValue=TRUE)
###############################
# args <- list()
# args$indexStat <- 2
# args$dirExample <- "newcomb/montecarlo/"

# Rscript 3_computeMCSE.R --dirExample="dipperCC/montecarlo/" --indexStat=1
# Rscript 3_computeMCSE.R --dirExample="capRecapSimulated/montecarlo/" --indexStat=1
# Rscript 3_computeMCSE.R --dirExample="newcomb/montecarlo/" --indexStat=2
# Rscript 3_computeMCSE.R --dirExample="dipperTT/montecarlo/" --indexStat=1
###############################
## cppp variance via monte carlo

dirExample <- args$dirExample
dir 	    <- dirname(dirExample)
indexStat  <- as.numeric(args$indexStat)


fileList <- list.files(dirExample)
nMC <- length(list.files(dirExample))

# combination of replicates and MCMC iters
## total computational cost
compCost <- c(5000, 10000, 20000, 50000)
M <- c(50, 100, 200, 500)


estimate <- array(0, dim = c(nMC, length(compCost), length(M)))

cat("Computing monte carlo estimate of the cppp variance \n")


cpppEst <- numeric(nMC)
for(i in 1:nMC){
	if(i %% 10 == 0) cat("monte carlo sample - ", i, "\n")

	res <- readRDS(paste0(dirExample, fileList[i]))

	cpppEst[i] <- (sum(res$repPPP[indexStat, ] <= res$obsPPP[indexStat]) + 0.5)/1001
	for(r in 1:length(compCost)){
		R <- compCost[r]/M
		for(m in 1:length(M)){
			discList <- res$repDisc[sample(1:1000, R[m], replace = FALSE)]
			## use continuity correction to avoid 0 in ppp estimate
			pppVec <- unlist(lapply(discList, function(x) 
				(sum(x[indexStat,1:M[m],2] >= x[indexStat,1:M[m],1]) + 0.5)/(M[m] + 1)))
			
			estimate[i,r,m] <- mean(pppVec < res$obsPPP[indexStat])

		}
	}	
}

res <- list()
res$MCvariance <- apply(estimate, c(2, 3), var)
dimnames(res$MCvariance) <- list(compCost, M)

res$MCestimate <- apply(estimate, c(2, 3), mean)
dimnames(res$MCestimate) <- list(compCost, M)

res$cpppEstAll <- mean(cpppEst)
res$cpppEstVarAll <- var(cpppEst)


cat("Saving monte carlo estimate of the cppp variance \n")

saveRDS(res, file = paste0(dir, "/varianceMC.rds"))

