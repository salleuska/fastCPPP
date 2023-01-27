library(xtable)
args <- R.utils::commandArgs(asValue=TRUE)
#######################
## set up args for the  script 
## --dirExample
## --plotTitle

# args <- list()
# args$dirExample <- "newcomb"
# args$plotTitle <- "Newcomb example"

# args <- list()
# args$dirExample <- "dipperTT"
# args$plotTitle <- "Dipper example - T/T model"

dirExample <- args$dirExample
plotTitle <- args$plotTitle

##############################
## Monte carlo variance - baseline
##############################
if(grepl("BadMixing", dirExample)) {
	MCvar <- readRDS("newcomb/varianceMC.rds")
} else {
	MCvar <- readRDS(paste0(dirExample, "/varianceMC.rds"))
}

## "True CPPP from MonteCarlo procedure
trueCPPP <- MCvar$cpppEstAll

pluginRes <- readRDS(paste0(dirExample, "/variancePlugin.rds"))
boot <- readRDS(paste0(dirExample, "/varianceBootstrap.rds"))

pluginRes$averageCoverage
boot$averageCoverageNormal
boot$averageCoverageMBB

xtable(rbind(pluginRes$averageCoverage[1, ],
boot$averageCoverageNormal[1, ],
boot$averageCoverageMBB[1, ]))

xtable(rbind(c(t(pluginRes$averageCoverage)),  
	c(t(boot$averageCoverageNormal)), 
c(t(boot$averageCoverageMBB))))