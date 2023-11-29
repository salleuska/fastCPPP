#-----------------------------------------#
## Computational methods for fast Bayesian hierarchical model assessment via calibrated posterior p-values.
## Sally Paganin
## last update: June 2023
## R version 4.3.0 (2023-04-21) -- "Already Tomorrow"
## nimble version 0.13.2
##-----------------------------------------#
library(xtable)
args <- R.utils::commandArgs(asValue=TRUE)
#######################
## Script arguments
#######################
## --dirExample	 	## directory for input/output
##############################
## Monte carlo variance - baseline
##############################
dirExample <- args$dirExample

if(grepl("BadMixing", dirExample)) {
	MCvar <- readRDS("sec6_examples/newcomb/varianceMC.rds")
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

# xtable(rbind(pluginRes$averageCoverage[1, ],
# boot$averageCoverageNormal[1, ],
# boot$averageCoverageMBB[1, ]))

matrix <- rbind(c(t(pluginRes$averageCoverage)),  
	c(t(boot$averageCoverageMBB)),
c(t(boot$averageCoverageNormal)))

rownames(matrix) <- c("Plug-in" , "Bootstrap - MBB", "Bootstrap - Normal")

xtable(matrix, digits = 3)
###########
## Extra - plot for interpretation

dd <- matrix - 0.95
library(pheatmap)

# pheatmap(dd, display_numbers = T, cluster_rows = F, cluster_cols = F, color = rev(RColorBrewer::brewer.pal(n = 5, name ="YlOrRd")), 
# filename = "newcombCoverage.pdf", width = 10, height = 4)

# pheatmap(dd, display_numbers = T, cluster_rows = F, cluster_cols = F, color = rev(RColorBrewer::brewer.pal(n = 5, name ="YlOrRd")), 
# filename = "newcombBadCoverage.pdf", width = 10, height = 4)


# pheatmap(dd, display_numbers = T, cluster_rows = F, cluster_cols = F, color = rev(RColorBrewer::brewer.pal(n = 5, name ="YlOrRd")), 
# filename = "dipperCC.pdf", width = 10, height = 4)

# pheatmap(dd, display_numbers = T, cluster_rows = F, cluster_cols = F, color = rev(RColorBrewer::brewer.pal(n = 5, name ="YlOrRd")), 
# filename = "dipperTT.pdf", width = 10, height = 4)


# pheatmap(dd, display_numbers = T, cluster_rows = F, cluster_cols = F, color = rev(RColorBrewer::brewer.pal(n = 5, name ="YlOrRd")), 
# filename = "capRecap.pdf", width = 10, height = 4)

