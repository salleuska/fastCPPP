#-----------------------------------------#
## Computational methods for fast Bayesian hierarchical model assessment via calibrated posterior p-values.
## Sally Paganin
## last update: June 2023
## R version 4.3.0 (2023-04-21) -- "Already Tomorrow"
## nimble version 0.13.2
##-----------------------------------------#
library(ggplot2)
library(latex2exp)
library(cowplot)

args <- R.utils::commandArgs(asValue=TRUE)
#######################
## Script arguments
#######################
## --dirExample   directory
## --plotTitle	  title for the plot
#######################
# Rscript 4_plotResults.R --dirExample="newcomb" --plotTitle="Newcomb example"
# Rscript 4_plotResults.R --dirExample="newcombBadMixing" --plotTitle="Newcomb example - bad mixing"
#######################

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

##############################
## files with plugin and bootstrap estimates
##############################
pluginRes <- readRDS(paste0(dirExample, "/variancePlugin.rds"))
boot <- readRDS(paste0(dirExample, "/varianceBootstrap.rds"))

## Average ESS
dimnames(pluginRes$averageESSTransfer) <- dimnames(pluginRes$estimate)
averageESS <- round(reshape2::melt(1/pluginRes$averageESSTransfer))

## Cppp point estimate and average ESS
pluginEst <- pluginRes$estimate

## cppp variance estimate - plug in
plugin <- pluginRes$variancePlugin
pluginVar <- pluginRes$mcVarPlugin  ## simulation variance

## cppp variance estimate - MBB bootstrap (np)
bootMBB <- boot$varianceMBB
bootMBBVar <- boot$mcVarMBB      ## simulation variance

## cppp variance estimate - parametric bootstrap
bootNormal <- boot$varianceNormal
bootNormalVar <- boot$mcVarNormal   ## simulation variance

####################
### Standard deviation - different estimates
####################
## Create dataframes
dfPlugin 		<- reshape2::melt(sqrt(plugin))
dfMBB 			<- reshape2::melt(sqrt(bootMBB))
dfBootNormal 	<- reshape2::melt(sqrt(bootNormal))

dfPluginVar 		<- reshape2::melt(sqrt(pluginVar))
dfMBBVar 			<- reshape2::melt(sqrt(bootMBBVar))
dfBootNormalVar 	<- reshape2::melt(sqrt(bootNormalVar))

dfCPPPEst 	<- reshape2::melt(pluginRes$estimate)

dfPlugin$se <- dfPluginVar$value
dfMBB$se <- dfMBBVar$value
dfBootNormal$se <- dfBootNormalVar$value

dfPlugin$ESS 		<- averageESS$value
dfMBB$ESS 	        <- averageESS$value
dfBootNormal$ESS 	<- averageESS$value

dfPlugin$varEstimate 		<- "Plug-in"
dfMBB$varEstimate 	        <- "Bootstrap - MBB"
dfBootNormal$varEstimate 	<- "Bootstrap - normal"

df <- rbind(dfPlugin, dfMBB, dfBootNormal)
df <- cbind(df, CPPPest = rep(dfCPPPEst$value, 3))

colnames(df) <- c("ComputationalCost", "m", "value", "se","ESS", "varEstimate", "cpppEst")
df$value[df$value ==Inf] <- NA

df$Replicates <- round(df$ComputationalCost/df$m)

df$varEstimate <- as.factor(df$varEstimate)
df$varEstimate <- factor(df$varEstimate, levels = levels(df$varEstimate)[c(3,1,2)])

df$ComputationalCost <- as.factor(df$ComputationalCost)
levels(df$ComputationalCost ) = paste0("c = ", levels(df$ComputationalCost ))
df$m <- as.factor(df$m)

df$ESS <- as.character(df$ESS)

colors <- c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF")

#########################
## Montecarlo variance
#########################
if(grepl("newcomb", dirExample)){
	maxVal <- 0.10
} else {
	maxVal <- max(df$value[df$value < 1], na.rm= T)
}

dfMC  <- reshape2::melt(sqrt(MCvar$MCvariance))
dfMC$ESS 		<- as.character(averageESS$value)

dfMC$varEstimate 		<- "Monte Carlo"
colnames(dfMC) <- c("ComputationalCost", "m", "value", "ESS", "varEstimate")

dfMC$Replicates <- dfMC$ComputationalCost/dfMC$m
df$varEstimate <- as.factor(df$varEstimate)

dfMC$ComputationalCost <- as.factor(dfMC$ComputationalCost)
levels(dfMC$ComputationalCost ) = paste0("c = ", levels(dfMC$ComputationalCost ))
dfMC$m <- as.factor(dfMC$m)



########################################
## Point estimate with error bars
########################################
pd <- position_dodge(width=0.5)

df$se <- NULL
### Monte carlo mean estimate of cppp 
dfMC$cpppEst <- reshape2::melt(MCvar$MCestimate)$value
dfPlot <- rbind(df, dfMC)

dfPlot$lower <- ifelse(dfPlot$cpppEst - dfPlot$value > 0, dfPlot$cpppEst - dfPlot$value, 0)
dfPlot$upper <- ifelse(dfPlot$cpppEst + dfPlot$value < 1 , dfPlot$cpppEst + dfPlot$value, 1)

maxVal <- max(dfPlot$upper[dfPlot$upper < 1], na.rm= T)


trueCPPP <- MCvar$cpppEstAll
trueCPPPVar <- MCvar$cpppEstVarAll

xLabels <- levels(dfPlot$m)
dfPlot$m <- as.numeric(dfPlot$m)

###
plotList <- list()
for(i in 1:length(levels(dfPlot$ComputationalCost))){
	subsetDfPlot <- subset(dfPlot, dfPlot$ComputationalCost == levels(dfPlot$ComputationalCost)[i])

	mVals <- paste0("\\tilde{m} = ", c(50,100, 200, 500))
	essVals <- paste0("\\widehat{\\widetilde{ESS}} = ", 
		unique(as.character(subsetDfPlot$ESS)))


	xLabels <- TeX(paste0("\\overset{",mVals, "}","{", essVals, "}"))
	
	# xLabels <- TeX(paste0("\\tilde{m} = ", c(50,100, 200, 500), "\n", 
	# 	"\\widehat{\\tilde{ESS}} = ", 
	# 	unique(as.character(subsetDfPlot$ESS))))
	subsetDfPlot$ESS <- 1:length(unique(subsetDfPlot$ESS))

	plotList[[i]] <- ggplot(subsetDfPlot,
	 	aes(x= ESS,  y= cpppEst, color = varEstimate, 
		group = varEstimate)) +
		scale_x_continuous(breaks = 1:4, label = xLabels) +
	  	facet_wrap(. ~ ComputationalCost, nrow = 1) +
		geom_hline(yintercept=trueCPPP, linetype="dashed", color = "gray30") + 
		annotate("rect", xmin = -Inf, xmax = Inf, ymin = trueCPPP - sqrt(trueCPPPVar),
		 ymax = trueCPPP +sqrt(trueCPPPVar), fill = "gray80", alpha = .5) + 
		geom_point(aes(shape = varEstimate), alpha=0.80, position= pd) + 
		scale_color_manual(values =c(colors, "black")) +
		scale_linetype_manual(values=c(rep("solid", 3), "solid"))+      
		geom_errorbar(aes(ymin=lower, ymax=upper, linetype = varEstimate), width=.4, position= pd) +
	  	theme_bw() + theme(legend.title = element_blank()) + 
	  	ylab("cppp") + xlab("") + 
	  	expand_limits(y = maxVal + 0.15) + coord_cartesian(ylim = c(0, maxVal + 0.1)) + 
		geom_text(aes(label = paste0("r=", Replicates), y = maxVal + 0.06),
				  	   color = "black", angle = 90, vjust = 0) + 
	    theme(legend.position="bottom", 
		strip.background = element_rect(fill="white"))          
}

p_no_legend <- lapply(plotList, function(x) x + theme(legend.position = "none"))
legend <- get_legend(plotList[[1]] + theme(legend.position = "bottom"))

title <- ggdraw() + draw_label(plotTitle, fontface = "bold")
p_grid <- plot_grid(plotlist = p_no_legend, nrow = 1)

plotToSave <- plot_grid(title, p_grid, legend, ncol = 1,
rel_heights = c(0.1, 1, 0.2))

ggsave(plotToSave, file = paste0("figures/", basename(dirExample), "_varComparison.png"), 
 	    width = 16, height = 5, dpi = 400, scale = 0.75)


########################################
## Extra - Figures for slides
########################################

# p_grid <- plot_grid(plotlist = p_no_legend[c(2, 3)], nrow = 1)
# plotToSave <- plot_grid(title, p_grid, legend, ncol = 1,
# rel_heights = c(0.1, 1, 0.2))

# ggsave(plotToSave, file = paste0("figureForSlides/", basename(dirExample), "_CPPPVarComparisonESS.pdf"), 
#  	    width = 8, height = 5, dpi = 400, scale = 0.9)


# plotList2 <- list()
# for(i in 1:length(levels(dfPlot$ComputationalCost))){
# 	subsetDfPlot <- subset(dfPlot, dfPlot$ComputationalCost == levels(dfPlot$ComputationalCost)[i])

# 	mVals <- paste0("\\tilde{m} = ", c(50,100, 200, 500))
# 	essVals <- paste0("\\widehat{\\widetilde{ESS}} = ", 
# 		unique(as.character(subsetDfPlot$ESS)))


# 	xLabels <- TeX(paste0("\\overset{",mVals, "}","{", essVals, "}"))
	
# 	# xLabels <- TeX(paste0("\\tilde{m} = ", c(50,100, 200, 500), "\n", 
# 	# 	"\\widehat{\\tilde{ESS}} = ", 
# 	# 	unique(as.character(subsetDfPlot$ESS))))
# 	subsetDfPlot$ESS <- 1:length(unique(subsetDfPlot$ESS))

# 	plotList2[[i]] <- ggplot(subsetDfPlot,
# 	 	aes(x= ESS,  y= cpppEst, color = varEstimate, 
# 		group = varEstimate)) +
# 		scale_x_continuous(breaks = 1:4, label = TeX(mVals)) +
# 	  	facet_wrap(. ~ ComputationalCost, nrow = 1) +
# 		geom_hline(yintercept=trueCPPP, linetype="dashed", color = "gray30") + 
# 		annotate("rect", xmin = -Inf, xmax = Inf, ymin = trueCPPP - sqrt(trueCPPPVar),
# 		 ymax = trueCPPP +sqrt(trueCPPPVar), fill = "gray80", alpha = .5) + 
# 		geom_point(aes(shape = varEstimate), alpha=0, position= pd) + 
# 		scale_color_manual(values =c(colors, "black")) +
# 		scale_linetype_manual(values=c(rep("solid", 3), "solid"))+      
# 		geom_errorbar(aes(ymin=lower, ymax=upper, linetype = varEstimate),
# 		 width=.4, position= pd, alpha = 0) +
# 	  	theme_bw() + theme(legend.title = element_blank()) + 
# 	  	ylab("cppp") + xlab("") + 
# 	  	expand_limits(y = maxVal + 0.15) + coord_cartesian(ylim = c(0, maxVal + 0.1)) + 
# 		geom_text(aes(label = paste0("r=", Replicates), y = maxVal + 0.06),
# 				  	   color = "black", angle = 90, vjust = 0) + 
# 	    theme(legend.position="none", 
# 		strip.background = element_rect(fill="white"))          
# }
# plotList2[[3]]
# library(cowplot)
# p_no_legend <- lapply(plotList2[c(1, 3)], function(x) x + theme(legend.position = "none"))
# legend <- get_legend(plotList2[[1]] + theme(legend.position = "bottom"))

# title <- ggdraw() + draw_label(plotTitle, fontface = "bold")
# p_grid <- plot_grid(plotlist = p_no_legend, nrow = 1)

# plotToSave <- plot_grid(title, p_grid, ncol = 1,
# rel_heights = c(0.1, 1))
# plotToSave

# ggsave(plotToSave, file = paste0("figureForSlides/", basename(dirExample), "_CPPPVarComparisonESS_empty.png"), 
#  	    width = 8, height = 4, dpi = 400, scale = 0.9)


########################################


