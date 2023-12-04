library(extraDistr)
library(latex2exp)
library(ggplot2)
options(scipen = 999) 
#########################################################
aVec <- c(0.5, 2, 4, 2)
bVec <- c(0.5, 2, 2, 4)
#########################################################
## Set true cppp
cppp0 <- c(0.01, 0.05, 0.2, 0.5)

## number of MCMCM samples 
M <- c(50, 100, 200, 500)
## number of calibration replicates
nCal <- c(100, 500, 1000, 2000)

variance <- array(0, dim = c(length(aVec), length(nCal), length(M), length(cppp0)))
bias <- array(0, dim = c(length(aVec), length(nCal), length(M), length(cppp0)))
for(betaScenario in 1:length(aVec)){
	
	## Set the scenario for the null ppp distribution 
	## Uniform - round - skewed left - skewed right - U shape
	a <- aVec[betaScenario]
	b <- bVec[betaScenario]

	## compute the corresponsing pppObs
	pppObs <- qbeta(cppp0, a, b, lower.tail = TRUE)

	for(j in 1:length(nCal)){
		# for(m in 1:length(M)){
		for(m in 1:length(M)){
			for(i in 1:length(pppObs)){
				## Bias 
				bias[betaScenario, j, m, i] <- abs(pbbinom(M[m]*pppObs[i], M[m], a, b) - cppp0[i])
				## variance
				variance[betaScenario, j, m ,i] <-  (pbbinom(M[m]*pppObs[i], M[m], a, b)*(1 - pbbinom(M[m]* pppObs[i], M[m], a, b)))/nCal[j] 
			}

		}	
	}

}

dimnames(bias) <- list(1:length(aVec), nCal, M, cppp0)
dimnames(variance) <- list(1:length(aVec), nCal, M, cppp0)

dimnames(bias)[[1]] <- sapply(1:length(aVec), function(x) paste0("Beta(", aVec[x], ",", bVec[x], ")"))
dimnames(variance)[[1]] <- sapply(1:length(aVec), function(x) paste0("Beta(", aVec[x], ",", bVec[x], ")"))

## standard error and sd 
sd <- sqrt(variance)
rmse <- sqrt(sd^2 + bias^2)

## parameter of the plot
xAxis <- TeX("\\tilde{m}")
#facetLabel <- Tex("c =  r\\tilde{m} = ")

########################################################################
## Plots

plotListSD <- list()
plotListBIAS <- list()
plotListRMSE <- list()

plotListSD2 <- list()
plotListBIAS2 <- list()
plotListRMSE2 <- list()

for(betaScenario in 1:length(aVec)){
	a <- aVec[betaScenario]
	b <- bVec[betaScenario]

	dfSd <- reshape2::melt(sd[betaScenario,,,])
	colnames(dfSd) <- c("nCal", "M", "cppp", "value")
	
	dfSd$nCal <- as.factor(dfSd$nCal)
	dfSd$M <- as.factor(dfSd$M)
	dfSd$cppp <- as.factor(dfSd$cppp)
	levels(dfSd$nCal ) = paste0("r = ", levels(dfSd$nCal ))
	

	gg <- ggplot(dfSd, aes(x= M,  y= value, group = cppp, 
		 color=cppp)) +
		  xlab(xAxis) +
		  geom_point(aes(shape = cppp), alpha=0.80) + geom_line(alpha=0.80) + 
	      scale_color_brewer(palette = "Dark2") +
		  scale_y_continuous(breaks = seq(0, 0.06, by = 0.01)) + 
		  facet_wrap(. ~ nCal, nrow = 1) +
	  	  theme_bw() + 
	  	  expand_limits(y = 0.06) +
	  	  # ylab(TeX("\\sqrt{variance}")) + 
	  	  ylab("standard error") + 
	  	 ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
	      theme(axis.text.x = element_text(angle = 90, hjust = 1),
		  strip.background = element_rect(fill="white")) 
	gg

	plotListSD[[betaScenario]] <- gg

	ggsave(gg, file = paste0("figures/fixedM_standard_error_a_",a, "_b_" , b, ".png"), 
		   width = 8, height = 4, dpi = 300)

	## Bias 
	dfBias <- reshape2::melt(bias[betaScenario,,,])
	colnames(dfBias) <- c("nCal", "M", "cppp", "value")


	dfBias$nCal <- as.factor(dfBias$nCal)
	dfBias$M <- as.factor(dfBias$M)
	dfBias$cppp <- as.factor(dfBias$cppp)
	levels(dfBias$nCal ) = paste0("r = ", levels(dfBias$nCal ))

	gg <- ggplot(dfBias, aes(x= M,  y= value, group = cppp, 
		 color=cppp)) +
		  xlab(xAxis) +
		  geom_point(aes(shape = cppp), alpha=0.80) + geom_line(alpha=0.80) + 
	      scale_color_brewer(palette = "Dark2") +
		  scale_y_continuous(breaks = seq(0, 0.06, by = 0.01)) + 
		  facet_wrap(. ~ nCal, nrow = 1) +
	  	  theme_bw() + 
	  	  expand_limits(y = 0.06) +
	  	  ylab("|bias|") + 	 
	  	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
	      theme(axis.text.x = element_text(angle = 90, hjust = 1),
		  strip.background = element_rect(fill="white")) 
	
	plotListBIAS[[betaScenario]] <- gg
	
	
	ggsave(gg, file = paste0("figures/fixedM_bias_a_",a, "_b_" , b, ".png"), 
		   width = 8, height = 4, dpi = 300)

	## rmse 
	dfRmse <- reshape2::melt(rmse[betaScenario,,,])
	colnames(dfRmse) <- c("nCal", "M", "cppp", "value")


	dfRmse$nCal <- as.factor(dfRmse$nCal)

	dfRmse$M <- as.factor(dfRmse$M)
	dfRmse$cppp <- as.factor(dfRmse$cppp)
	levels(dfRmse$nCal ) = paste0("r = ", levels(dfRmse$nCal ))

	gg <- ggplot(dfRmse, aes(x= M,  y= value, group = cppp, 
		 color=cppp)) +
		  xlab(xAxis) +
		  geom_point(aes(shape = cppp), alpha=0.80) + geom_line(alpha=0.80) + 
	      scale_color_brewer(palette = "Dark2") +
		  scale_y_continuous(breaks = seq(0, 0.06, by = 0.01)) + 
		  facet_wrap(. ~ nCal, nrow = 1) +
	  	  theme_bw() + 
	  	  expand_limits(y = 0.06) +
	  	  ylab("RMSE") +  		 
	  	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
	      theme(axis.text.x = element_text(angle = 90, hjust = 1),
		  strip.background = element_rect(fill="white")) 
	
	plotListRMSE[[betaScenario]] <- gg

	

	ggsave(gg, file = paste0("figures/fixedM_rmse_a_",a, "_b_" , b, ".png"), 
		   width = 8, height = 4, dpi = 300)

}

# ########################
## plot
########################

# library(cowplot)

# allPlots <- cowplot::plot_grid(
#    cowplot::plot_grid(
#     plotListSD[[1]] + theme(legend.position = "none"), 
#     plotListSD[[2]] + theme(legend.position = "none"),
#     nrow = 1
#    ),
#    cowplot::get_legend(plotListSD[[1]]  + 
#       theme(legend.position = "bottom")), 
#    rel_widths = c(1, .001), nrow=2)

# allPlots

# save_plot("figures/Fig2_SD.PNG", allPlots, 
# 		  base_height = 5, base_width = 10, ncol = 1)

plotListBIAS[[2]] <- plotListBIAS[[2]] + 
 expand_limits(y = 0.06) 
plotListBIAS[[3]] <- plotListBIAS[[3]] + 
 expand_limits(y = 0.06) 

library(ggpubr)
allPlots <- ggarrange(plotListBIAS[[3]] , 
				  plotListBIAS[[2]] ,
				  plotListSD[[3]], 
				  plotListSD[[2]],
				  plotListRMSE[[3]], 
				  plotListRMSE[[2]], 
                  common.legend = T, # COMMON LEGEND
                  legend = "bottom", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 2, nrow = 3) 

ggsave("figures/FigSM_fixedM_BetaExperiment.PNG", allPlots, 
		width = 10, height = 8)

