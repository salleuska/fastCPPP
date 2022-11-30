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

## total computational cost
# compCost <- c(5, 10, 20, 50)*1000
compCost <- c(5, 20)*1000
## number of MCMCM samples 
M <- c(10, 20, 50, 100, 200, 500, 1000)

variance <- array(0, dim = c(length(aVec), length(compCost), length(M), length(cppp0)))
bias <- array(0, dim = c(length(aVec), length(compCost), length(M), length(cppp0)))
#betaScenario <- 4
for(betaScenario in 1:length(aVec)){
	
	## Set the scenario for the null ppp distribution 
	## Uniform - round - skewed left - skewed right - U shape
	a <- aVec[betaScenario]
	b <- bVec[betaScenario]

	## compute the corresponsing pppObs
	pppObs <- qbeta(cppp0, a, b, lower.tail = TRUE)

	for(j in 1:length(compCost)){
		
		## number of calibration replicates
		R <- round(compCost[j]/M)
		# for(m in 1:length(M)){
		for(m in 1:length(M)){
			for(i in 1:length(pppObs)){
				## Bias 
				bias[betaScenario, j, m, i] <- abs(pbbinom(M[m]*pppObs[i], M[m], a, b) - cppp0[i])
				## variance
				variance[betaScenario, j, m ,i] <-  (pbbinom(M[m]*pppObs[i], M[m], a, b)*(1 - pbbinom(M[m]* pppObs[i], M[m], a, b)))/R[m] 
			}

		}	
	}

}


# pp <- qbeta(0.2, 2, 2, lower.tail = TRUE)

# pbbinom(10*pp, 10, 2,2)
# pbbinom(20*pp, 20, 2,2)
# pbbinom(50*pp, 50, 2,2)


# pbeta(pp, 2,2)

# ss <- pbbinom(runif(1000, 0, 10), 10, 2, 2)
# xx <- pbeta(runif(1000, 0, 1), 2, 2)


# plot(density(ss))
# qqplot(ss, xx)

# curve(pbeta(x, 2, 2), 0, 1)



dimnames(bias) <- list(1:length(aVec), compCost, M, cppp0)
dimnames(variance) <- list(1:length(aVec), compCost, M, cppp0)

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
	colnames(dfSd) <- c("ComputationalCost", "M", "cppp", "value")
	dfSd$Replicates <- dfSd$ComputationalCost/dfSd$M

	dfSd$MCrep <- paste0("ESS =" , dfSd$M, ", rep = ", dfSd$Replicates)

	dfSd$ComputationalCost <- as.factor(dfSd$ComputationalCost)
	dfSd$MCrep <- as.factor(dfSd$MCrep)
	dfSd$M <- as.factor(dfSd$M)
	dfSd$cppp <- as.factor(dfSd$cppp)
	levels(dfSd$ComputationalCost ) = paste0("c = ", levels(dfSd$ComputationalCost ))
	
	gg2 <- ggplot(dfSd, aes(x= M,  y= value, 
							group= interaction(cppp, ComputationalCost))) +
		  xlab(xAxis) +
		  geom_point(aes(color = cppp, shape = cppp), alpha=0.80) + 
		  geom_line(aes(linetype= ComputationalCost, color = cppp), alpha=0.80) + 
	      scale_color_brewer(palette = "Dark2") +
		  scale_y_continuous(breaks = seq(0, max(dfSd$value) + 0.02, by = 0.02)) + 
	  	  theme_bw() + 
	  	  # ylab(TeX("\\sqrt{variance}")) + 
	  	  ylab("standard error") + 
	  	  expand_limits(y = max(dfSd$value) + 0.05) 
	  	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
	      theme(axis.text.x = element_text(angle = 90, hjust = 1),
	  		strip.background = element_rect(fill="white")) 

	plotListSD2[[betaScenario]] <- gg2

	gg <- ggplot(dfSd, aes(x= M,  y= value, group = cppp, 
		 color=cppp)) +
		  xlab(xAxis) +
		  geom_point(aes(shape = cppp), alpha=0.80) + geom_line(alpha=0.80) + 
	      scale_color_brewer(palette = "Dark2") +
		  scale_y_continuous(breaks = seq(0, max(dfSd$value) + 0.02, by = 0.02)) + 
		  facet_wrap(. ~ ComputationalCost, nrow = 1) +
	  	  theme_bw() + 
	  	  # ylab(TeX("\\sqrt{variance}")) + 
	  	  ylab("standard error") + 
	  	  expand_limits(y = max(dfSd$value) + 0.05) + 
		  geom_text(aes(label = paste0("r=", Replicates), y = max(value) + 0.03),
				  	   color = "black", angle = 90, vjust = 0) + 
	  	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
	      theme(axis.text.x = element_text(angle = 90, hjust = 1),
		  strip.background = element_rect(fill="white")) 
	gg

	plotListSD[[betaScenario]] <- gg

	ggsave(gg, file = paste0("figures/standard_error_a_",a, "_b_" , b, ".png"), 
		   width = 8, height = 4, dpi = 300)

	## Bias 
	dfBias <- reshape2::melt(bias[betaScenario,,,])
	colnames(dfBias) <- c("ComputationalCost", "M", "cppp", "value")
	dfBias$Replicates <- dfBias$ComputationalCost/dfBias$M

	dfBias$MCrep <- paste0("ESS =" , dfBias$M, ", rep = ", dfBias$Replicates)

	dfBias$ComputationalCost <- as.factor(dfBias$ComputationalCost)
	dfBias$MCrep <- as.factor(dfBias$MCrep)
	dfBias$M <- as.factor(dfBias$M)
	dfBias$cppp <- as.factor(dfBias$cppp)
	levels(dfBias$ComputationalCost ) = paste0("c = ", levels(dfBias$ComputationalCost ))

	gg <- ggplot(dfBias, aes(x= M,  y= value, group = cppp, 
		 color=cppp)) +
		  xlab(xAxis) +
		  geom_point(aes(shape = cppp), alpha=0.80) + geom_line(alpha=0.80) + 
	      scale_color_brewer(palette = "Dark2") +
		  scale_y_continuous(breaks = seq(0, max(dfBias$value) + 0.02, by = 0.02)) + 
		  facet_wrap(. ~ ComputationalCost, nrow = 1) +
	  	  theme_bw() + 
	  	  ylab("|bias|") + 
	  	  expand_limits(y = max(dfBias$value) + 0.05) + 
		  geom_text(aes(label = paste0("r=", Replicates), y = max(value) + 0.03),
				  	   color = "black", angle = 90, vjust = 0) + 
	  	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
	      theme(axis.text.x = element_text(angle = 90, hjust = 1),
		  strip.background = element_rect(fill="white")) 
	
	plotListBIAS[[betaScenario]] <- gg
	
	gg2 <- ggplot(dfBias, aes(x= M,  y= value, 
							group= interaction(cppp, ComputationalCost))) +
		  xlab(xAxis) +
		  geom_point(aes(color = cppp, shape = cppp), alpha=0.80) + 
		  geom_line(aes(linetype= ComputationalCost, color = cppp), alpha=0.80) + 
	      scale_color_brewer(palette = "Dark2") +
		  scale_y_continuous(breaks = seq(0, max(dfBias$value) + 0.02, by = 0.02)) + 
	  	  theme_bw() + 
	  	  # ylab(TeX("\\sqrt{variance}")) + 
	  	  ylab("|bias|") + 
	  	  expand_limits(y = max(dfBias$value) + 0.05) + 
		  # geom_text(aes(label = paste0("r=", Replicates), y = max(value) + 0.03),
				#   	   color = "black", angle = 90, vjust = 0) + 
	  	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
	      theme(axis.text.x = element_text(angle = 90, hjust = 1),
	  		strip.background = element_rect(fill="white")) 

	plotListBIAS2[[betaScenario]] <- gg2


	ggsave(gg, file = paste0("figures/bias_a_",a, "_b_" , b, ".png"), 
		   width = 8, height = 4, dpi = 300)

	## rmse 
	dfRmse <- reshape2::melt(rmse[betaScenario,,,])
	colnames(dfRmse) <- c("ComputationalCost", "M", "cppp", "value")
	dfRmse$Replicates <- dfRmse$ComputationalCost/dfRmse$M

	dfRmse$MCrep <- paste0("ESS =" , dfRmse$M, ", rep = ", dfRmse$Replicates)

	dfRmse$ComputationalCost <- as.factor(dfRmse$ComputationalCost)
	dfRmse$MCrep <- as.factor(dfRmse$MCrep)
	dfRmse$M <- as.factor(dfRmse$M)
	dfRmse$cppp <- as.factor(dfRmse$cppp)
	levels(dfRmse$ComputationalCost ) = paste0("c = ", levels(dfRmse$ComputationalCost ))

	gg <- ggplot(dfRmse, aes(x= M,  y= value, group = cppp, 
		 color=cppp)) +
		  xlab(xAxis) +
		  geom_point(aes(shape = cppp), alpha=0.80) + geom_line(alpha=0.80) + 
	      scale_color_brewer(palette = "Dark2") +
		  scale_y_continuous(breaks = seq(0, max(dfRmse$value) + 0.02, by = 0.02)) + 
		  facet_wrap(. ~ ComputationalCost, nrow = 1) +
	  	  theme_bw() + 
	  	  ylab("RMSE") + 
	  	  expand_limits(y = max(dfRmse$value) + 0.05) + 
		  geom_text(aes(label = paste0("r=", Replicates), y = max(value) + 0.03),
				  	   color = "black", angle = 90, vjust = 0) + 
	  	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
	      theme(axis.text.x = element_text(angle = 90, hjust = 1),
		  strip.background = element_rect(fill="white")) 
	
	plotListRMSE[[betaScenario]] <- gg

	gg2 <- ggplot(dfRmse, aes(x= M,  y= value, 
							group= interaction(cppp, ComputationalCost))) +
		  xlab(xAxis) +
		  geom_point(aes(color = cppp, shape = cppp), alpha=0.80) + 
		  geom_line(aes(linetype= ComputationalCost, color = cppp), alpha=0.80) + 
	      scale_color_brewer(palette = "Dark2") +
		  scale_y_continuous(breaks = seq(0, max(dfRmse$value) + 0.02, by = 0.02)) + 
	  	  theme_bw() + 
	  	  # ylab(TeX("\\sqrt{variance}")) + 
	  	  ylab("RMSE") + 
	  	  expand_limits(y = max(dfRmse$value) + 0.05) + 
		  # geom_text(aes(label = paste0("r=", Replicates), y = max(value) + 0.03),
				#   	   color = "black", angle = 90, vjust = 0) + 
	  	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
	      theme(axis.text.x = element_text(angle = 90, hjust = 1),
	  		strip.background = element_rect(fill="white")) 

	plotListRMSE2[[betaScenario]] <- gg2

	ggsave(gg, file = paste0("figures/rmse_a_",a, "_b_" , b, ".png"), 
		   width = 8, height = 4, dpi = 300)

}

##################
## Override bias plot for scenario 2 to have the same scale ot
## scenario 3 in the paper
##################
betaScenario <- 2

a <- aVec[betaScenario]
b <- bVec[betaScenario]

## Bias 
dfBias <- reshape2::melt(bias[betaScenario,,,])
colnames(dfBias) <- c("ComputationalCost", "M", "cppp", "value")
dfBias$Replicates <- dfBias$ComputationalCost/dfBias$M

dfBias$MCrep <- paste0("ESS =" , dfBias$M, ", rep = ", dfBias$Replicates)

dfBias$ComputationalCost <- as.factor(dfBias$ComputationalCost)
dfBias$MCrep <- as.factor(dfBias$MCrep)
dfBias$M <- as.factor(dfBias$M)
dfBias$cppp <- as.factor(dfBias$cppp)
levels(dfBias$ComputationalCost ) = paste0("c = ", levels(dfBias$ComputationalCost ))

gg <- ggplot(dfBias, aes(x= M,  y= value, group = cppp, 
	 color=cppp)) +
	  xlab(xAxis) +
	  geom_point(aes(shape = cppp), alpha=0.80) + geom_line(alpha=0.80) + 
      scale_color_brewer(palette = "Dark2") +
	  scale_y_continuous(breaks = seq(0, max(dfBias$value) + 0.04, by = 0.02)) + 
	  facet_wrap(. ~ ComputationalCost, nrow = 1) +
  	  theme_bw() + 
  	  ylab("|bias|") + 
  	  expand_limits(y = max(dfBias$value) + 0.02 + 0.05) + 
	  geom_text(aes(label = paste0("r=", Replicates), y = max(value) + 0.02 + 0.03),
			  	   color = "black", angle = 90, vjust = 0) + 
  	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
	  strip.background = element_rect(fill="white")) 

plotListBIAS[[2]] <- gg
ggsave(gg, file = paste0("figures/bias_a_",a, "_b_" , b, ".png"), 
		   width = 8, height = 4, dpi = 300)

gg2 <- ggplot(dfBias, aes(x= M,  y= value, 
						group= interaction(cppp, ComputationalCost))) +
	  xlab(xAxis) +
	  geom_point(aes(color = cppp, shape = cppp), alpha=0.80) + 
	  geom_line(aes(linetype= ComputationalCost, color = cppp), alpha=0.80) + 
      scale_color_brewer(palette = "Dark2") +
	  scale_y_continuous(breaks = seq(0, max(dfBias$value) + 0.02 + 0.02, by = 0.02)) + 
  	  theme_bw() + 
  	  # ylab(TeX("\\sqrt{variance}")) + 
  	  ylab("|bias|") + 
  	  expand_limits(y = max(dfBias$value) + 0.05+ 0.02) + 
	  # geom_text(aes(label = paste0("r=", Replicates), y = max(value) + 0.03),
			#   	   color = "black", angle = 90, vjust = 0) + 
  	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
  		strip.background = element_rect(fill="white")) 
plotListBIAS2[[2]] <- gg2

###################


# For slides
yMax <- max(dfRmse$value)

gg <- ggplot(dfRmse, aes(x= M,  y= value, group = cppp, 
 color=cppp)) +
  xlab(xAxis) +
  geom_point(aes(shape = cppp), alpha=0.80) + geom_line(alpha=0.80) + 
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = seq(0, yMax + 0.02, by = 0.02)) + 
  facet_wrap(. ~ ComputationalCost, nrow = 1) +
	  theme_bw() + 
	  ylab("RMSE") + 
	  expand_limits(y = yMax+ 0.05) + 
  geom_text(aes(label = paste0("r=", Replicates), y = yMax + 0.03),
		  	   color = "black", angle = 90, vjust = 0) + 
	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
  strip.background = element_rect(fill="white")) 

gg

ggsave(gg, file = paste0("figures/rmse_a_",a, "_b_" , b, ".png"), 
	   width = 8, height = 4, dpi = 300)

yMax <- max(dfBias$value)

gg <- ggplot(dfBias, aes(x= M,  y= value, group = cppp, 
 color=cppp)) +
  xlab(xAxis) +
  geom_point(aes(shape = cppp), alpha=0.80) + geom_line(alpha=0.80) + 
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = seq(0, yMax + 0.02, by = 0.02)) + 
  facet_wrap(. ~ ComputationalCost, nrow = 1) +
	  theme_bw() + 
	  ylab("|bias|") + 
	  expand_limits(y = yMax+ 0.05) + 
  geom_text(aes(label = paste0("r=", Replicates), y = yMax + 0.03),
		  	   color = "black", angle = 90, vjust = 0) + 
	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
  strip.background = element_rect(fill="white")) 

gg

ggsave(gg, file = paste0("figures/bias_a_",a, "_b_" , b, ".png"), 
	   width = 8, height = 4, dpi = 300)


yMax <- max(dfSd$value)

gg <- ggplot(dfSd, aes(x= M,  y= value, group = cppp, 
 color=cppp)) +
  xlab(xAxis) +
  geom_point(aes(shape = cppp), alpha=0.80) + geom_line(alpha=0.80) + 
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(breaks = seq(0, yMax + 0.02, by = 0.02)) + 
  facet_wrap(. ~ ComputationalCost, nrow = 1) +
	  theme_bw() + 
	  ylab("sd") + 
	  expand_limits(y = yMax+ 0.05) + 
  geom_text(aes(label = paste0("r=", Replicates), y = yMax + 0.03),
		  	   color = "black", angle = 90, vjust = 0) + 
	  ggtitle(paste0("Scenario Beta(", a, "," , b, ")"))  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
  strip.background = element_rect(fill="white")) 

gg

ggsave(gg, file = paste0("figures/sd_a_",a, "_b_" , b, ".png"), 
	   width = 8, height = 4, dpi = 300)


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

	

library(ggpubr)
allPlots <- ggarrange(plotListBIAS[[3]], 
				  plotListBIAS[[2]] ,
				  plotListSD[[3]], 
				  plotListSD[[2]],
				  plotListRMSE[[3]], 
				  plotListRMSE[[2]], 
                  common.legend = T, # COMMON LEGEND
                  legend = "bottom", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 2, nrow = 3) 

ggsave("figures/Fig2_BetaExperiment.PNG", allPlots, 
		width = 10, height = 11)


plotListBIAS2[[1]]

library(ggpubr)
allPlots2 <- ggarrange(plotListBIAS2[[3]], 
				  plotListBIAS2[[2]] ,
				  plotListBIAS2[[4]],
				  plotListSD2[[3]], 
				  plotListSD2[[2]],
				  plotListSD2[[4]], 
				  plotListRMSE2[[3]], 
				  plotListRMSE2[[2]], 
				  plotListRMSE2[[4]], 
                  common.legend = T, # COMMON LEGEND
                  legend = "bottom", # legend position
                  align = "hv", # Align them both, horizontal and vertical
                  ncol = 3, nrow = 3) 
allPlots2
ggsave("figures/Fig2_BetaExperiment2.PNG", allPlots2, 
		width = 10, height = 11)

########################
## Test fancy version
########################
# library(ggpubr)

# curveColors <- brewer.pal(3, "Dark2")

# p <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) + 
#         stat_function(fun = dbeta, args = list(aVec[1], bVec[1]),
#                       colour = curveColors[1], size = 1.5) +
#           geom_area(stat = "function", fun = dbeta, args = list(aVec[1], bVec[1]),
#           	fill = curveColors[1], alpha = 0.6) +
#         scale_x_continuous(breaks = seq(0, 1, 0.2),
#                               limits=c(0, 1)) + 
#         ylab("") + xlab("") + coord_cartesian(ylim = c(0,4))  +
# 		theme_bw() + theme(legend.position="none")  
# p

# pX <- p + clean_theme() + rremove("legend")

# mainplot <- plotListRMSE[[1]] + ggtitle("") + rremove("legend")

# library(cowplot)
# plot_grid(pX, NULL, mainplot, NULL, ncol = 2, align = "hv", 
#           rel_widths = c(2, 1), rel_heights = c(1, 2))

