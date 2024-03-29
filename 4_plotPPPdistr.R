#-----------------------------------------#
## Computational methods for fast Bayesian hierarchical model assessment via calibrated posterior p-values.
## Sally Paganin
## last update: June 2023
## R version 4.3.0 (2023-04-21) -- "Already Tomorrow"
## nimble version 0.13.2
##-----------------------------------------#
library(ggplot2)
library(latex2exp)
##-----------------------------------------#
## Dipper example
## Distribution of PPP and naive results
tt <- readRDS("sec6_examples/dipperTT/results_nCRep_1000_nIter_10000.rds")
## naive cppp
cpppTT <- mean(tt$repPPP[1,] < tt$obsPPP[1])


dfTT <- data.frame(x = tt$repPPP[1,])

plotTT <- ggplot(dfTT, aes(x=x)) + 
	geom_histogram(aes(x, after_stat(density)), position="identity", alpha=0.5,  color="gray20", fill="white")+
	geom_density(alpha=0.4, color =  "#E69F00", fill =  "#E69F00", bounds = c(0,1))+
	labs(title="Dipper example - T/T model \nNull distributon of ppp",x="ppp", y = "Density")+
	geom_vline(aes(xintercept = tt$obsPPP[1]), col = "red") + 
	theme_classic() + 
	 coord_cartesian(xlim = c(0, 1))

ggsave(plotTT, file = paste0("figures/dipperTT_pppDistribution.png"), 
 	    width = 6, height = 4, dpi = 300)


cc <- readRDS("sec6_examples/dipperCC/results_nCRep_1000_nIter_10000.rds")
## naive cppp
mean(cc$repPPP[1,] < cc$obsPPP[1])

df <- data.frame(x = cc$repPPP[1,])

plotCC <- ggplot(df, aes(x=x)) + 
	geom_histogram(aes(x, after_stat(density)), position="identity", alpha=0.5,  color="gray20", fill="white")+
	geom_density(alpha=0.4, color =  "#E69F00", fill =  "#E69F00", bounds = c(0,1))+
	labs(title="Dipper example - C/C model \nNull distributon of ppp",x="ppp", y = "Density")+
	geom_vline(aes(xintercept = cc$obsPPP[1]), col = "red") + 
	theme_classic() + 
	 coord_cartesian(xlim = c(0, 1))


ggsave(plotCC, file = paste0("figures/dipperCC_pppDistribution.png"), 
 	    width = 6, height = 4, dpi = 300)


## Newcomb example
## Distribution of PPP and naive results
res <- readRDS("sec6_examples/newcomb/results_nCRep_1000_nIter_1000.rds")
## naive cppp
cpppTT <- mean(res$repPPP[2,] < res$obsPPP[2])


dfTT <- data.frame(x = res$repPPP[2,])
plotTT <- ggplot(dfTT, aes(x=x)) + 
	geom_histogram(aes(x, after_stat(density)), position="identity", alpha=0.5,  color="gray20", fill="white")+
	geom_density(alpha=0.4, color =  "#E69F00", fill =  "#E69F00", bounds = c(0,1))+
	labs(title="Newcomb example - good mixing \nNull distributon of ppp",x="ppp", y = "Density")+
	geom_vline(aes(xintercept = res$obsPPP[2]), col = "red") + 
	theme_classic()  + 
	 coord_cartesian(xlim = c(0, 1))

ggsave(plotTT, file = paste0("figures/newcomb_pppDistribution.png"), 
 	    width = 6, height = 4, dpi = 300)


## Distribution of PPP and naive results
res <- readRDS("sec6_examples/newcombBadMixing/results_nCRep_1000_nIter_1000.rds")
## naive cppp
cpppTT <- mean(res$repPPP[2,] <= res$obsPPP[2])


dfTT <- data.frame(x = res$repPPP[2,])
plotTT <- ggplot(dfTT, aes(x=x)) + 
	geom_histogram(aes(x, after_stat(density)), position="identity", alpha=0.5,  color="gray20", fill="white")+
	geom_density(alpha=0.4, color =  "#E69F00", fill =  "#E69F00", bounds = c(0,1))+
	labs(title="Newcomb example - bad mixing \nNull distributon of ppp",x="ppp", y = "Density")+
	geom_vline(aes(xintercept = res$obsPPP[2]), col = "red") + 
	theme_classic()  + 
	 coord_cartesian(xlim = c(0, 1))

ggsave(plotTT, file = paste0("figures/newcomb_badMixingpppDistribution.png"), 
 	    width = 6, height = 4, dpi = 300)


## CapRecap simulated example
## Distribution of PPP and naive results
res <- readRDS("sec6_examples/capRecapSimulated/results_nCRep_1000_nIter_1000.rds")
## naive cppp
cpppTT <- mean(res$repPPP[1,] <= res$obsPPP[1])

dfTT <- data.frame(x = res$repPPP[1,])
plotTT <- ggplot(dfTT, aes(x=x)) + 
	geom_histogram(aes(x, after_stat(density)), position="identity", alpha=0.5,  color="gray20", fill="white")+
	geom_density(alpha=0.4, color =  "#E69F00", fill =  "#E69F00", bounds = c(0,1))+
	labs(title="Simulated example - T/T model \nNull distributon of ppp",x="ppp", y = "Density")+
	geom_vline(aes(xintercept = res$obsPPP[1]), col = "red") + 
	theme_classic()  + 
	 coord_cartesian(xlim = c(0, 1))

ggsave(plotTT, file = paste0("figures/capRecap_pppDistribution.png"), 
 	    width = 6, height = 4, dpi = 300)
