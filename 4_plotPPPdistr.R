library(ggplot2)
library(latex2exp)

## Dipper example
## Distribution of PPP and naive results
tt <- readRDS("dipperTT/results_nCRep_1000_nIter_10000.rds")
## naive cppp
cpppTT <- mean(tt$repPPP[1,] < tt$obsPPP[1])


dfTT <- data.frame(x = tt$repPPP[1,])

density(tt$repPPP[1,], from = 0, to = 1)

plotTT <- ggplot(dfTT, aes(x=x)) + 
	geom_histogram(stat ="density", from = 0, to = 1, position="identity", alpha=0.5) + 
	geom_histogram(color="gray20", fill="white", binwidth = 0.01)+
	geom_density(alpha=0.4, color =  "#E69F00", fill =  "#E69F00", bounds = c(0,1))+
	labs(title="Dipper T/T - null distributon of ppp",x="ppp", y = "Density")+
	geom_vline(aes(xintercept = tt$obsPPP[1]), col = "red") + 
	theme_classic() + 
	 coord_cartesian(xlim = c(0, 1))
ggsave(plotTT, file = paste0("figures/dipperTT_pppDistribution.png"), 
 	    width = 6, height = 4, dpi = 300)


cc <- readRDS("dipperCC/results_nCRep_1000_nIter_10000.rds")
## naive cppp
mean(cc$repPPP[1,] < cc$obsPPP[1])

df <- data.frame(x = cc$repPPP[1,])

plotCC <- ggplot(df, aes(x=x)) + 
	geom_histogram(aes(y=..density..), position="identity", alpha=0.5, 
		color="gray20", fill="white", binwidth = 0.01)+
	geom_density(alpha=0.4, color =  "#E69F00", fill =  "#E69F00")+
	labs(title="Dipper C/C- null distributon of ppp",x="ppp", y = "Density")+
	geom_vline(aes(xintercept = cc$obsPPP[1]), col = "red") + 
	theme_classic() + 
	 coord_cartesian(xlim = c(0, 1))


ggsave(plotCC, file = paste0("figures/dipperCC_pppDistribution.png"), 
 	    width = 6, height = 4, dpi = 300)


## Newcomb example
## Distribution of PPP and naive results
res <- readRDS("newcomb/results_nCRep_1000_nIter_1000.rds")
## naive cppp
cpppTT <- mean(res$repPPP[2,] < res$obsPPP[2])


dfTT <- data.frame(x = res$repPPP[2,])
plotTT <- ggplot(dfTT, aes(x=x)) + 
	geom_histogram(aes(y=..density..), position="identity", alpha=0.5, 
		color="gray20", fill="white", binwidth = 0.01)+
	geom_density(alpha=0.4, color =  "#E69F00", fill =  "#E69F00")+
	labs(title="Newcomb - null distributon of ppp",x="ppp", y = "Density")+
	geom_vline(aes(xintercept = res$obsPPP[2]), col = "red") + 
	theme_classic()  + 
	 coord_cartesian(xlim = c(0, 1))

ggsave(plotTT, file = paste0("figures/newcomb_pppDistribution.png"), 
 	    width = 6, height = 4, dpi = 300)


## Distribution of PPP and naive results
res <- readRDS("newcomb/badMixing/results_nCRep_1000_nIter_1000.rds")
## naive cppp
cpppTT <- mean(res$repPPP[2,] <= res$obsPPP[2])


dfTT <- data.frame(x = res$repPPP[2,])
plotTT <- ggplot(dfTT, aes(x=x)) + 
	geom_histogram(aes(y=..density..), position="identity", alpha=0.5, 
		color="gray20", fill="white", binwidth = 0.01)+
	geom_density(alpha=0.4, color =  "#E69F00", fill =  "#E69F00")+
	labs(title="Newcomb - bad mixing - null distributon of ppp",x="ppp", y = "Density")+
	geom_vline(aes(xintercept = res$obsPPP[2]), col = "red") + 
	theme_classic()  + 
	 coord_cartesian(xlim = c(0, 1))

ggsave(plotTT, file = paste0("figures/newcomb_pppDistribution.png"), 
 	    width = 6, height = 4, dpi = 300)
