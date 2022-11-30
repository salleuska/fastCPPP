library(cowplot)
library(ggplot2)


cppp0 <- c(0.05, 0.1, 0.4)

## null ppp distribution
a <- c(0.5, 2, 4)
b <- c(0.5, 2, 2)


# pppObs <- qbeta(cppp0, a[1], b[1],  lower.tail = TRUE)
# curve(dbeta(x,a[1], b[1]))
# abline(v = pppObs)

library(RColorBrewer)
library(ggplot2)

curveColors <- brewer.pal(3, "Accent")
        
plotList <- list()

for(i in 1:length(cppp0)) {
p <- ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
        stat_function(fun = dbeta, args = list(a[1], b[1]),
                      aes(colour = paste0("Beta(", a[1], "," , b[1], ")")), size = 1.5) +
        stat_function(fun = dbeta, args = list(a[2], b[2]),
                      aes(colour = paste0("Beta(", a[2], "," , b[2], ")")), size = 1.5) +
        stat_function(fun = dbeta, args = list(a[3], b[3]),
                      aes(colour = paste0("Beta(", a[3], "," , b[3], ")")), size = 1.5) +
        scale_x_continuous(name = "Probability",
                              breaks = seq(0, 1, 0.2),
                              limits=c(0, 1)) + 
        scale_colour_manual(values = curveColors) +
        labs(colour = "Null distribution of ppp") +
        ylab("") + ggtitle(paste0("cppp =", cppp0[i])) + 
		theme_bw()        

funcShaded <- function(x, a, b, prob) {
    y 		 		 <- dbeta(x, shape1 = a, shape2 = b)
    quantile 		 <- qbeta(prob, shape1 = a, shape2 = b, lower.tail = TRUE)
    y[x  > quantile] <- NA
    return(y)
}

p <- p + stat_function(fun=funcShaded, args = list(a = a[1], b = b[1], prob = cppp0[i]),
	                   geom="area", fill= curveColors[1], alpha=0.2, n = 500) + 
 		 geom_vline(xintercept = qbeta(cppp0[i], shape1 = a[1], shape2 = b[1], lower.tail = TRUE),
 		 			color = curveColors[1]) +
		 stat_function(fun=funcShaded, args = list(a = a[2], b = b[2], prob = cppp0[i]),
	                   geom="area", fill= curveColors[2], alpha=0.2, n = 500) + 
 		 geom_vline(xintercept = qbeta(cppp0[i], shape1 = a[2], shape2 = b[2], lower.tail = TRUE),
 		 			color = curveColors[2]) +
		 stat_function(fun=funcShaded, args = list(a = a[3], b = b[3], prob = cppp0[i]),
	                   geom="area", fill= curveColors[3], alpha=0.2, n = 500) + 
 		 geom_vline(xintercept = qbeta(cppp0[i], shape1 = a[3], shape2 = b[3], lower.tail = TRUE),
 		 			color = curveColors[3])



print(i)
print(p)
plotList[[i]] <- p

}



allPlots <- cowplot::plot_grid(
   cowplot::plot_grid(
    plotList[[1]] + theme(legend.position = "none") + coord_cartesian(ylim = c(0,3)),
    plotList[[2]] + theme(legend.position = "none") + coord_cartesian(ylim = c(0,3)),
    plotList[[3]] + theme(legend.position = "none") + coord_cartesian(ylim = c(0,3)),
    nrow = 1, align = "h"
   ),
   cowplot::get_legend(plotList[[1]]  + 
      theme(legend.position = "right")), 
   rel_widths = c(.85, .15), nrow=1)


save_plot("figures/Fig1_cpppScenarios.png", allPlots, 
		  base_height = 4, base_width = 5, ncol = 3)

