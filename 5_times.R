#-----------------------------------------#
## Computational methods for fast Bayesian hierarchical model assessment via calibrated posterior p-values.
## Sally Paganin
## last update: January 2024
## R version 4.3.2 (2023-10-31) -- "Eye Holes"
##-----------------------------------------#
library(ggplot2)
library(latex2exp)
##-----------------------------------------#
## dipper TT
xx <- readRDS("sec6_examples/dipperTT/results_nCRep_1000_nIter_1000.rds")
dfTime <- data.frame(example = "dipperTT" , minutes = xx$time[3]/60)

## dipper CC
dfTime[2, ]$example <- "dipperCC"
dfTime[2, ]$minutes <- readRDS("sec6_examples/dipperCC/results_nCRep_1000_nIter_1000.rds")$time[3]/60

## capture
dfTime[3, ]$example <- "capRecapSimulated"
dfTime[3, ]$minutes <- readRDS("sec6_examples/capRecapSimulated/results_nCRep_1000_nIter_1000.rds")$time[3]/60

## newcomb
dfTime[4, ]$example <- "newcomb"
dfTime[4, ]$minutes <- readRDS("sec6_examples/newcomb/results_nCRep_1000_nIter_1000.rds")$time[3]/60

## newcombBadMixing
dfTime[5, ]$example <- "newcombBadMixing"
dfTime[5, ]$minutes <- readRDS("sec6_examples/newcombBadMixing/results_nCRep_1000_nIter_1000.rds")$time[3]/60


dfTime

## dfTime contain the time to perform 1000 calibration replicates, each with 1000 MCMC samples

dfTime$seconds <- dfTime$minutes * 60
dfTime$mcmc1000 <- dfTime$seconds/1000
dfTime$mcmc1 <- dfTime$mcmc1000/1000

dfTime$ppp <- dfTime$mcmc1* c(10000, 10000, 10000, 5000, 5000)

dfTime$c5000 <- dfTime$mcmc1* 5000
dfTime$c10000 <- dfTime$mcmc1* 10000
dfTime$c20000 <- dfTime$mcmc1* 20000
dfTime$c50000 <- dfTime$mcmc1* 50000
dfTime$naiveTime <- dfTime$ppp * 1000

dfTime <- dfTime[c(4,5,2,1,3), ]

dfTime[, c(6:11)]/60


###################
df <- data.frame(example = rep(dfTime$example, 4),
 m = rep(c(50, 100, 200, 500), each = 5), 
 mcmc1 = rep(dfTime$mcmc1, 4), 
 naiveTime = rep(dfTime$naiveTime, 4))

df$time = df$m * df$mcmc1 * 1000
df$saving = df$naiveTime/df$time

ggplot(data = df, aes(x = m, y = time/60, color = example)) + 
geom_point() + geom_line() + theme_bw() + 
xlab(TeX("\\tilde{m}")) + ylab("time in minutes") 


ggplot(data = df, aes(x = m, y = naiveTime/60, color = example)) + 
geom_point() + geom_line() + theme_bw() + 
xlab(TeX("\\tilde{m}")) + ylab("time in minutes") 