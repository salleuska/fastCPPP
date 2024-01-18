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