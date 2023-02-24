# fastCPPP

Supplementary code for the paper "Computational methods for fast Bayesian hierarchical model assessment via calibrated posterior p-values."

The CPPP procedure is implemented using the `nimble` software and compatible with other models written using the `nimble` language.

## Preliminaries 

Make sure to have the following `R` packages installed.

```r
install.packages("nimble")

## packages for cppp variance estimation
install.packages("maotai")  ## moving block bootstrap
install.packages("mcmcse")  ## mcmc variance

## packages for output processing and visualization
install.packages("ggplot2")
install.packages("latex2exp")
install.packages("cowplot")

install.packages("reshape2")
install.packages("bayestestR")

install.packages("extraDistr")  ## for betabinomial distribution

## package for the dipper examples
install.packages("R2ucare")  ## for function marray - capture-recapture example


```

### Folder organization

## Folders 

```bash
├── figures						## output figures
├── sec4_betaBinomialExample    ## code to reproduce example and plot in section 4
├── sec6_examples				## examples in section 6	
└── slurmScripts				## scripts for computing cluster using SLURM
```

## R scripts

The following R files are parametrized so that the same script can be used for different datasets.  

```bash

├── 1_runCPPP.R 					  ## run cppp procedure for one example
├── 2_runMonteCarloCPPP.R 		      ## rum cppp procedure multiple times 
├── 3_computeBootstrapSE.R            ## compute standard error of cppp via Bootstrap
├── 3_computeMCSE.R                   ## compute standard error of cppp via monte carlo 
├── 3_computePluginSE.R               ## compute standard error of cppp via plugin estimate
├── 4_coverage.R                      ## extract results in terms of coverate
├── 4_plotPPPdistr.R                  ## plot the distribution of the ppp for the example
├── 4_plotResults.R                   ## reproduce plots in Section6 with cppp estimates and variance estimates
├── registeredDiscrepancies.R         ## functions that calculate discrepancies

```