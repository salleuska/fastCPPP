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

### Organization

## Folders 

```bash
├── CPPPfunctions			    ## Containts the main function for the cppp procedure - the scripts can be used by users to use the procedure for their own models
├── figures						## output folder for figures
├── sec4_betaBinomialExample    ## code to reproduce example and plot in section 4
├── sec6_examples				## containts one folder for each examples in section 6	
└── slurmScripts				## scripts for computing cluster using SLURM
```

## R scripts

The following R scripts are parametrized so that the same script can be used for different datasets. Arguments of the scripts are described at the beginning of the R script. 

Scripts `1` run the cppp procedure for one example, while script `2` performs a Monte Carlo simulation of all the cppp procedure; lots of computations here so I have been usint 

Scripts `3` uses outputs from scripts `1` and `2` to compute different estimates of the cppp and its variance under various scenarios of computational allocation (i.e. number of MCMC samples and calibration replicates). 

Scripts `4` reproduce plots in Section 6 of the paper. 

```bash

├── 1_runCPPP.R 					  ## run cppp procedure for one example
├── 2_runMonteCarloCPPP.R 		      ## run the cppp procedure multiple times for brute force Monte Carlo estimation
├── 3_computeBootstrapSE.R            ## compute Bootstrap estimates of the cppp standard error + coverage
├── 3_computeMCSE.R                   ## compute brute force estimate of the cppp standard error via Monte carlo 
├── 3_computePluginSE.R               ## compute Plug-in estimates of the cppp standard error + coverage
├── 4_coverage.R                      ## extract results for coverage e
├── 4_plotPPPdistr.R                  ## plot the distribution of the ppp for the examples
├── 4_plotResults.R                   ## reproduce plots in Section6 with cppp estimates and variance estimates

```

## Usage

The bash scripts `main_EXAMPLENAME.sh` contains all the steps to reproduce results for each of the examples presented in the paper. 

Running the bash script can be memory/time-consuming, especially when calling `2_runMonteCarloCPPP.R`. For results in the paper, I made use of cluster computing managed via SLURM; those files are in the `slurmScripts` folder.

All scripts (except `4_plotPPPdistr`) are parametrized and can be run separately. Details are in the comments at the beginning of the file. For example to run the CPPP procedure for the newcomb data:

```bash
Rscript 1_runCPPP.R \
--dirExample=sec6_examples/newcomb \
--dataPath="sec6_examples/newcomb/light.txt" \
--runOriginal=TRUE \
--nCalibrationReplicates=1000 \
--nIterMCMC=1000 \
--returnSamples=TRUE \
--returnDiscrepancies=TRUE \
--calcDisc=TRUE \
```

Notice that for each folder there is a script named `model.R`, including a statistical model coded in nimble, with inits and constants, and (optional) discrepancy functions. It also contains the details on MCMC settings for the first MCMC run.

```bash
--dirExample               ## path to folder containing files for input and outputs 
--dataPath                 ## path to data
--runOriginal              ## if TRUE then run first original MCMC 
--nCalibrationReplicates   ## number of calibration replicates
--nIterMCMC                ## number of mcmc samples per calibration replicates
--returnSamples            ## if TRUE save and return all samples
--returnDiscrepancies      ## if TRUE save and return all computed discrepancies
--calcDisc                 ## if TRUE compute PPP
```
