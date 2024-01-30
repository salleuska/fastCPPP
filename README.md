# fastCPPP

Supplementary code for the paper "Computational methods for fast Bayesian hierarchical model assessment via calibrated posterior p-values."

The CPPP procedure is implemented using the `nimble` software and compatible with other models written using the `nimble` language. The code in this repository has been written for the purpose of running different experiments for the manuscript. There are some plans to extend this to be broadly accessible and user friendly in the future. If you want to try this out, you can jump at the tutorial at [the end](#what-if-i-just-want-to-try-out-the-cppp-approximation).


## Note


This folder comes with some pre-computed `.rds` results for the different example to allow to reproduce figures in the manuscript as running the script `2_runMonteCarloCPPP.R` for Monte Carlo simulation of all the procedure is computationally intensive. 

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
├── CPPPfunctions             ## Containts the main function for the cppp procedure
├── figures                   ## output folder for figures
├── sec4_betaBinomialExample  ## code to reproduce example and plot in Section 4
├── sec6_examples             ## containts one folder for each examples in Section 6	
└── slurmScripts              ## scripts for computing cluster using SLURM
```

## R scripts

The following R scripts are parametrized so that the same script can be used for different examples. Arguments of the scripts are described at the beginning of the R script. 

Script `1` run the cppp procedure for one example, while script `2` performs a Monte Carlo simulation of all the cppp procedure.

Scripts `3_*` uses outputs from scripts `1` and `2` to compute different estimates of the cppp and its variance under various scenarios of computational cost (i.e. number of MCMC samples and calibration replicates). 

Scripts `4_*` make plots in Section 6 of the paper and in the supplementary material.

```bash

├── 1_runCPPP.R                       ## run cppp procedure for one example
├── 2_runMonteCarloCPPP.R             ## run the cppp procedure multiple times for brute force Monte Carlo estimation
├── 3_computeBootstrapSE.R            ## compute Bootstrap estimates of the cppp standard error + coverage
├── 3_computeMCSE.R                   ## compute brute force estimate of the cppp standard error via Monte carlo 
├── 3_computePluginSE.R               ## compute Plug-in estimates of the cppp standard error + coverage
├── 4_coverage.R                      ## extract results for coverage e
├── 4_plotPPPdistr.R                  ## plot the distribution of the ppp for the examples
├── 4_plotResults.R                   ## reproduce plots in Section6 with cppp estimates and variance estimates

```

## Usage

The bash scripts `main_EXAMPLENAME.sh` contains all the steps to reproduce results for each of the examples presented in the paper. 

Running the bash script can be memory/time-consuming, especially when calling `2_runMonteCarloCPPP.R`. For results in the paper, I made use of cluster computing managed via SLURM; those scripts are in the `slurmScripts` folder.

All scripts (except `4_plotPPPdistr.R` and `5_times.R`) are parametrized and can be run separately. Details are given in the comments at the beginning of each file. For example to run the CPPP procedure for the newcomb data:

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

Notice that in each folder there is a script named `model.R`, including a statistical model coded in nimble, with initial values and constants, and discrepancy functions. It also contains the details regarding the MCMC settings for the first MCMC run.

Info on the parameters for `1_runCPPP.R`; output will be a list  saved in an `.rds` file. 

```bash
--dirExample               ## path to folder containing files for input and outputs 
--dataPath                 ## path to data
--runOriginal              ## if TRUE then run first original MCMC 
--nCalibrationReplicates   ## number of calibration replicates
--nIterMCMC                ## number of mcmc samples per calibration replicates
--returnSamples            ## if TRUE save and return all samples
--returnDiscrepancies      ## if TRUE save and return all computed discrepancies
--calcDisc                 ## if TRUE compute the PPP
```


## Steps for reproducing Figures

### Section 4 

Figures 3 in Section 4 (and Figure S2 in SM)

```
Rscript sec4_betaBinomialExample/1_plotFixedCC.R
Rscript sec4_betaBinomialExample/1_SM_plotFixedM.R
```

### Section 6

Figures in Section 6; the scripts uses pre-computed values of the cppp variance using different methods in the paper. The results are saved in each example folder in `sec6_examples/[name_example]/[file.rds]`  where `file.rds` can be:

 * `varianceMC.rds` - cppp variance estimated via Monte Carlo (baseline)
 * `variancePlugin.rds` - cppp variance estimated via plug-in and using a transfer estimator
 * `varianceBootstrap.rds` - cppp variance estimated via Bootstrap (Bootstrap-normal and Bootstrap-Plugin)

```bash
Rscript 4_plotResults.R --dirExample="sec6_examples/newcomb" --plotTitle="Newcomb example - good mixing"

Rscript 4_plotResults.R --dirExample="sec6_examples/newcombBadMixing" --plotTitle="Newcomb example - bad mixing"

Rscript 4_plotResults.R --dirExample="sec6_examples/dipperTT" --plotTitle="Dipper example - T/T model"

Rscript 4_plotResults.R --dirExample="sec6_examples/dipperCC" --plotTitle="Dipper example - C/C model"

Rscript 4_plotResults.R --dirExample="sec6_examples/capRecapSimulated" --plotTitle="Simulated example - T/T model"

```

### What if I just want to try out the cppp approximation?

Minimum requirements are `1_runCPPP.R` script and the following package.

```r
install.packages("nimble")

## packages for cppp variance estimation
install.packages("maotai")  ## moving block bootstrap
install.packages("mcmcse")  ## mcmc variance
```
A minimal example is given [here](https://htmlpreview.github.io/?https://github.com/salleuska/fastCPPP/blob/main/Example_Newcomb.html)
