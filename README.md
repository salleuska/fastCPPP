# fastCPPP

Supplementary NIMBLE code for the paper "Computational methods for Bayesian hierarchical model assessment"



Make sure to have the following `R` packages installed.

```r
install.packages("nimble")

## packages for output processing and visualization
install.packages("ggplot2")
install.packages("latex2exp")
install.packages("cowplot")

install.packages("reshape2")
install.packages("bayestestR")

install.packages("extraDistr")

## package for the dipper examples
install.packages("R2ucare")  ## for function marray


```

### Folder organization


```bash

├── 1_runCPPP.R
├── 2_runMonteCarloCPPP.R
├── calculateCPPP.R
├── README.md
└── sec4_betaBinomialExample   ## Reproduce figures in Section 4

```