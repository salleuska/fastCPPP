# CPPP procedure in nimble

The script `calculateCPPP` contains functions to perform the CPPP procedure described in the paper. The code uses `nimble` and can be employed for different models that ones in the paper. This is a rough description of the code.

The script `registeredDiscrepancies.R` implements a default discrepancy measure based on the data log-likelihood. 

To use this, one just needs to i) have `nimble` installed and ii) source the 2 scripts. The main function to use is called `runCalibration()`.
This function runs the CPPP procedure described in the paper using the advantages of the OPP paradigm of a `nimbleModel`

- calculate as `observedPPP` using original MCMC samples and model
- for each calibration replicate
	1. put values from iteration `r` in the model        
    2. simulate data from the model posterior predictive
    3. run mcmc to obtain replicated posterior samples        
    4. calculate `replicatedPPP`
- calculate the cppp

### Function arguments
```
model                               ## nimbleModel compiled
dataNames                           ## names of data nodes to simulate into
paramNames                          ## names of parameter nodes to simulate into
origMCMCSamples                     ## originalMCMC samples
mcmcConfFun = NULL                  ## optional configuration for nimble model (if not using the default)	
discrepancyFunctions = NULL         ## either a nimbleFunction or a list of nimbleFunctions
discrepancyFunctionsArgs = list()   ## list of list containing arguments for each of the provided nimble functions 
nCalibrationReplicates              ## number of calibration replicates
MCMCcontrol = list(niter = 1000     ## list specifying MCMC parameters for calibration replicates. Default niter= 500, nburnin=0, nthin = 1
returnSamples = TRUE                ## if TRUE save all samples from each calibration replicates                  
returnDiscrepancies = TRUE          ## if TRUE save all calculated discrepancies from each calibration replicates              
calcDisc = TRUE                     ## if TRUE calculate PPP for each calibration replicates              
parallel = FALSE                    ## NOT IMPLEMENTED option for parallel computing 
nCores = 1                          ## cores to use when parallel = TRUE              

```