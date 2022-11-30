##-------------------------------------------------------##
## This file may contain some already implemented discrepancies
##-------------------------------------------------------##

## discrepancy base class
discrepancyFunction_BASE <- nimbleFunctionVirtual(
    run = function() returnType(double())
)

# discrepancyFunction_BASE_VEC <- nimbleFunctionVirtual(
#     run = function() returnType(double(1))
# )

## Data log-likelihood discrepancy function
logLikDiscFunction <- nimbleFunction(
  contains = discrepancyFunction_BASE,
  setup = function(model, discrepancyFunctionsArgs){
    dataNames <- discrepancyFunctionsArgs[['dataNames']]
  },
  run = function(){
    disc <- model$getLogProb(dataNames)
    returnType(double(0)) 
    return(disc)
  }
)

