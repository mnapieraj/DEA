

calculateCrossEfficiencyForAll = function(dmuData, subjectDmu, transformToUtilities=TRUE) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,1))
  for(i in 1:dmuCount) {
    result[i] <- calculateCrossEfficiency(dmuData, i, transformToUtilities)
  }
  
  return (result)  
}

calculateCrossEfficiency = function(dmuData, subjectDmu, transformToUtilites=TRUE) {
   source("efficiency.R")
  if(transformToUtilites == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }
  dmuCount = nrow(dmuData$data)
  efficiency = 0
  
 
  for(i in 1:dmuCount) {
    if(i != subjectDmu) {
      weights = calculateWeights(dmuData, i)
      efficiency = efficiency + calculateEfficiencyForWeights(dmuData, subjectDmu, weights)
    }
  }  
  efficiency = efficiency / (dmuCount - 1)
  return (efficiency)
}