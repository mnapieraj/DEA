transformToUtilityBounds = function(dmuData, lowerData, upperData, subjectDmuIdx, direction) {
  boundaries <- dmuData$boundaries
  dmuCount <- nrow(dmuData$data)
  inputs <- dmuData$inputCount
  outputs <- dmuData$outputCount
  for(i in 1:dmuCount) {
    for(j in 1:(inputs + outputs)) {
      if(direction == "OPT") {
        if(j <= inputs) {
          dmuData$data[i,j] <- (boundaries$up[j] - lowerData$data[i,j]) / (boundaries$up[j] - boundaries$low[j])
        } else if (j <= inputs + outputs) {
          dmuData$data[i,j] <- (upperData$data[i,j] - boundaries$low[j]) / (boundaries$up[j] - boundaries$low[j])
        }
      } else {
        if(j <= inputs) {
          dmuData$data[i,j] <- (boundaries$up[j] - upperData$data[i,j]) / (boundaries$up[j] - boundaries$low[j])
        } else if (j <= inputs + outputs) {
          dmuData$data[i,j] <- (lowerData$data[i,j] - boundaries$low[j]) / (boundaries$up[j] - boundaries$low[j])
        }
      }
    }
  }
  return (dmuData)
}

transformToBoundaryPerformance = function(dmuData, direction) {
  tolerance <- dmuData$tolerance
  dmuCount <- nrow(dmuData$data) 
  weightsCount <- dmuData$inputCount + dmuData$outputCount
  coeff <- 1 - tolerance
  if(direction == "OPT") {
    coeff <- 1 + tolerance
  }
  for(i in 1:dmuCount) {
    for(j in 1:weightsCount) {
      dmuData$data[i,j] <- dmuData$data[i,j] * coeff
    }
  }
  return(dmuData)
}

############ maximum tolerance
calculateMaxToleranceForAll = function(dmuData, maxTolerance) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,1))
  
  for(i in 1:dmuCount) {
    result[i] <- calculateMaxTolerance(dmuData, i)
  }
  
  return (result)
}

calculateMaxTolerance = function(dmuData, subjectDmu, maxTolerance=0.99, precision=0.001) {
  lowTolerance = 0
  upTolerance = maxTolerance
  currentTolerance = 0
  while((upTolerance - lowTolerance) > precision) {
    currentTolerance= (lowTolerance + upTolerance) / 2
    dmuData$tolerance = currentTolerance
    d_pes = calculateEfficiencyBounds(dmuData, subjectDmu)[4]
    if(d_pes >= 0) {
      upTolerance = currentTolerance
    } else {
      lowTolerance = currentTolerance
    }
  }
  return (lowTolerance)
}

#lowerBoundDmuData <- transformToBoundaryPerformance(dmuData, "PES")
#upperBoundDmuData <- transformToBoundaryPerformance(dmuData, "OPT")
#lowerUtilities <- transformToUtilityBounds(dmuData, lowerBoundDmuData, upperBoundDmuData, subjectDmu, "PES")
# upperUtilities <- transformToUtilityBounds(dmuData, lowerBoundDmuData, upperBoundDmuData, subjectDmu, "OPT")

#temp <- lowerUtilities$data[subjectDmu,]
#lowerUtilities$data[subjectDmu, ] <- upperUtilities$data[subjectDmu,]
#upperUtilities$data[subjectDmu,] <- temp
