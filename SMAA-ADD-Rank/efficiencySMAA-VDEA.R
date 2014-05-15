calculateEfficiencyIntervals <- function (dmuData, samplesNo=10, intervalsNo=10, transformToUtilites=TRUE) {
  result <- calculateEfficiencySMAAForAll(dmuData, samplesNo, transformToUtilites)
  intervals <- createIntervals(result, intervalsNo)
  return(intervals)
}

calculateEfficiencySMAAForAll <- function(dmuData, samplesNo=10, transformToUtilites=TRUE) {
  if(transformToUtilites == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }
  dmuCount = nrow(dmuData$data)
  result <- c()
  source("smaa.R")
  samples <- createWeightsSamplesForSMAA(dmuData, samplesNo, vdea=TRUE)
  #print("Samples ")
  #print(samples)
  for(i in 1:dmuCount) {
    result <- rbind(result, calculateEfficiencySMAA(dmuData, i, samples))
  }
 #print(result)
  return (result)
}

calculateEfficiencySMAA <- function (dmuData, subjectDmuIdx, samples) {
  samplesNo <- nrow(samples)
  effResults  <- array(0, dim=samplesNo)
  for(i in 1:samplesNo) {
    effResults[i] <- calculateEfficiencyForWeights(dmuData, subjectDmuIdx, samples[i,])
  }
  #print(effResults)
  return (effResults)
}

createIntervals <- function (efficiencyResults, intervalsNo) {
  dmuCount <- NROW(efficiencyResults)
  samplesNo <- ncol(efficiencyResults)
  intervals  <- array(0, dim=c(dmuCount,intervalsNo))
  intervalLenght <- 1/intervalsNo
  for(i in 1:samplesNo) {
    for(j in 1:dmuCount){
      if(efficiencyResults[j,i] == 0) {
        intervalIdx = 1
      } else {
        intervalIdx <- ceiling(efficiencyResults[j,i]/intervalLenght)
      }
      intervals[j,intervalIdx] = intervals[j,intervalIdx] + 1
    }
  }
 # print(efficiencyResults)
  return (intervals)
}

calculateEfficiencyForWeights = function (dmuData, subjectDmuIdx, weights) {
  outputs <- 0
  for(i in 1:(dmuData$inputCount + dmuData$outputCount)) {
      outputs <- outputs + weights[i] * dmuData$data[subjectDmuIdx, i]
  }
  return (outputs)
}

transformToUtilityValues = function(dmuData) {
  boundaries <- dmuData$boundaries
  dmuCount <- nrow(dmuData$data)
  inputs <- dmuData$inputCount
  outputs <- dmuData$outputCount
  for(i in 1:dmuCount) {
    for(j in 1:(inputs + outputs)) {
      if(j <= inputs) {
        dmuData$data[i,j] <- (boundaries$up[j] - dmuData$data[i,j]) / (boundaries$up[j] - boundaries$low[j])
      } else if (j <= inputs + outputs) {
        dmuData$data[i,j] <- (dmuData$data[i,j] - boundaries$low[j]) / (boundaries$up[j] - boundaries$low[j])
      }
    }
  }
  return (dmuData)
}
