calculateEfficiencyIntervals <- function (dmuData, samplesNo=10, intervalsNo=10) {
  result <- calculateEfficiencySMAAForAll(dmuData, samplesNo)
  intervals <- createIntervals(result, intervalsNo)
  return(intervals)
}

calculateEfficiencySMAAForAll <- function(dmuData, samplesNo=10) {
  dmuCount = nrow(dmuData$data)
  result <- c()
  source("smaa.R")
  samples <- createWeightsSamplesForSMAA(dmuData, samplesNo)
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
    maxEff <- max(efficiencyResults[,i])
    efficiencyResults[,i] <- efficiencyResults[,i]/maxEff
    for(j in 1:dmuCount){
      if(efficiencyResults[j,i] == 0) {
        intervalIdx = 1
      } else {
        intervalIdx <- ceiling(efficiencyResults[j,i]/intervalLenght)
      }
      intervals[j,intervalIdx] = intervals[j,intervalIdx] + 1
    }
  }
  return (intervals)
}

calculateEfficiencyForWeights = function (dmuData, subjectDmuIdx, weights) {
  inputs <- 0
  outputs <- 0
  for(i in 1:(dmuData$inputCount + dmuData$outputCount)) {
    if(i <= dmuData$inputCount) {
      inputs <- inputs + weights[i] * dmuData$data[subjectDmuIdx, i]
    } else {
      outputs <- outputs + weights[i] * dmuData$data[subjectDmuIdx, i]
    }
  }
  return (outputs / inputs)
}

