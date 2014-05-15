calculateComparisonsSMAAForAll <- function(dmuData, samplesNo=10) {
  dmuCount = nrow(dmuData$data)
  result <- c()
  source("smaa.R")
  source("efficiencySMAA.R")
  effResults <- calculateEfficiencySMAAForAll(dmuData, samplesNo)
  result <- calculateComparisonsSMAA(effResults)
 # summary <- createSummary(result, dmuCount)
  
  return (result)
}

calculateComparisonsSMAA <- function (effResults) {
  dmuCount <- NROW(effResults)
  result <- array(0, dim=c(dmuCount, dmuCount))
  for(i in 1:ncol(effResults)) {
    for(j in 1:dmuCount) {
      for(k in 1:dmuCount) {
        if(effResults[j,i] >= effResults[k,i]) {
          result[j,k] = result[j,k] + 1
        }
          
      }
    }
  }
  return (result)
}

createSummary <- function (ranks, intervalsNo) {
  dmuCount <- NROW(ranks)
  intervals  <- array(0, dim=c(dmuCount,intervalsNo))
  for(i in 1:dmuCount) {
    for(j in 1:length(ranks[i,])){
      intervals[i, ranks[i,j]] = intervals[i,intervalIdx] + 1
    }
  }
  return (intervals)
}

