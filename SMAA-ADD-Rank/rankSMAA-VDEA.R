calculateRankSMAAForAll <- function(dmuData, samplesNo=10) {
  dmuCount = nrow(dmuData$data)
  result <- c()
  source("smaa.R")
  source("efficiencySMAA-VDEA.R")
  effResults <- calculateEfficiencySMAAForAll(dmuData, samplesNo)
  result <- calculateRankSMAA(effResults)
  intervals <- createSummary(result, dmuCount)
  
  return (intervals)
}

calculateRankSMAA <- function (effResults) {
  maxRank <- NROW(effResults) + 1
  dmuCount <- ncol(effResults)
  for(i in 1:ncol(effResults)) {
    effResults[,i] <- maxRank - rank(effResults[,i], ties.method="min")
  }
  return (effResults)
}

createSummary <- function (ranks, intervalsNo) {
  dmuCount <- NROW(ranks)
  intervals  <- array(0, dim=c(dmuCount,intervalsNo))
  for(i in 1:dmuCount) {
    for(j in 1:length(ranks[i,])){
      intervals[i, ranks[i,j]] = intervals[i,ranks[i,j]] + 1
    }
  }
  return (intervals)
}

