#other DMU's constraints - bestRank
createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  CONST <- 1000
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuCount + dmuData$inputCount + dmuData$outputCount
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1 : constrCount) {
    if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if(i == j - (dmuData$inputCount + dmuData$outputCount)) {
          result[i,j] <- -CONST
        } else if (j <= dmuData$inputCount) {
          result[i, j] <- -dmuData$data[i, j]
        } else if (j <= dmuData$inputCount + dmuData$outputCount) {
          result[i, j] <- dmuData$data[i, j]
        } else {
          result[i, j] <- 0
        }
      } 
      add.constraint(model, result[i,], "<=", 0)
    }
  }   
}