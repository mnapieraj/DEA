createWeightConstraints = function (model, dmuData) {
  varCount <-  dmuData$inputCount + dmuData$outputCount
  constrCount <- nrow(dmuData$weightConstraints)
  result <- array(0, dim = varCount)
  for (i in 1 : constrCount){
    for(j in 1: varCount){
      result[j] <- dmuData$weightConstraints[i,]$weights[j]
    }
    operator <- dmuData$weightConstraints[i,]$operator
    add.constraint(model, result, operator, c(dmuData$weightConstraints[i,]$rhs))  
  }
}

createCustomWeightConstraints = function (model, dmuData, varCount) {
  weightCount <- dmuData$inputCount + dmuData$outputCount
  constrCount <- nrow(dmuData$weightConstraints)
  result <- array(0, dim = varCount)
  for (i in 1 : constrCount){
    for(j in 1: weightCount){
      result[j] <- dmuData$weightConstraints[i,]$weights[j]
    }
    operator <- dmuData$weightConstraints[i,]$operator
    add.constraint(model, result, operator , c(dmuData$weightConstraints[i,]$rhs))  
  }
}