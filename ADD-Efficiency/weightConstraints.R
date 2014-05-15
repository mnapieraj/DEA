createWeightConstraints = function (model, dmuData) {
  varCount = 2 *  (dmuData$inputCount + dmuData$outputCount) + 1
  weightsCount = dmuData$inputCount + dmuData$outputCount
  constrCount <- nrow(dmuData$weightConstraints)
  result <- array(0, dim = varCount)
  for (i in 1 : constrCount){
    for(j in 1: weightsCount){
      result[j] <- dmuData$weightConstraints[i,]$weights[j]
    }
    operator <- dmuData$weightConstraints[i,]$operator
    add.constraint(model, result, operator , c(dmuData$weightConstraints[i,]$rhs))   
  }
}

createCustomWeightConstraints = function (model, dmuData, varCount) {
  weightCount <- dmuData$inputCount + dmuData$outputCount
  constrCount <- nrow(dmuData$weightConstraints)
  result <- array(0, dim = varCount)
  for (i in 1 : constrCount){
    for(j in 1: weightCount){
      result[j] <- dmuData$weightConstraints[i,]$weights[j]
     # result[j] <- dmuData$weightConstraints[i,j]
    }
    operator <- dmuData$weightConstraints[i,]$operator
    #operator <- dmuData$weightConstraints[i,j+1]
    add.constraint(model, result, operator , c(dmuData$weightConstraints[i,]$rhs))  
    #add.constraint(model, result, operator , c(dmuData$weightConstraints[i,j+2]))  
  }
}

createOrigWeightConstraints = function (model, dmuData, varCount=-1) {
 if(varCount == -1) {
  varCount <-  dmuData$inputCount + dmuData$outputCount
 }
  constrCount <- nrow(dmuData$weightConstraints)
  result <- array(0, dim = varCount)
  for (i in 1 : constrCount){
    for(j in 1: varCount){
      result[j] <- dmuData$weightConstraints[i,]$weights[j]
    }
    operator <- dmuData$weightConstraints[i,]$operator
    add.constraint(model, result, operator, c(dmuData$weightConstraints[i,]$rhs))  
  }
 return (model)
}
