
#result : N(necesssary)/P(possible)/0(not dominating)


calculateEfficiencyDominanceForAll = function(dmuData, transformToUtilities=TRUE){
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,dmuCount))
  for(i in 1:dmuCount) {
    for(j in 1:dmuCount) {
      if(i != j) {
        result[i,j] = calculateEfficiencyDominance(dmuData, i, j, transformToUtilities)
      }
    }
  }
  return (result)
}

calculateEfficiencyDominance = function(dmuData, subjectDmu, relativeDmu, transformToUtilities=TRUE) {
  if(transformToUtilities == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }
  modelNecessary <- createProblemModel(dmuData, subjectDmu, relativeDmu, "N")
  modelPossible <- createProblemModel(dmuData, subjectDmu, relativeDmu, "P")
  necessary = get.objective(modelNecessary)
  possible = -get.objective(modelPossible)
  result = 0
  
  if(necessary >= 0) {
    result = "N"
  } else if (possible >= 0) {
    result = "P"
  }
 
  return (result)
}

createProblemModel = function (dmuData, subjectDmuIdx, relativeDmuIdx, dominanceType) {
  variablesCount = 2 *  (dmuData$inputCount + dmuData$outputCount) + 1;
  lprec <- make.lp(0, variablesCount)
  createProblemObjective(lprec, dmuData, dominanceType)
  createConstraints(lprec, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model,dmuData, dominanceType) {
  varCount = 2 *  (dmuData$inputCount + dmuData$outputCount) + 1;
  objective <-  array(0, dim=varCount)
  sign = 1
  if(dominanceType == "P") {
    sign = -1
  }
  objective[varCount] <- sign
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType) {
  createSubjectConstraints(model, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType)
  createOtherConstraints(model, subjectDmuIdx, dmuData)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createWeightConstraints(model, dmuData)
  }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, relativeDmuIdx, dmuData, dominanceType) {
  data <- dmuData$data
  inputCount <- dmuData$inputCount
  outputCount <- dmuData$outputCount
  dmuCount = nrow(data)
  weightsCount = inputCount + outputCount
  varCount <- 2 * (weightsCount) + 1
  constrCount <- 1
  result <- array(0, dim = varCount)
 
  sign = 1
  if(dominanceType == "P") {
    sign = -1
  }
  
  for(j in 1:varCount) {
    if (j <= weightsCount) {
      result[j] <- sign * data[subjectDmuIdx, j]
    } else if (j <= varCount - 1) {
      result[j] <- -sign * data[relativeDmuIdx, j - weightsCount]
    } else {
      result[j] <- -sign
    }
  } 
  sign = "<="
  add.constraint(model, result, sign, 0)
    
}

#wagi "sztucznego" DMU musza byc takie same jak wagi DMU obliczanego, suma wag = 1
createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  data <- dmuData$data
  inputCount <- dmuData$inputCount
  outputCount <- dmuData$outputCount
  dmuCount = nrow(data)
  weightsCount = inputCount + outputCount
  varCount <- 2 * (weightsCount) + 1
  constrCount <- weightsCount + 1
  result <- array(0, dim = c(constrCount,varCount))
  sign <- "="
  for (i in 1:(constrCount-1)) {
    result[i,i] <- -data[subjectDmuIdx, i]
    result[i, i + weightsCount] <- data[subjectDmuIdx, i]
    add.constraint(model, result[i,], sign, 0)
  }
  for(i in 1:weightsCount) {
    result[constrCount, i] <- 1
  }
  add.constraint(model, result[constrCount,], sign, 1)
  set.bounds(model, lower = -Inf, columns = varCount)
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