#problem w01 w02 w0q | w1 w2 ... wq | d
#result : efficiency | distance
calculateEfficiency = function (dmuData, subjectDmu,transformToUtilites=TRUE) {
  if(transformToUtilites == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }

 # print(dmuData$data)
  modelEff <- createProblemModel(subjectDmu, dmuData)
  weights <- get.variables(modelEff)
  #print(get.objective(modelEff))
  #print(weights)
  result <- array(0, dim=2)
  result[1] <- calculateEfficiencyForWeights(dmuData, subjectDmu, weights)
  result[2] <- get.objective(modelEff)
  rm(modelEff)
  return (result)
}

calculateEfficiencyForAll = function (dmuData, transformToUtilities=TRUE, normalize=FALSE) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,2))
  for(i in 1:dmuCount) {
    result[i,] <- calculateEfficiency(dmuData, i, transformToUtilities)
  }
  if(normalize==TRUE) {
    maxEff <- max(result)
    result <- result/maxEff
  }
  return (result)
}

calculateWeights = function(dmuData, subjectDmu) {
  modelEff <- createProblemModel(subjectDmu, dmuData)
  weights <- get.variables(modelEff)
  return (weights)
}

calculateEfficiencyForWeights = function (dmuData, subjectDmuIdx, weights) {
  outputs <- 0
  for(i in 1:(dmuData$inputCount + dmuData$outputCount)) {
      outputs <- outputs + weights[i] * dmuData$data[subjectDmuIdx, i]
  }
  return (outputs)
}

createProblemModel = function (subjectDmuIdx, dmuData) {
  variablesCount = 2 *  (dmuData$inputCount + dmuData$outputCount) + 1;
  lprec <- make.lp(0, variablesCount)
  createProblemObjective(lprec, subjectDmuIdx, dmuData)
  createConstraints(lprec, subjectDmuIdx, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model, subjectDmuIdx, dmuData) {
  sign <- 1
  varCount = 2 *  (dmuData$inputCount + dmuData$outputCount) + 1;
  objective <-  array(0, dim=varCount)
  objective[varCount] <- sign
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, dmuData) {
  createSubjectConstraints(model, subjectDmuIdx, dmuData)
  createOtherConstraints(model, subjectDmuIdx, dmuData)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createWeightConstraints(model, dmuData)
  }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  data <- dmuData$data
  inputCount <- dmuData$inputCount
  outputCount <- dmuData$outputCount
  dmuCount = nrow(data)
  weightsCount = inputCount + outputCount
  varCount <- 2 * (weightsCount) + 1
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  sign = "<="
  for (i in 1:constrCount) {
      for(j in 1:varCount) {
        if (j <= weightsCount) {
          result[i, j] <-  -data[subjectDmuIdx, j]
        } else if (j <= varCount - 1) {
          result[i, j] <-  data[i, j - weightsCount]
        } else {
          result[i,j] <- -1
        }
      } 
      
      add.constraint(model, result[i,], sign, 0)
    
  }  
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