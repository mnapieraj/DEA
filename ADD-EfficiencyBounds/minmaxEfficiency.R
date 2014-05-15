#problem  w1 w2 ... wq 
#result : efficiencyMin | efficiencymax
calculateMinMaxEfficiency = function (dmuData, subjectDmu,transformToUtilites=TRUE) {
  if(transformToUtilites == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }
  
  # print(dmuData$data)
  minModelEff <- createProblemModel("min", subjectDmu, dmuData)
  maxModelEff <- createProblemModel("max", subjectDmu, dmuData)
  result <- array(0, dim=2)
  result[1] <- get.objective(minModelEff)
  result[2] <- -get.objective(maxModelEff)
  rm(minModelEff)
  rm(maxModelEff)
  return (result)
}

calculateMinMaxEfficiencyForAll = function (dmuData, transformToUtilities=TRUE, normalize=FALSE) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,2))
  for(i in 1:dmuCount) {
    result[i,] <- calculateMinMaxEfficiency(dmuData, i, transformToUtilities)
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

createProblemModel = function (dir, subjectDmuIdx, dmuData) {
  variablesCount = dmuData$inputCount + dmuData$outputCount;
  lprec <- make.lp(0, variablesCount)
  createProblemObjective(dir,lprec, subjectDmuIdx, dmuData)
  createConstraints(lprec, subjectDmuIdx, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(dir, model, subjectDmuIdx, dmuData) {
  sign <- 1
  if(dir == "max") {
    sign <- -1
  } 
  varCount = (dmuData$inputCount + dmuData$outputCount);
  objective <-  array(0, dim=varCount)
  for(i in 1:varCount) {
    objective[i] <- sign * dmuData$data[subjectDmuIdx, i]
  }
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, dmuData) {
  createOtherConstraints(model, subjectDmuIdx, dmuData)
  varCount = (dmuData$inputCount + dmuData$outputCount);
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createCustomWeightConstraints(model, dmuData, varCount)
  }
}

# suma wag = 1
createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  varCount = (dmuData$inputCount + dmuData$outputCount);
  constrCount <- 1
  result <- array(1, dim = c(constrCount,varCount))
  sign <- "="
  add.constraint(model, result[constrCount,], sign , 1)
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