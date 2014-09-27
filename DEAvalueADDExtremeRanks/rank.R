#problem :
# u01 u02 .. u0n | u1 u2 .. un | z1 z2 .. zk
#result[i] :  
#minRank_i, maxRank_i

calculateRank = function (dmuData, transformToUtilites=TRUE) {
  if(transformToUtilites == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount, 2))
  
  for(i in 1:nrow(dmuData$data)) {
    modelMin <-createProblemModel("minRank", i, dmuData)
    modelMax <- createProblemModel("maxRank", i, dmuData)
    result[i,1] <- as.integer(get.objective(modelMin) + 1)
    result[i,2] <- as.integer(-get.objective(modelMax) + 1)
    rm(modelMin)
    rm(modelMax)
  }
  return (result)
}

createProblemModel = function (problemName, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  variablesCount = dmuCount +  2 * (dmuData$inputCount + dmuData$outputCount);
  lprec <- make.lp(0, variablesCount)
  createProblemObjective(problemName, lprec, subjectDmuIdx, dmuData)
  createConstraints(problemName, lprec, subjectDmuIdx, dmuData)
  setVariablesTypes(lprec, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(problemName, model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  if(problemName == "minRank") {
    value = 1
  }
  else {
    value = -1
  }
  objectiveVariables <- array(value, dim=dmuCount)
  objectiveVariables[subjectDmuIdx] = 0;
  restOfVariables <- array(0, dim=2 * (dmuData$inputCount + dmuData$outputCount))
  objective <- append(restOfVariables, objectiveVariables)
  set.objfn(model, objective)
}

createConstraints = function(problemName, model, subjectDmuIdx, dmuData) {
  varCount <- nrow(dmuData$data) + 2 * (dmuData$inputCount + dmuData$outputCount)
  if(problemName == "minRank"){
    createSubjectConstraintsForMin(model, subjectDmuIdx, dmuData)
  }
  else {
    createSubjectConstraintsForMax(model, subjectDmuIdx, dmuData)
  }
  createOtherConstraints(model, subjectDmuIdx, dmuData)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createCustomWeightConstraints(model, dmuData, varCount)
  }
}


#subject DMU's constraints- worstRank
createSubjectConstraintsForMax = function(model, subjectDmuIdx, dmuData) {
  CONST <- 100
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuCount + 2 * (dmuData$inputCount + dmuData$outputCount)
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1 : constrCount) {
    if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if(i == j - 2 * (dmuData$inputCount + dmuData$outputCount)) {
          result[i,j] <- CONST
        } else if (j <= (dmuData$inputCount + dmuData$outputCount)) {
          result[i, j] <- dmuData$data[subjectDmuIdx, j]
        } else if (j <= 2 * (dmuData$inputCount + dmuData$outputCount)) {
          result[i, j] <- -dmuData$data[i, (j - (dmuData$inputCount + dmuData$outputCount))]
        } else {
          result[i, j] <- 0
        }
      } 
      add.constraint(model, result[i,], "<=", CONST)
    }
  }   
}


#subject DMU's constraints - bestRank
createSubjectConstraintsForMin = function(model, subjectDmuIdx, dmuData) {
  CONST <- 100
  dmuCount <- nrow(dmuData$data)
  weightsCount <- (dmuData$inputCount + dmuData$outputCount)
  varCount <- dmuCount + 2 * weightsCount
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1 : constrCount) {
    if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if(i == j - 2 * weightsCount) {
          result[i,j] <- -CONST
        } else if (j <= weightsCount) {
          result[i, j] <- dmuData$data[i, j]
        } else if (j <= 2 * weightsCount) {
          result[i, j] <- -dmuData$data[subjectDmuIdx, (j - weightsCount)]
        } else {
          result[i, j] <- 0
        }
      } 
      add.constraint(model, result[i,], "<=", 0)
    }
  }   
}

#wagi "sztucznego" DMU musza byc takie same jak wagi DMU obliczanego, suma wag = 1
createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  data <- dmuData$data
  inputCount <- dmuData$inputCount
  outputCount <- dmuData$outputCount
  dmuCount = nrow(data)
  weightsCount = inputCount + outputCount
  varCount <- 2 * (weightsCount) + dmuCount
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

setVariablesTypes = function (model, dmuData) {
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuCount + 2 * (dmuData$inputCount + dmuData$outputCount)
  set.type(model, columns = (varCount - dmuCount + 1):varCount, type="binary")
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
