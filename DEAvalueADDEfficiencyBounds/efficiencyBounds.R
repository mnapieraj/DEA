#result : lowerBound, upperBound, d_opt, d_pes

calculateEfficiencyBounds = function (dmuData, subjectDmu, transformToUtilities=TRUE) {
  result <- array(0, dim=4)
  source("efficiencyBounds.R")
  maxDist <- calculateMaxDistance(dmuData, subjectDmu, transformToUtilities)
  source("efficiency.R")
  minDist <- calculateEfficiency(dmuData, subjectDmu, transformToUtilities)
  source("minmaxEfficiency.R")
  minmaxEff <- calculateMinMaxEfficiency(dmuData, subjectDmu, transformToUtilities)
  result[1] <- minmaxEff[1]
  result[2] <- minmaxEff[2]
  result[3] <- minDist[2]
  result[4] <- maxDist[1]
  return (result)
}

calculateEfficiencyBoundsForAll = function (dmuData, transformToUtilities=TRUE) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,4))
  
  for(i in 1:dmuCount) {
    result[i,] <- calculateEfficiencyBounds(dmuData, i, transformToUtilities)
  }
  return (result)
}

#problem :
# u01 u02 .. u0n | u1 u2 .. un | z1 z2 .. zk | d
#result[i] :  
#max_d

calculateMaxDistance = function (dmuData, subjectDmuIdx, transformToUtilites=TRUE) {
  if(transformToUtilites == TRUE) {
    dmuData <- transformToUtilityValues(dmuData)
  }
  modelMax <- createProblemModel(subjectDmuIdx, dmuData)
  result <- get.variables(modelMax)[ncol(modelMax)]
  rm(modelMax)
  return (result)
}

createProblemModel = function (subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  variablesCount = dmuCount +  2 * (dmuData$inputCount + dmuData$outputCount) + 1;
  lprec <- make.lp(0, variablesCount)
  createProblemObjective(lprec, subjectDmuIdx, dmuData)
  createConstraints(lprec, subjectDmuIdx, dmuData)
  setVariablesTypes(lprec, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  variablesCount = dmuCount +  2 * (dmuData$inputCount + dmuData$outputCount) + 1;
  objective <- array(0, dim=variablesCount)
  objective[variablesCount] <- -1
 # objective[1:(2 * (dmuData$inputCount + dmuData$outputCount))] <- 0
  
  set.objfn(model, objective)
  set.bounds(model, upper=1, lower = -Inf, columns = variablesCount)
}

createConstraints = function(model, subjectDmuIdx, dmuData) {
  varCount <- nrow(dmuData$data) + 2 * (dmuData$inputCount + dmuData$outputCount) + 1
  createSubjectConstraints(model, subjectDmuIdx, dmuData)
  createOtherConstraints(model, subjectDmuIdx, dmuData)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createCustomWeightConstraints(model, dmuData, varCount)
  }
}


#subject DMU's constraints- worstRank
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  CONST <- 100
  dmuCount <- nrow(dmuData$data)
  weightsCount <- (dmuData$inputCount + dmuData$outputCount) 
  varCount <- dmuCount + 2 * weightsCount + 1
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1 : constrCount) {
    #if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if(i == j - 2 * weightsCount) {
          result[i,j] <- -CONST
        } else if (j <= weightsCount) {
          result[i, j] <- dmuData$data[subjectDmuIdx, j]
        } else if (j <= 2 * weightsCount) {
          result[i, j] <- -dmuData$data[i, (j - weightsCount)]
        } else if (j == varCount){
          result[i, j] <- 1 
        } 
      } 
      add.constraint(model, result[i,], "<=", 0)
    }
  #}   
}


#wagi "sztucznego" DMU musza byc takie same jak wagi DMU obliczanego, suma wag = 1
createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  data <- dmuData$data
  inputCount <- dmuData$inputCount
  outputCount <- dmuData$outputCount
  dmuCount = nrow(data)
  weightsCount = inputCount + outputCount
  varCount <- 2 * (weightsCount) + dmuCount + 1
  constrCount <- weightsCount + 2
  result <- array(0, dim = c(constrCount,varCount))
  sign <- "="
  for (i in 1:(constrCount-2)) {
    result[i,i] <- -data[subjectDmuIdx, i]
    result[i, i + weightsCount] <- data[subjectDmuIdx, i]
    add.constraint(model, result[i,], sign, 0)
  }
  for(i in 1:weightsCount) {
    result[constrCount - 1, i] <- 1
  }
  add.constraint(model, result[constrCount-1,], sign, 1)
  
  for(i in 1:dmuCount) {
    result[constrCount, i + 2 * weightsCount] <- 1
  }
  add.constraint(model, result[constrCount,], "<=", dmuCount-1)
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




