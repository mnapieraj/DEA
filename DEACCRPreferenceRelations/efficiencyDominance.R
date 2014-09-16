#problem
# v1 v2 .. vm | u1 u2 .. un

#result[i] :
#dominanceMatrix : N(necesssary)/P(possible)/0(not dominating) minDom: min_dominance, maxDom : max_dominance

calculateDominance = function(dmuData, subjectDmu, relativeDmu) {
  result <- array(0, dim=3)
  modelMin <-createProblemModel("minEff", subjectDmu, dmuData, relativeDmu)
  modelMax <- createProblemModel("maxEff", subjectDmu, dmuData, relativeDmu)
 # print(modelMin)
#  print(modelMax)
  result[2] <- get.objective(modelMin)
  result[3] <- -get.objective(modelMax)
  if(result[2] >= 0.999) {
    result[1] <- 'N'
  } else if(result[3] >= 0.999){
    result[1] <- 'P'
  } else {
    result[1] <- 0
  }
  rm(modelMin)
  rm(modelMax)
  return (result)
}

calculateDominanceForAll = function (dmuData) {
  dmuCount <- nrow(dmuData$data)
  result <- c()
  result$dominanceMatrix <- array(0, dim=c(dmuCount, dmuCount))
  result$minDom <- array(100, dim=c(dmuCount))
  result$maxDom <- array(0, dim=c(dmuCount))
  for(i in 1:dmuCount) {
    for(j in 1:dmuCount) {
      tempResult <- calculateDominance(dmuData, i, j)
      result$dominanceMatrix[i,j] <- tempResult[1]
      if(i != j ) {
      result$minDom[i] <- min(result$minDom[i], tempResult[2])
      result$maxDom[i] <- max(result$maxDom[i], tempResult[3])        
      }
    }
  }
  return (result)
}

createProblemModel = function (problemName, subjectDmuIdx, dmuData, relativeDmu) {
  dmuCount = nrow(dmuData$data)
  variablesCount = dmuData$inputCount + dmuData$outputCount;
  lprec <- make.lp(0, variablesCount)
  
  createProblemObjective(problemName, lprec, subjectDmuIdx,dmuData)
  createConstraints(problemName, lprec, subjectDmuIdx, dmuData, relativeDmu)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(problemName, model, subjectDmuIdx, dmuData) {
  if (problemName == "maxEff") {
    sign = -1
  }
  else if(problemName == "minEff") {
    sign = 1
  }
  variablesCount = dmuData$inputCount + dmuData$outputCount 
  objective <- array(0, dim=variablesCount)
  for (i in (dmuData$inputCount + 1):variablesCount) {
    objective [i] <- sign * dmuData$data[subjectDmuIdx, i]
  }
  set.objfn(model, objective)
}

createConstraints = function(problemName, model, subjectDmuIdx, dmuData, relativeDmu) {
  createSubjectConstraints(model, subjectDmuIdx, dmuData)
  createOtherConstraints(problemName, model, subjectDmuIdx, dmuData, relativeDmu)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createWeightConstraints(model, dmuData)
  }
}

#subject DMU constraints - sum of inputs has to be equal 1
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  varCount <-  dmuData$inputCount + dmuData$outputCount
  result <- array(0, dim = varCount)
  
  for (i in 1 : dmuData$inputCount){
    result[i] <- dmuData$data[subjectDmuIdx, i]  
  }
  add.constraint(model, result, "=", 1)
}

createOtherConstraints = function(problemName, model, subjectDmuIdx, dmuData, relativeDmu) {
  dmuCount = nrow(dmuData$data)
  varCount <- dmuData$inputCount + dmuData$outputCount
  constrCount <- 1
  result <- array(0, dim = c(constrCount,varCount))
  
  for(j in 1 : varCount) {
    if (j <= dmuData$inputCount) {
      result[j] <- -dmuData$data[relativeDmu, j]
    } else if (j <= dmuData$inputCount + dmuData$outputCount) {
      result[j] <- dmuData$data[relativeDmu, j]
    } 
  } 
  sign <- "="
  add.constraint(model, result, sign, 0)
  
}
