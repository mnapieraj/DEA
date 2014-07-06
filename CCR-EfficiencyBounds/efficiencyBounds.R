#problem
# v1 v2 .. vm | u1 u2 .. un
#result : [1] leftOverLeastEfficient, [2] rightOverLeastEfficient, 
# [3] leftOverMostEfficient, [4] rightOverMostEff, [5]superEfficiency

calculateEfficiencyBoundsForAll = function (dmuData, relativeDmus) {
  result <- array(0, dim=c(nrow(dmuData$data), 5))
  for(i in 1:nrow(dmuData$data)) {
    result[i,] <- calculateEfficiencyBounds(dmuData, i, relativeDmus)
  }
  return(result)
}

calculateEfficiencyBounds = function (dmuData, subjectDmuIdx, relativeDmus) {
  minDom <- 1000
  maxDom <- 0
  result <- array(0, dim=5)
  source("efficiencyDominance.R")
  for(i in 1:nrow(dmuData$data)) {
    if(i != subjectDmuIdx) {
    dominanceResult <- calculateDominance(dmuData, subjectDmuIdx, i)  
    minDom <- min(dominanceResult[2], minDom)
    maxDom <- max(dominanceResult[3], maxDom)
    }
  }
  source("efficiencyBounds.R")
  modelMinOverLeast <- createProblemModel("minEffOverLeastEff", subjectDmuIdx, dmuData, relativeDmus)
  modelMaxOverMost <- createProblemModel("maxEffOverMostEff", subjectDmuIdx, dmuData, relativeDmus)

  result[1] <- as.numeric(minDom)
  result[2] <- as.numeric(-get.objective(modelMaxOverMost))
  result[3] <- as.numeric(get.objective(modelMinOverLeast))
  result[4] <- as.numeric(maxDom)
  result[5] <- as.numeric(result[2])
  
  rm(modelMinOverLeast)
  rm(modelMaxOverMost)
  
  return (result)
}

createProblemModel = function (problemName, subjectDmuIdx, dmuData, relativeDmus) {
  dmuCount = nrow(dmuData$data)
  variablesCount = dmuData$inputCount + dmuData$outputCount;
  lprec <- make.lp(0, variablesCount)
  
  createProblemObjective(problemName, lprec, subjectDmuIdx, dmuData)
  createConstraints(problemName, lprec, subjectDmuIdx, dmuData, relativeDmus)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(problemName, model, subjectDmuIdx, dmuData) {
  if (problemName == "maxEffOverMostEff") {
    sign = -1
  }
  else if(problemName == "minEffOverLeastEff") {
    sign = 1
  }
  variablesCount = dmuData$inputCount + dmuData$outputCount 
  objective <- array(0, dim=variablesCount)
  for (i in (dmuData$inputCount + 1):variablesCount) {
    objective [i] <- sign * dmuData$data[subjectDmuIdx, i]
  }
  set.objfn(model, objective)
}

createConstraints = function(problemName, model, subjectDmuIdx, dmuData, relativeDmus) {
  createSubjectConstraints(model, subjectDmuIdx, dmuData)
  createOtherConstraints(problemName, model, subjectDmuIdx, dmuData, relativeDmus)
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

createOtherConstraints = function(problemName, model, subjectDmuIdx, dmuData, relativeDmus) {
  dmuCount = nrow(dmuData$data)
  varCount <- dmuData$inputCount + dmuData$outputCount
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  if(!is.null(dmuData$includeSubject) && dmuData$includeSubject == TRUE) {
	subjectDmuIdx <- -1
  }
  for (i in relativeDmus) {
    if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if (j <= dmuData$inputCount) {
          result[i, j] <- -dmuData$data[i, j]
        } else if (j <= dmuData$inputCount + dmuData$outputCount) {
          result[i, j] <- dmuData$data[i, j]
        } 
      } 
      if(problemName == "maxEffOverMostEff") {
        sign = "<="
      }
      else if(problemName == "minEffOverLeastEff") {
        sign = ">="
      }
      add.constraint(model, result[i,], sign, 0)
    }
  }   
}
