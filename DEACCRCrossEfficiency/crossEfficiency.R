#problem :
# v1 v2 ..vm | u1 u2 .. un 
#result[i] :
#v1 v2 ..vm | u1 u2 .. un

calculateCrossEfficiencyForAll = function (type, dmuData) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,1))
  for(i in 1:dmuCount) {
    result[i] <- calculateCrossEfficiency(type, dmuData, i)
  }
  
  return (result)
}

calculateCrossEfficiency = function (type, dmuData, subjectDmu) {
  source("efficiency.R")
  dmusEfficiency <- calculateEfficiencyForAll(dmuData)
  source("crossEfficiency.R")
  dmuCount = nrow(dmuData$data)
  result <- 0
  if(type == "aggressive") {
    sign <- 1
  } else {
    sign <- -1
  }
  for (i in 1:dmuCount) {
    if (i != subjectDmu) {
      modelEff <- createProblemModel(type, dmuData, i, dmusEfficiency[i])
      weights <- get.variables(modelEff)
      result <- result + calculateEfficiencyForWeights(dmuData, subjectDmu, weights)
      rm(modelEff)
    }
  }
  result = result / (dmuCount - 1)
  return (result)
}

calculateEfficiencyForWeights = function (dmuData, subjectDmuIdx, weights) {
  inputs <- 0
  outputs <- 0
  for(i in 1:(dmuData$inputCount + dmuData$outputCount)) {
    if(i <= dmuData$inputCount) {
      inputs <- inputs + weights[i] * dmuData$data[subjectDmuIdx, i]
    } else {
      outputs <- outputs + weights[i] * dmuData$data[subjectDmuIdx, i]
    }
  }
  return (outputs / inputs)
}

createProblemModel = function (problemName, dmuData, subjectDmuIdx, subjectDmuEfficiency) {
  variablesCount = dmuData$inputCount + dmuData$outputCount;
  lprec <- make.lp(0, variablesCount)
  createProblemObjective(problemName, lprec, subjectDmuIdx, dmuData)
  createConstraints(lprec, subjectDmuIdx, subjectDmuEfficiency, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(problemName, model, subjectDmuIdx, dmuData) {
  if(problemName == "aggressive") {
    sign <- 1
  } else {
    sign <- -1
  }
  
  dmuCount = nrow(dmuData$data) 
  varCount = dmuData$inputCount + dmuData$outputCount
  objective <-  array(0, dim=varCount)
  
  for(i in (dmuData$inputCount+1):(varCount)) {
    for(j in 1:dmuCount) {
      if(j != subjectDmuIdx) {
        objective[i] <- objective[i] + sign * dmuData$data[j, i]
      }
    }
  }
  set.objfn(model, objective)
}

createConstraints = function(model, subjectDmuIdx, subjectDmuEfficiency, dmuData) {
  createSubjectConstraints(model, subjectDmuIdx, dmuData)
  createOtherConstraints(model, subjectDmuIdx, subjectDmuEfficiency, dmuData)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createWeightConstraints(model, dmuData)
  }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  dmuCount <- nrow(dmuData$data)
  varCount <-  dmuData$inputCount + dmuData$outputCount
  result <- array(0, dim = varCount)
  
  for (i in 1 : dmuData$inputCount){
    for(j in 1:dmuCount) {
      if(j != subjectDmuIdx) {
        result[i] <- result[i] + dmuData$data[j, i]  
      }
    }
  }
  add.constraint(model, result, "=", 1)
}

createOtherConstraints = function(model, subjectDmuIdx, subjectDmuEfficiency, dmuData) {
  dmuCount = nrow(dmuData$data)
  varCount <- dmuData$inputCount + dmuData$outputCount
  constrCount <- dmuCount
  resultWithoutEff <- array(0, dim = c(constrCount,varCount))
  resultWithEff <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1:constrCount) {
    #if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if (j <= dmuData$inputCount) {
          resultWithoutEff[i, j] <- -dmuData$data[i, j]
          resultWithEff[i,j] <- -dmuData$data[i,j] * subjectDmuEfficiency
        } else if (j <= dmuData$inputCount + dmuData$outputCount) {
          resultWithoutEff[i, j] <- dmuData$data[i, j]
          resultWithEff[i, j] <- dmuData$data[i, j]
        } 
      } 
      if(i != subjectDmuIdx) {        
        add.constraint(model, resultWithoutEff[i,], "<=", 0)
      } else {
        add.constraint(model, resultWithEff[i,], "=", 0)      
      }
   # }
  }  
}
