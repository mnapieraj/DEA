#problem :
# v1 v2 ..vm | u1 u2 .. un u(n+1) 
#result[i] :
#efficiency_i

calculateEfficiency = function (dmuData, subjectDmu) {
  modelEff <- createProblemModel(subjectDmu, dmuData)
  result <- get.objective(modelEff)
  #print(modelEff)
  rm(modelEff)
  return (result)
}

calculateEfficiencyForAll = function (dmuData) {
  dmuCount = nrow(dmuData$data)
  result <- array(0, dim=c(dmuCount,1))
  
  for(i in 1:dmuCount) {
     result[i] <- calculateEfficiency(dmuData, i)
  }
  
  return (result)
}

createProblemModel = function (subjectDmuIdx, dmuData) {
  variablesCount = dmuData$inputCount + dmuData$outputCount + nrow(dmuData$data);
  lprec <- make.lp(0, variablesCount)
  createProblemObjective(lprec, subjectDmuIdx, dmuData)
  createConstraints(lprec, subjectDmuIdx, dmuData)
  setVariablesTypes(lprec, dmuData)
  solve(lprec)
  return (lprec)
}

createProblemObjective = function(model, subjectDmuIdx, dmuData) {
  sign = 1;
  varCount = dmuData$inputCount + dmuData$outputCount + nrow(dmuData$data)
  objective <-  array(0, dim=varCount)
  for(i in (dmuData$inputCount+1):(dmuData$inputCount + dmuData$outputCount)) {
    objective[i] <- sign * dmuData$data[subjectDmuIdx, i]
  }
  set.objfn(model, objective)
  #add.constraint(model, objective, ">=", -1)
}

createConstraints = function(model, subjectDmuIdx, dmuData) {
 varCount <-  dmuData$inputCount + dmuData$outputCount + nrow(dmuData$data)
  createSubjectConstraints(model, subjectDmuIdx, dmuData)
  createOtherConstraints(model, subjectDmuIdx, dmuData)
  if(dmuData$withWeightConstraints == TRUE) {
    source("weightConstraints.R")
    createCustomWeightConstraints(model, dmuData, varCount)
  }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  varCount <-  dmuData$inputCount + dmuData$outputCount + nrow(dmuData$data)
  result <- array(0, dim = varCount)
  
  for (i in 1 : dmuData$inputCount){
    result[i] <- dmuData$data[subjectDmuIdx, i]  
  }
  add.constraint(model, result, "=", 1)
}

createOtherConstraints = function(model, subjectDmuIdx, dmuData) {
  CONST <- 10000
  data <- dmuData$data
  dmuCount = nrow(data)
  inputCount = dmuData$inputCount
  outputCount = dmuData$outputCount
  varCount <- dmuCount + inputCount + outputCount
  constrCount <- dmuCount
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1:constrCount) {
   # if(i != subjectDmuIdx) {
      for(j in 1 : varCount) {
        if (j <= inputCount) {
          result[i, j] <- -data[i, j]
        } else if (j <= inputCount + outputCount) {
          result[i, j] <- data[i, j]
        } else if( j == i + inputCount + outputCount) {
			result[i,j] <- -CONST
		}
      } 
      
      sign = ">="
      add.constraint(model, result[i,], sign, -CONST)
    #}
  } 
  constr <- array(0, dim=c(1, varCount))
  for(i in (inputCount + outputCount + 1):varCount) {
	constr[1,i] <- 1
  }
  add.constraint(model, constr, sign, 1)
  
}

setVariablesTypes = function (model, dmuData) {
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuCount + dmuData$inputCount + dmuData$outputCount
  set.type(model, columns = (dmuData$inputCount + dmuData$outputCount + 1):varCount, type="binary")
}
