#problem :
# v1 v2 ..vm | u1 u2 .. un | z1 z2 .. zk
#result[i] :  
#minRank_i, maxRank_i

calculateRank = function (dmuData) {
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
  variablesCount = dmuCount +  dmuData$inputCount + dmuData$outputCount;
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
  restOfVariables <- array(0, dim=dmuData$inputCount + dmuData$outputCount)
  objective <- append(restOfVariables, objectiveVariables)
  set.objfn(model, objective)
}

createConstraints = function(problemName, model, subjectDmuIdx, dmuData) {
  varCount <- nrow(dmuData$data) + dmuData$inputCount + dmuData$outputCount
   createSubjectConstraints(model, subjectDmuIdx, dmuData)
   if(problemName == "minRank"){
      source("minRank.R")
      createOtherConstraints(model, subjectDmuIdx, dmuData)
   }
   else {
     source("maxRank.R")
     createOtherConstraints(model, subjectDmuIdx, dmuData)
   }
   if(dmuData$withWeightConstraints == TRUE) {
     source("weightConstraints.R")
     createCustomWeightConstraints(model, dmuData, varCount)
   }
}

#subject DMU constraints
createSubjectConstraints = function(model, subjectDmuIdx, dmuData) {
  CONST <- 1000000
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuCount + dmuData$inputCount + dmuData$outputCount
  constrCount <- 2 #one for inputs, one for outputs
  result <- array(0, dim = c(constrCount,varCount))
  
  for (i in 1 : constrCount) {
    for(j in 1 : (varCount - dmuCount)) {
        if((i == 1 && j <= dmuData$inputCount) ||
             (i == 2 && j > dmuData$inputCount && j <= dmuData$inputCount + dmuData$outputCount)){
          result[i, j] <- dmuData$data[subjectDmuIdx, j]
        } 
    }
    add.constraint(model, result[i,], "=", 1)
  }
}



setVariablesTypes = function (model, dmuData) {
  dmuCount <- nrow(dmuData$data)
  varCount <- dmuCount + dmuData$inputCount + dmuData$outputCount
  set.type(model, columns = (dmuData$inputCount + dmuData$outputCount + 1):varCount, type="binary")
}
