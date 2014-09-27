createWeightsSamplesForSMAA <- function (dmuData, samplesNo, vdea=FALSE) {
  baseCons <- createBaseConstraints(dmuData, vdea)
  colnames(baseCons) <- colnames(dmuData$weightConstraints)
  if(dmuData$withWeightConstraints == TRUE) {
	dmuData$weightConstraints <- rbind(dmuData$weightConstraints, baseCons)
  } else {
	dmuData$weightConstraints <- baseCons
  }
  weightsCons <- parseWeights(dmuData)
  samples <- createWeightsSamples(weightsCons, samplesNo)
  return (samples)
}
#normalize inputs and outputs, all crit >= 0
createBaseConstraints <- function (dmuData, vdea=FALSE) {
  additionalCons <- 2
  if(vdea == TRUE) {
    additionalCons <- 1
  }
  constrCount <- dmuData$inputCount + dmuData$outputCount + additionalCons
  varCount <- dmuData$inputCount + dmuData$outputCount + 2
  weights <- dmuData$weightConstraints
  normWeights <- array(0, dim=c(constrCount,varCount))
  normWeights[,varCount - 1] <- "<="
  for(i in 1:(constrCount - additionalCons)) {
    normWeights[i + additionalCons, i] <- -1
  }
  if(vdea == TRUE) {
    normWeights[1,1:(varCount - 2)] <- 1
    normWeights[1,varCount - 1] <- "="
    normWeights[1,varCount] <- 1
  } else {
    normWeights[1,1:dmuData$inputCount] <- 1
    normWeights[2,(dmuData$inputCount + 1):(dmuData$inputCount + dmuData$outputCount)] <- 1
    normWeights[1:2,varCount - 1] <- "="
    normWeights[1:2,varCount] <- 1
  }
  
  return (normWeights)
}

#parse to har format
parseWeights <- function (dmuData) {
  varCount <- dmuData$inputCount + dmuData$outputCount
  weights <- dmuData$weightConstraints
  constrCount <- nrow(weights)
  constr <- c()
  dir <- c()
  rhs <- c()
  for(i in 1:constrCount) {
    currConstr <- c()
    for(j in 1:varCount) {
      currConstr <- c(currConstr, as.numeric(weights[i,j]))
    }
    rownames(currConstr) <- NULL
    constr <- rbind(constr, currConstr)
    dir <- c(dir,  gsub("\\s","",as.character(weights[i,varCount + 1])))
    rhs <- c(rhs, as.numeric(weights[i, varCount + 2]))
  }
  rownames(constr) <- NULL
  constraints <- list(constr=constr, dir=dir, rhs=rhs)
  return (constraints)
}

createWeightsSamples <- function(constraints, samplesNo) {
  state <- har.init(constraints)
  result <- har.run(state, n.samples=samplesNo)
  samples <- result$samples
  return(samples)
}

parseSampleToWeightsConstraints <- function(sample) {
  varCount <- length(sample)
  weights <- list()
  operator <- '='
  for(i in 1:varCount) {
    weight <- array(0, dim=c(varCount))
    weight[i] <- 1
    rhs <- sample[i]
    weights <- rbind(weights, list(weights=weight, rhs=rhs, operator=operator))
  }
  
  return (weights)
}
