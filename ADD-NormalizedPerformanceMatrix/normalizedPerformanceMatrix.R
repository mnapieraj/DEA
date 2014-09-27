normalizePerformanceMatrix <- function(dmuData) {
	dmuData <- updateFunctions(dmuData)
	dmuData <- transformToUtilityValues(dmuData)
	print(dmuData)
	return(dmuData$data)
}

updateFunctions <- function(dmuData) {
  varCount <- dmuData$inputCount + dmuData$outputCount
  bounds <- dmuData$boundaries
  for(i in 1:varCount) {
	critName <- dmuData$critIDs[i]
	minPoint <- c(bounds$low[i], 0)
	maxPoint <- c(bounds$up[i], 1)
	if(i <= dmuData$inputCount) {
		minPoint <- c(bounds$up[i], 0)
		maxPoint <- c(bounds$low[i], 1)
	} 
	dmuData$functions[[critName]] <- rbind(dmuData$functions[[critName]], minPoint)
	dmuData$functions[[critName]] <- rbind(dmuData$functions[[critName]], maxPoint)
  }
	return (dmuData)
}


transformToUtilityValues = function(dmuData) {
  dmuCount <- nrow(dmuData$data)
  boundaries <- dmuData$boundaries
  inputs <- dmuData$inputCount
  outputs <- dmuData$outputCount
  for(i in 1:dmuCount) {
    for(j in 1:(inputs + outputs)) {
		critName <- dmuData$critIDs[j]
		if(!is.null(dmuData$functions[[critName]])) {
			dmuData <- computePerformance(dmuData,dmuData$functions[[critName]], i, j)
		} else {
			dmuData <- normalize(dmuData, i, j)
		}
    }
  }
  return (dmuData)
}

computePerformance <- function(dmuData, fun, i, j) {
	val <- dmuData$data[i,j]
	idxs <- which(fun[,1] == val)
	if(length(idxs) != 0) {
		dmuData$data[i,j] <- (fun[idxs[1], 2])
	} else {
		lowX <- max(fun[fun[,1] < val, 1])
		listUp <- fun[fun[,1] > val, 1]
		upX <- min(listUp) 
		yFromLowX <- which(fun[,1] == max(fun[fun[,1] < val,1]))[1]
		yFromUpX <- which(fun[,1] == min(fun[fun[,1] > val,1]))[1]
		yFromLowX <- fun[yFromLowX, 2]
		yFromUpX <- fun[yFromUpX,2]
		resultVal <- (val - lowX) / (upX - lowX) * (yFromUpX - yFromLowX) + yFromLowX
		dmuData$data[i,j] <- resultVal 		
	}
	return (dmuData)
}

normalize <- function(dmuData, i, j){
print("N")
  boundaries <- dmuData$boundaries
  inputs <- dmuData$inputCount
  outputs <- dmuData$outputCount
	if(j <= inputs) {
        dmuData$data[i,j] <- (boundaries$up[j] - dmuData$data[i,j]) / (boundaries$up[j] - boundaries$low[j])
	} else if (j <= inputs + outputs) {
		dmuData$data[i,j] <- (dmuData$data[i,j] - boundaries$low[j]) / (boundaries$up[j] - boundaries$low[j])
	}
	return (dmuData)
}