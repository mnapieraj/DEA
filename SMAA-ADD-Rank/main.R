#Usage : 
# Example (linux): 
# R --slave --vanilla --args "${PWD}/in" "${PWD}/out" < main.R 
# Example (windows): 
# R --slave --vanilla --args "%CD%/in" "%CD%/out" < main.R 

readFile <- function(fileName) {
  errMsg <- NULL
  tmpErr<-try(
{
  data<-xmlTreeParse(fileName, useInternalNodes=TRUE)
}
  )
  if (inherits(tmpErr, 'try-error')){
    errMsg<-paste("Cannot read file : ",fileName) 
  }
  result <- list(data = data, errMsg = errMsg)
  return(result)
}

readFiles <- function() {
  altData <- readFile("alternatives.xml")
  criteriaData <- readFile("criteria.xml")
  performanceData <- readFile("performanceTable.xml")
  #optional files
  methodParametersData <- readFile("methodParameters.xml")
  weightsConstraintsData <- readFile("criteriaLinearConstraints.xml")
  
  errMsg <- ""
  
  if(!is.null(altData$errMsg)) {
    errMsg <- paste(errMsg,altData$errMsg)
  }
  if(!is.null(criteriaData$errMsg)) {
    errMsg <- paste(errMsg,criteriaData$errMsg)
  }
  if(!is.null(performanceData$errMsg)) {
    errMsg <- paste(errMsg,performanceData$errMsg)
  }
  #optional files
  if(!is.null(weightsConstraintsData$errMsg)){
    weightsConstraintsData$data <- NULL
  }
  if(!is.null(methodParametersData$errMsg)){
    methodParametersData$data <- NULL
  }
  result <- list(altTree = altData$data, 
                 criteriaTree = criteriaData$data, 
                 performanceTree = performanceData$data,
                 weightsConstraintsTree = weightsConstraintsData$data,
                 methodParametersTree = methodParametersData$data,
                 errMsg = errMsg)
  
  return(result)
}

checkXSDValid <- function(dataTree) {
  err <- ""
  if (checkXSD(dataTree$altTree)==0) {
    err <- paste(err,"Alternatives file is not XMCDA valid.")  
  }
  if (checkXSD(dataTree$criteriaTree)==0) {
    err <- paste(err,"Criteria file is not XMCDA valid.")  
  }
  if (checkXSD(dataTree$performanceTree)==0) {
    err <- paste(err,"PerformanceTable file is not XMCDA valid.")  
  }  
  #optional file
  if (!is.null(dataTree$methodParametersTree) && checkXSD(dataTree$methodParametersTree)==0) {
    err <- paste(err,"Method parameters file is not XMCDA valid.")  
  }  
  
  return (err)
}

getValues <- function(tree, nodeName) {
  nodes <- getNodeSet(tree, paste("//", nodeName))
  count <- length(nodes)
  values <- array(dim=count)
  for(i in 1:count) {
    values[i] <- xmlValue(nodes[[i]][[1]])
  }
  return(values)
}

getWeightConstraints <- function(tree, critIDs) {
  consNodes <- getNodeSet(tree, "//constraint")
  consCount <- length(consNodes)
  varCount <- length(critIDs) 
  weightCons <- NULL
  for(i in 1:consCount) {
    weightCons <- rbind(weightCons,getWeightConstraint(consNodes[[i]], critIDs))
  }
  return (weightCons)
}

getWeightConstraint <- function(node, critIDs) {
  varCount <- length(critIDs)
  elementsNodes <- xmlElementsByTagName(node, "element")
  operator <- xmlValue(xmlElementsByTagName(node, "operator")[[1]])
  rhs <- xmlValue(getNodeSet(node, "rhs")[[1]][[1]])
  weightConstraint <- array(0,dim=varCount+2)  
  
  for(i in 1:length(elementsNodes)) {
    element <- elementsNodes[[i]]
    critID <- xmlValue(xmlElementsByTagName(element, "criterionID")[[1]])
    critIdx <- which(critIDs == critID) 
    value <- as.double(xmlValue(xmlElementsByTagName(element, "coefficient")[[1]]))
    weightConstraint[critIdx] <- value
  }
  if(operator == "eq") {
    operator <- "="
  }
  if(operator == "leq") {
    operator <- "<="
  }
  if(operator == "geq") {
    operator <- ">="
  }
  weightConstraint[varCount + 1] <- operator
  weightConstraint[varCount + 2] <- as.double(rhs)
 # weightConstraintData <- list(weights = weightConstraint, operator=operator, rhs=as.double(rhs))
  return (weightConstraint)
}

orderCriteriaByPreference <- function(criteriaIDs, preferenceDirs) {
  sortedPref <- sort(preferenceDirs, decreasing=TRUE, index.return = TRUE)
  orderedCriteriaIDs <- array(0, dim=length(criteriaIDs))
  inputCount <- 0
  outputCount <- 0
  for(i in 1:length(sortedPref$ix)) {
    if(sortedPref$x[i] == "min") {
      inputCount <- inputCount + 1
    } else {
      outputCount <- outputCount + 1
    }
    critIdx <- sortedPref$ix[i]
    orderedCriteriaIDs[i] <- criteriaIDs[critIdx]  
  }
  result <- list (critIDs = orderedCriteriaIDs, 
                  inputCount = inputCount, 
                  outputCount = outputCount)
  return (result)
}

#switch column order to match pattern -> inputs first, then outputs
orderPerformanceByCriteria <- function(performance, orderedCriteriaIDs) {
  orderedPerformance <- array(0, dim=c(nrow(performance), ncol(performance)))
  for(i in 1:length(orderedCriteriaIDs)) {
    orderedPerformance[,i] <- performance[,orderedCriteriaIDs[i]]
  }
  return (orderedPerformance)
}

parseTrees <- function (dataTree) {
  altIDs <- getAlternativesIDs(dataTree$altTree)[[1]]
  critIDs <- getCriteriaIDs(dataTree$criteriaTree)[[1]]
  preferenceDirs <- getValues(dataTree$criteriaTree, "preferenceDirection") 
    
  #inputs have to be first, then oputputs
  orderedCriteria <- orderCriteriaByPreference(critIDs, preferenceDirs)
  performance <- getPerformanceTables(dataTree$performanceTree, altIDs, orderedCriteria$critIDs)[[1]]
  performance <- orderPerformanceByCriteria(performance, orderedCriteria$critIDs)
  
  weightConstraints <- NULL
  if(!is.null(dataTree$weightsConstraintsTree)) {
    weightConstraints <- getWeightConstraints(dataTree$weightsConstraintsTree,
                                                orderedCriteria$critIDs)
  }
  
  withWeightConstraints <- FALSE
  samplesNo <- 100
  intervalsNo <- 10
  boundariesProvided <- FALSE 
  if(!is.null(dataTree$methodParametersTree)) {
    withConsNode <- getParameters(dataTree$methodParametersTree, "withConstraints")
    if (withConsNode$status == "OK" && withConsNode$withConstraints != 0) {
      withWeightConstraints <- TRUE
    }
	
	samplesNoNode <- getParameters(dataTree$methodParametersTree, "samplesNo")
    if (samplesNoNode$status == "OK" && samplesNoNode$samplesNo != 0) {
      samplesNo <- samplesNoNode$samplesNo
    }
	
	intervalsNode <- getParameters(dataTree$methodParametersTree, "intervalsNo")
    if (intervalsNode$status == "OK" && intervalsNode$intervalsNo != 0) {
      intervalsNo <- intervalsNode$intervalsNo
    }
	
	boundariesProvidedNode <- getParameters(dataTree$methodParametersTree, "boundariesProvided")
	if (boundariesProvidedNode$status == "OK" && boundariesProvidedNode$boundariesProvided != 0) {
      boundariesProvided <- TRUE
    } 
	
  }
  
  result <- list(data=performance, 
                 inputCount=orderedCriteria$inputCount,
                 outputCount=orderedCriteria$outputCount,
                 weightConstraints = weightConstraints,
                 withWeightConstraints = withWeightConstraints,
                 samplesNo = samplesNo,
				 intervalsNo = intervalsNo,
                 altIDs = altIDs)
				 
	if(boundariesProvided == TRUE) {
		low <- getValues(dataTree$criteriaTree, "minimum")
		colnames(low) <- unorderedColnames
		low <- orderByCriteria(low, orderedCriteria$critIDs)
		up <- getValues(dataTree$criteriaTree, "maximum") 
		colnames(up) <- unorderedColnames
		up <- orderByCriteria(up, orderedCriteria$critIDs)
		
		low <- as.numeric(unlist(low))
		up <- as.numeric(unlist(up))
		result$boundaries = list(up=up, low=low)
	} else {
		result$boundaries = createBoundaries(result)
	}
  return (result)

}

saveMessages <- function(msg, name, fileName) {
  msgTree = newXMLDoc()
  
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = 
                       "http://www.decision-deck.org/2009/XMCDA-2.0.0 
                     http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = 
                             "http://www.w3.org/2001/XMLSchema-instance", 
                           "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
             parent=msgTree)
  
 putLogMessage(msgTree, msg, name = name)
  
 saveXML(msgTree, file=paste(fileName,'.xml', sep=""))
}

saveResult <- function (altIDs, res, fileName) {
  cols <- ncol(res)
  colnames(res) <- c(paste("Rank", altIDs))
  rownames(res) <- (altIDs)
  setwd(outDirectory)
  resultTree <- newXMLDoc()
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
             parent=resultTree)
  putPerformanceTable(resultTree, res)
  saveXML(resultTree, file=paste(fileName,'.xml', sep=""))
}

createBoundaries <- function (dmuData) {
  eps <- 0.01
  varCount <- dmuData$inputCount + dmuData$outputCount
  up <- c()
  low <- c()
  for(i in 1:varCount) {
    up <- cbind(up, max(dmuData$data[,i]) + eps)
    low <- cbind(low, min(dmuData$data[,i]) - eps)
  }
  result <- list(up = up, low = low)
  return (result)
}
###########################

library(RXMCDA)
library(lpSolveAPI)
library(hitandrun)
library(ppls)

workingDirectory <- getwd()
inDirectory <- commandArgs()[5]
outDirectory <- commandArgs()[6]

setwd(inDirectory)

errMsg <- ""
dataTree <- readFiles()
#if obligatory files were loaded successfully, check if those files are valid
if(dataTree$errMsg == "") {
  errMsg <- checkXSDValid(dataTree)
  #if files are XSD valid, parse them
  if(errMsg == "") {
    dmuData <- parseTrees(dataTree)
    setwd(workingDirectory)
    source("rankSMAA-VDEA.R")
	result <- calculateRankSMAAForAll(dmuData, dmuData$samplesNo)
	#print(result)
    saveResult(dmuData$altIDs, result, "performanceTable")
    saveMessages("OK", "executionStatus", "messages")
  
  } else {
    setwd(outDirectory)
    saveMessages(errMsg, "Error", "messages")
  }
} else {
  setwd(outDirectory)
  saveMessages(dataTree$errMsg, "Error", "messages")
}

###########################



