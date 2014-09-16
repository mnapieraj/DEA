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
  altData <- readFile("units.xml")
  criteriaData <- readFile("inputsOutputs.xml")
  performanceData <- readFile("performanceTable.xml")
  #optional files
  methodParametersData <- readFile("methodParameters.xml")
  weightsConstraintsData <- readFile("weightsLinearConstraints.xml")
  
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
  weightCons <- list()
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
  weightConstraint <- array(0,dim=varCount)  
  
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
  weightConstraintData <- list(weights = weightConstraint, operator=operator, rhs=as.double(rhs))
  return (weightConstraintData)
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
  if(!is.null(dataTree$methodParametersTree)) {
    withConsNode <- getParameters(dataTree$methodParametersTree, "withConstraints")
    if (withConsNode$status == "OK" && withConsNode$withConstraints != 0) {
      withWeightConstraints <- TRUE
    }
  }
  
  includeSubject <- FALSE
  if(!is.null(dataTree$methodParametersTree)) {
    includeSubNode <- getParameters(dataTree$methodParametersTree, "includeSubject")
    if (includeSubNode$status == "OK" && includeSubNode$includeSubject != 0) {
      includeSubject <- TRUE
    }
  }
  
  type <- "aggressive"
  if(!is.null(dataTree$methodParametersTree)) {
    typeNode <- getParameters(dataTree$methodParametersTree, "type")
    if (typeNode$status == "OK" && typeNode$type == "benevolent") {
      type <- typeNode$type
    }
  }
  
  result <- list(data=performance, 
                 inputCount=orderedCriteria$inputCount,
                 outputCount=orderedCriteria$outputCount,
                 weightConstraints = weightConstraints,
                 withWeightConstraints = withWeightConstraints,
                 type = type,
                 altIDs = altIDs,
				 includeSubject = includeSubject)
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
  resultSize <- length(altIDs)
  result <- c()
  for(i in 1:(resultSize)) {
    result <- rbind(result, c(i, res[i]))
  }
  
  setwd(outDirectory)
  resultTree <- newXMLDoc()
  newXMLNode("xmcda:XMCDA", 
             attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
             suppressNamespaceWarning=TRUE, 
             namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
             parent=resultTree)
  putAlternativesValues(resultTree, result, altIDs, fileName)
  saveXML(resultTree, file=paste(fileName,'.xml', sep=""))
}


###########################

library(RXMCDA)
library(lpSolveAPI)

workingDirectory <- getwd()
inDirectory <- commandArgs()[5]
outDirectory <- commandArgs()[6]

setwd(inDirectory)

errMsg <- ""
dataTree <- readFiles()
#if obligatory files were loaded successfully, check if files are valid
if(dataTree$errMsg == "") {
  errMsg <- checkXSDValid(dataTree)
  #if files are XSD valid, parse them
  if(errMsg == "") {
    dmuData <- parseTrees(dataTree)
    setwd(workingDirectory)
    source("efficiencyBounds.R")
    effBounds <- calculateEfficiencyBoundsForAll(dmuData, array(1:nrow(dmuData$data)))
	#print(effBounds)
    saveResult(dmuData$altIDs, effBounds[,1], "minEfficiencyOverMostEfficient")
    saveResult(dmuData$altIDs, effBounds[,2], "maxEfficiencyOverMostEfficient")
    saveResult(dmuData$altIDs, effBounds[,3], "minEfficiencyOverLeastEfficient")
    saveResult(dmuData$altIDs, effBounds[,4], "maxEfficiencyOverLeastEfficient")
    #saveResult(dmuData$altIDs, effBounds[,5], "superEfficiency")
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



