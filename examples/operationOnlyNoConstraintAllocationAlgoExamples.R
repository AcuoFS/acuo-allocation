library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc1','mc2','mc3','mc5'); clientId1 <- 'c1'
callId2 <- c('mc1','mc2','mc3','mc4','mc5','mc8','mc9','mc10') ; clientId2 <- 'c1'
callId3 <- c('mc4','mc8','mc12','mc13','mc16') ; clientId3 <- 'c1'

modifyDBToSimulateOperationOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modifyAssetQuantityToSimulateOperationOnlyNoConstraintAllocationAlgo.load'
restoreDBDueToSimulateOperationOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restoreAssetQuantityDueToSimulateOperationOnlyNoConstraintAllocationAlgo.load'


#### EXAMPLE FUNCTIONS ##################
operationOnlyNoConstraintAllocationAlgoEx1 <- function(){
  
  executeCypher(path=modifyDBToSimulateOperationOnlyNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=c(1,0,0))
  
  executeCypher(path=restoreDBDueToSimulateOperationOnlyNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}
operationOnlyNoConstraintAllocationAlgoEx2 <- function(){
  
  executeCypher(path=modifyDBToSimulateOperationOnlyNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=c(1,0,0))
  
  executeCypher(path=restoreDBDueToSimulateOperationOnlyNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}
operationOnlyNoConstraintAllocationAlgoEx3 <- function(){
  
  executeCypher(path=modifyDBToSimulateOperationOnlyNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId3,clientId=clientId3,pref=c(1,0,0))
  
  executeCypher(path=restoreDBDueToSimulateOperationOnlyNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
operationOnlyNoConstraintAllocationAlgoEx1()
operationOnlyNoConstraintAllocationAlgoEx2()
operationOnlyNoConstraintAllocationAlgoEx3()
