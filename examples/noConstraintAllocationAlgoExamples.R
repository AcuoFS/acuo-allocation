library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc1','mc2','mc3','mc5'); clientId1 <- '999'; pref1 <- c(5,5,0)
callId2 <- c('mc1','mc2','mc3','mc4','mc5','mc8','mc9','mc10') ; clientId2 <- '999'; pref2<- c(3,3,3)
callId3 <- c('mc4','mc8','mc12','mc13','mc16') ; clientId3 <- '999'; pref3<-c(2,5,8)

modifyDBToSimulateNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modifyAssetQuantityToSimulateOperationOnlyNoConstraintAllocationAlgo.load'
restoreDBDueToSimulateNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restoreAssetQuantityDueToSimulateOperationOnlyNoConstraintAllocationAlgo.load'


#### EXAMPLE FUNCTIONS ##################
noConstraintAllocationAlgoEx1 <- function(){
  
  executeCypher(path=modifyDBToSimulateNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=pref1)
  
  executeCypher(path=restoreDBDueToSimulateNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}
noConstraintAllocationAlgoEx2 <- function(){
  
  executeCypher(path=modifyDBToSimulateNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=pref2)
  
  executeCypher(path=restoreDBDueToSimulateNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}
noConstraintAllocationAlgoEx3 <- function(){
  
  executeCypher(path=modifyDBToSimulateNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId3,clientId=clientId3,pref=pref3)
  
  executeCypher(path=restoreDBDueToSimulateNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
noConstraintAllocationAlgoEx1()
noConstraintAllocationAlgoEx2()
noConstraintAllocationAlgoEx3()
