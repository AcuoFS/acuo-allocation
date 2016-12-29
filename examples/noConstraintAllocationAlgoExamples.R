library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc1','mc2','mc3','mc5'); clientId1 <- '999'; pref1 <- c(5,5,0)
callId2 <- c('mc1','mc2','mc3','mc5'); clientId2 <- '999'; pref2 <- c(2,5,9)

callId3 <- c('mc1','mc2','mc3','mc4','mc5','mc8','mc9','mc10') ; clientId3 <- '999'; pref3<- c(3,3,3)
callId4 <- c('mc1','mc2','mc3','mc4','mc5','mc8','mc9','mc10') ; clientId4 <- '999'; pref4<- c(3,8,5)

callId5 <- c('mc4','mc8','mc12','mc13','mc16') ; clientId5 <- '999'; pref5<-c(2,5,8)
callId6 <- c('mc4','mc8','mc12','mc13','mc16') ; clientId6 <- '999'; pref6<-c(7,4,5)



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
noConstraintAllocationAlgoEx4 <- function(){
  
  executeCypher(path=modifyDBToSimulateNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=pref4)
  
  executeCypher(path=restoreDBDueToSimulateNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}
noConstraintAllocationAlgoEx5 <- function(){
  
  executeCypher(path=modifyDBToSimulateNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId5,clientId=clientId5,pref=pref5)
  
  executeCypher(path=restoreDBDueToSimulateNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}
noConstraintAllocationAlgoEx6 <- function(){
  
  executeCypher(path=modifyDBToSimulateNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId6,clientId=clientId6,pref=pref6)
  
  executeCypher(path=restoreDBDueToSimulateNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
noConstraintAllocationAlgoEx1()$output
noConstraintAllocationAlgoEx2()$output
noConstraintAllocationAlgoEx3()$output
