library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc41','mc42','mc43','mc45'); clientId1 <- '999'
callId2 <- c('mc43','mc44','mc45','mc48','mc54','mc59') ; clientId2 <- '999'
callId3 <- c('mc49','mc50','mc51','mc54','mc55','mc57','mc60') ; clientId3 <- '999'

modifyDBToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modifyAssetQuantityToSimulateCostOnlyNoConstraintAllocationAlgo.load'
restoreDBDueToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restoreAssetQuantityDueToSimulateCostOnlyNoConstraintAllocationAlgo.load'

#### EXAMPLE FUNCTIONS ##################
costOnlyNoConstraintAllocationAlgoEx1 <- function(){

  executeCypher(path=modifyDBToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=c(0,0,1))
  
  executeCypher(path=restoreDBDueToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}
costOnlyNoConstraintAllocationAlgoEx2 <- function(){
  
  executeCypher(path=modifyDBToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=c(0,0,1))
  
  executeCypher(path=restoreDBDueToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}
costOnlyNoConstraintAllocationAlgoEx3 <- function(){
  
  executeCypher(path=modifyDBToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId3,clientId=clientId3,pref=c(0,0,1))
  
  executeCypher(path=restoreDBDueToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
costOnlyNoConstraintAllocationAlgoEx1()
costOnlyNoConstraintAllocationAlgoEx2()
costOnlyNoConstraintAllocationAlgoEx3()
