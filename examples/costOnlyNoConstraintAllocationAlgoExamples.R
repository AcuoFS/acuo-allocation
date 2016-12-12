library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc1','mc2','mc3','mc5'); clientId1 <- 'c1'
callId2 <- c('mc1','mc2','mc3','mc4','mc5','mc8','mc9','mc10') ; clientId2 <- 'c1'
callId3 <- c('mc2','mc3','mc8','mc12','mc16') ; clientId3 <- 'c1'

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
