library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc1','mc2','mc3','mc5'); clientId1 <- 'c1'
callId2 <- c('mc1','mc2','mc3','mc4','mc5','mc8','mc9','mc10') ; clientId2 <- 'c1'
callId3 <- c('mc4','mc8','mc12','mc13','mc16') ; clientId3 <- 'c1'

modifyDBToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modifyAssetQuantityToSimulateLiquidityOnlyNoConstraintAllocationAlgo.load'
restoreDBDueToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restoreAssetQuantityDueToSimulateLiquidityOnlyNoConstraintAllocationAlgo.load'


#### EXAMPLE FUNCTIONS ##################
liquidityOnlyNoConstraintAllocationAlgoEx1 <- function(){
  
  executeCypher(path=modifyDBToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=c(0,1,0))
  
  executeCypher(path=restoreDBDueToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}
liquidityOnlyNoConstraintAllocationAlgoEx2 <- function(){
  
  executeCypher(path=modifyDBToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=c(0,1,0))
  
  executeCypher(path=restoreDBDueToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}
liquidityOnlyNoConstraintAllocationAlgoEx3 <- function(){
  
  executeCypher(path=modifyDBToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath)
  
  allocation.result <- allocationAlgo(callId=callId3,clientId=clientId3,pref=c(0,1,0))
  
  executeCypher(path=restoreDBDueToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath)
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
liquidityOnlyNoConstraintAllocationAlgoEx1()
liquidityOnlyNoConstraintAllocationAlgoEx2()
liquidityOnlyNoConstraintAllocationAlgoEx3()
