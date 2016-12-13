library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc1','mc2','mc3','mc5'); clientId1 <- 'c1'
callId2 <- c('mc1','mc2','mc3','mc4','mc5','mc8') ; clientId2 <- 'c1'
callId3 <- c('mc4','mc8','mc12','mc13','mc16') ; clientId3 <- 'c1'
callId4 <- c('mc2','mc9','mc10','mc11','mc14','mc15','mc17','mc19','mc20') ; clientId4 <- 'c1'

modifyAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetQuantityToSimulateLiquidityOnlyQuantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetQuantityDueToSimulateLiquidityOnlyQuantityLimitAllocationAlgo.load'

modifyAssetQuantityCypherPath2 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify2AssetQuantityToSimulateLiquidityOnlyQuantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath2 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore2AssetQuantityDueToSimulateLiquidityOnlyQuantityLimitAllocationAlgo.load'

#### EXAMPLE FUNCTIONS ##################
liquidityOnlyQuantityLimitAllocationAlgoEx1 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=c(0,1,0))
  
  executeCypher(path=restoreAssetQuantityCypherPath1)
  
  return(allocation.result)
}
liquidityOnlyQuantityLimitAllocationAlgoEx2 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=c(0,1,0))
  
  executeCypher(path=restoreAssetQuantityCypherPath1)
  
  return(allocation.result)
}
liquidityOnlyQuantityLimitAllocationAlgoEx3 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath2)
  
  allocation.result <- allocationAlgo(callId=callId3,clientId=clientId3,pref=c(0,1,0))
  
  executeCypher(path=restoreAssetQuantityCypherPath2)
  
  return(allocation.result)
}
liquidityOnlyQuantityLimitAllocationAlgoEx4 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath2)
  
  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=c(0,0,1))
  
  executeCypher(path=restoreAssetQuantityCypherPath2)
  
  return(allocation.result)
}
liquidityOnlyQuantityLimitAllocationAlgoEx5 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=c(0,1,0))
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
liquidityOnlyQuantityLimitAllocationAlgoEx1()
liquidityOnlyQuantityLimitAllocationAlgoEx2()
liquidityOnlyQuantityLimitAllocationAlgoEx3()
liquidityOnlyQuantityLimitAllocationAlgoEx4()
liquidityOnlyQuantityLimitAllocationAlgoEx5()

