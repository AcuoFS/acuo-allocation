library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mcp41','mcp42','mcp46','mcp47'); clientId1 <- '999'
callId2 <- c('mcp41','mcp42','mcp43','mcp44','mcp45','mcp48') ; clientId2 <- '999'
callId3 <- c('mcp44','mcp48','mcp52','mcp53') ; clientId3 <- '999'
callId4 <- c('mcp42','mcp49','mcp50','mcp51','mcp54','mcp55','mcp57','mcp59','mcp60') ; clientId4 <- '999'

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
  
  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=c(0,1,0))
  
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

