library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc1','mc2','mc3','mc5'); clientId1 <- '999'
callId2 <- c('mc1','mc2','mc3','mc4','mc5','mc8') ; clientId2 <- '999'
callId3 <- c('mc4','mc8','mc12','mc13','mc16') ; clientId3 <- '999'
callId4 <- c('mc2','mc9','mc10','mc11','mc14','mc15','mc17','mc19','mc20') ; clientId4 <- '999'

modifyAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetQuantityToSimulateOperationOnlyQuantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetQuantityDueToSimulateOperationOnlyQuantityLimitAllocationAlgo.load'

modifyAssetQuantityCypherPath2 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify2AssetQuantityToSimulateOperationOnlyQuantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath2 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore2AssetQuantityDueToSimulateOperationOnlyQuantityLimitAllocationAlgo.load'

#### EXAMPLE FUNCTIONS ##################
operationOnlyQuantityLimitAllocationAlgoEx1 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=c(1,0,0))
  
  return(allocation.result)
}
operationOnlyQuantityLimitAllocationAlgoEx2 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=c(1,0,0))
  
  return(allocation.result)
}
operationOnlyQuantityLimitAllocationAlgoEx3 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId3,clientId=clientId3,pref=c(1,0,0))
  
  return(allocation.result)
}
operationOnlyQuantityLimitAllocationAlgoEx4 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=c(1,0,0))
  
  executeCypher(path=restoreAssetQuantityCypherPath1)
  
  return(allocation.result)
}
operationOnlyQuantityLimitAllocationAlgoEx5 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=c(1,0,0))
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
operationOnlyQuantityLimitAllocationAlgoEx1()
operationOnlyQuantityLimitAllocationAlgoEx2()
operationOnlyQuantityLimitAllocationAlgoEx3()
operationOnlyQuantityLimitAllocationAlgoEx4()
operationOnlyQuantityLimitAllocationAlgoEx5()


