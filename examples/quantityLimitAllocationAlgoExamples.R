library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc41','mc42','mc43','mc45'); clientId1 <- '999'
callId2 <- c('mc41','mc42','mc43','mc44','mc45','mc48') ; clientId2 <- '999'
callId3 <- c('mc44','mc48','mc52','mc53','mc56') ; clientId3 <- '999'
callId4 <- c('mc42','mc9','mc50','mc51','mc54','mc55','mc57','mc59','mc60') ; clientId4 <- '999'

modifyAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetQuantityToSimulatequantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetQuantityDueToSimulatequantityLimitAllocationAlgo.load'

modifyAssetQuantityCypherPath2 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify2AssetQuantityToSimulatequantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath2 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore2AssetQuantityDueToSimulatequantityLimitAllocationAlgo.load'

#### EXAMPLE FUNCTIONS ##################
quantityLimitAllocationAlgoEx1 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=c(2,3,5))
  
  return(allocation.result)
}
quantityLimitAllocationAlgoEx2 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=c(8,5,4))
  
  return(allocation.result)
}
quantityLimitAllocationAlgoEx3 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId3,clientId=clientId3,pref=c(4,9,2))
  
  return(allocation.result)
}
quantityLimitAllocationAlgoEx4 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=c(9,1,7))
  
  executeCypher(path=restoreAssetQuantityCypherPath1)
  
  return(allocation.result)
}
quantityLimitAllocationAlgoEx5 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=c(10,1,0))
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
quantityLimitAllocationAlgoEx1()
quantityLimitAllocationAlgoEx2()
quantityLimitAllocationAlgoEx3()
quantityLimitAllocationAlgoEx4()
quantityLimitAllocationAlgoEx5()


