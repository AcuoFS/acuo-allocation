library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc41','mc42','mc43','mc45'); clientId1 <- '999'
callId2 <- c('mc43','mc44','mc45','mc48','mc54','mc59') ; clientId2 <- '999'
callId3 <- c('mc49','mc50','mc51','mc54','mc55','mc57','mc60') ; clientId3 <- '999'
callId4 <- c('mc51','mc52','mc54','mc55','mc57','mc59','mc61') ; clientId4 <- '999'

modifyAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetQuantityToSimulateCostOnlyQuantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetQuantityDueToSimulateCostOnlyQuantityLimitAllocationAlgo.load'

modifyAssetQuantityCypherPath2 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify2AssetQuantityToSimulateCostOnlyQuantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath2 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore2AssetQuantityDueToSimulateCostOnlyQuantityLimitAllocationAlgo.load'

modifyAssetInternalCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetInternalCostToTestAllocationAlgo.load'
restoreAssetInternalCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetInternalCostToTestAllocationAlgo.load'

modifyAssetExternalCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetExternalCostToTestAllocationAlgo.load'
restoreAssetExternalCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetExternalCostToTestAllocationAlgo.load'

#### EXAMPLE FUNCTIONS ##################
costOnlyQuantityLimitAllocationAlgoEx1 <- function(){
  
  #executeCypher(path=modifyAssetQuantityCypherPath1)
  #executeCypher(path=modifyAssetInternalCostCypherPath1)
  #executeCypher(path=modifyAssetExternalCostCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=c(0,0,1))
  
  #executeCypher(path=restoreAssetQuantityCypherPath1)
  #executeCypher(path=restoreAssetInternalCostCypherPath1)
  #executeCypher(path=restoreAssetExternalCostCypherPath1)
  
  return(allocation.result)
}

costOnlyQuantityLimitAllocationAlgoEx2 <- function(){
  
  #executeCypher(path=modifyAssetQuantityCypherPath1)
  #executeCypher(path=modifyAssetInternalCostCypherPath1)
  #executeCypher(path=modifyAssetExternalCostCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=c(0,0,1))
  
  #executeCypher(path=restoreAssetQuantityCypherPath1)
  #executeCypher(path=restoreAssetInternalCostCypherPath1)
  #executeCypher(path=restoreAssetExternalCostCypherPath1)
  
  return(allocation.result)
}

costOnlyQuantityLimitAllocationAlgoEx3 <- function(){
  
  #executeCypher(path=modifyAssetQuantityCypherPath2)
  #executeCypher(path=modifyAssetInternalCostCypherPath1)
  #executeCypher(path=modifyAssetExternalCostCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId3,clientId=clientId3,pref=c(0,0,1))
  
  #executeCypher(path=restoreAssetQuantityCypherPath2)
  #executeCypher(path=restoreAssetInternalCostCypherPath1)
  #executeCypher(path=restoreAssetExternalCostCypherPath1)
  
  return(allocation.result)
}

costOnlyQuantityLimitAllocationAlgoEx4 <- function(){
  
  #executeCypher(path=modifyAssetQuantityCypherPath2)
  #executeCypher(path=modifyAssetInternalCostCypherPath1)
  #executeCypher(path=modifyAssetExternalCostCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=c(0,0,1))
  
  #executeCypher(path=restoreAssetQuantityCypherPath2)
  #executeCypher(path=restoreAssetInternalCostCypherPath1)
  #executeCypher(path=restoreAssetExternalCostCypherPath1)
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
costOnlyQuantityLimitAllocationAlgoEx1()
costOnlyQuantityLimitAllocationAlgoEx2()
costOnlyQuantityLimitAllocationAlgoEx3()
costOnlyQuantityLimitAllocationAlgoEx4()


