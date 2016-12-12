library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc1','mc2','mc3','mc5'); clientId1 <- 'c1'
callId2 <- c('mc1','mc2','mc3','mc4','mc5','mc8') ; clientId2 <- 'c1'
callId3 <- c('mc4','mc8','mc12','mc13','mc16') ; clientId3 <- 'c1'
callId4 <- c('mc2','mc3','mc8','mc12','mc13','mc16','mc17') ; clientId4 <- 'c1'

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
  
  executeCypher(path=modifyAssetQuantityCypherPath1)
  executeCypher(path=modifyAssetInternalCostCypherPath1)
  executeCypher(path=modifyAssetExternalCostCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=c(0,0,1))
  
  executeCypher(path=restoreAssetQuantityCypherPath1)
  executeCypher(path=restoreAssetInternalCostCypherPath1)
  executeCypher(path=restoreAssetExternalCostCypherPath1)
  
  return(allocation.result)
}

costOnlyQuantityLimitAllocationAlgoEx2 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath1)
  executeCypher(path=modifyAssetInternalCostCypherPath1)
  executeCypher(path=modifyAssetExternalCostCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=c(0,0,1))
  
  executeCypher(path=restoreAssetQuantityCypherPath1)
  executeCypher(path=restoreAssetInternalCostCypherPath1)
  executeCypher(path=restoreAssetExternalCostCypherPath1)
  
  return(allocation.result)
}

costOnlyQuantityLimitAllocationAlgoEx3 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath2)
  executeCypher(path=modifyAssetInternalCostCypherPath1)
  executeCypher(path=modifyAssetExternalCostCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId3,clientId=clientId3,pref=c(0,0,1))
  
  executeCypher(path=restoreAssetQuantityCypherPath2)
  executeCypher(path=restoreAssetInternalCostCypherPath1)
  executeCypher(path=restoreAssetExternalCostCypherPath1)
  
  return(allocation.result)
}

costOnlyQuantityLimitAllocationAlgoEx4 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath2)
  executeCypher(path=modifyAssetInternalCostCypherPath1)
  executeCypher(path=modifyAssetExternalCostCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=c(0,0,1))
  
  executeCypher(path=restoreAssetQuantityCypherPath2)
  executeCypher(path=restoreAssetInternalCostCypherPath1)
  executeCypher(path=restoreAssetExternalCostCypherPath1)
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
costOnlyQuantityLimitAllocationAlgoEx1()
costOnlyQuantityLimitAllocationAlgoEx2()
costOnlyQuantityLimitAllocationAlgoEx3()
costOnlyQuantityLimitAllocationAlgoEx4()


