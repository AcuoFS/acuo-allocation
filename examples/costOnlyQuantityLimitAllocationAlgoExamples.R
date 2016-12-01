library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mc1','mc2','mc3','mc5'); clientId1 <- 'c1'
callId2 <- c('mc1','mc2','mc3','mc4','mc5','mc6','mc7','mc8') ; clientId2 <- 'c1'

modifyAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetQuantityToSimulateCostOnlyQuantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetQuantityDueToSimulateCostOnlyQuantityLimitAllocationAlgo.load'

modifyAssetCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetCostToTestAllocationAlgo.load'
restoreAssetCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetCostToTestAllocationAlgo.load'


#### EXAMPLE FUNCTIONS ##################
costOnlyQuantityLimitAllocationAlgoEx1 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath1)
  executeCypher(path=modifyAssetCostCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=c(0,0,1,0))
  
  executeCypher(path=restoreAssetQuantityCypherPath1)
  executeCypher(path=restoreAssetCostCypherPath1)
  
  return(allocation.result)
}

costOnlyQuantityLimitAllocationAlgoEx2 <- function(){
  
  executeCypher(path=modifyAssetQuantityCypherPath1)
  executeCypher(path=modifyAssetCostCypherPath1)
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=c(0,0,1,0))
  
  executeCypher(path=restoreAssetQuantityCypherPath1)
  executeCypher(path=restoreAssetCostCypherPath1)
  
  return(allocation.result)
}

#### EXAMPLES RESULTS ####################
costOnlyQuantityLimitAllocationAlgoEx1()
costOnlyQuantityLimitAllocationAlgoEx2()


