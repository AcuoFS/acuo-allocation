library('RUnit')
source('src/allocationFunction.R')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS INPUT FOR TESTING, PLEASE DO NOT CHANGE ############
emptyList <- c()

nonexistentClientId1 <- 'c001'
nonexistentClientId2 <- 'adhknj'
existentClientId1 <- 'c1'
existentClientId2 <- 'c2'

nonexistentCallIdGroup1 <- c('asdd','null','adhajk')
nonexistentCallIdGroup2 <- c('mc001d','mc01010')
existentCallIdToClient1Group1 <- c("mc1","mc2")
existentCallIdToClient1Group2 <- c('mc1','mc2','mc3','mc5')
existentCallIdToClient1Group3 <- c("mc1","mc2","mc3","mc4","mc5","mc8","mc9","mc10")

prefForCostOnlyAllocationAlgo <- c(0,0,1)

modifyDBToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modifyAssetQuantityToSimulateCostOnlyNoConstraintAllocationAlgo.load'
restoreDBDueToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restoreAssetQuantityDueToSimulateCostOnlyNoConstraintAllocationAlgo.load'

modifyAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetQuantityToSimulateCostOnlyQuantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetQuantityDueToSimulateCostOnlyQuantityLimitAllocationAlgo.load'

modifyAssetCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetCostToTestAllocationAlgo.load'
restoreAssetCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetCostToTestAllocationAlgo.load'


###### TEST FUNCTIONS ##############################################
testCostOnlyNoConstraintAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsWhichDirectToTheClient<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group2
  clientId <- existentClientId1
  pref <- prefForCostOnlyAllocationAlgo

  # modify the database before running allocation function
  executeCypher(path=modifyDBToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath)
  
  # test function: allocationAlgo(callId,clientId,pref)
  input <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$input
  result <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$output
  
  # restore the database after getting the result from allocation funciton
  executeCypher(path=restoreDBDueToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath)
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$Name,'British Pound')
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$NetAmount,1000000)
  checkEquals(round(result[[existentCallIdToClient1Group2[1]]]$Amount,0),1052632)
  checkEquals(round(result[[existentCallIdToClient1Group2[1]]]$Quantity,0),821053)
  
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$Name,'British Pound')
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$NetAmount,1500000)
  checkEquals(round(result[[existentCallIdToClient1Group2[2]]]$Amount,0),1578947)
  checkEquals(round(result[[existentCallIdToClient1Group2[2]]]$Quantity,0),1231579)
  
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$Asset,'CAD')
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$Name,'Canadian Dollar')
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$NetAmount,2000000)
  checkEquals(round(result[[existentCallIdToClient1Group2[3]]]$Amount,0),2105263)
  checkEquals(round(result[[existentCallIdToClient1Group2[3]]]$Quantity,0),2800000)
  
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$Name,'British Pound')
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$NetAmount,3000000)
  checkEquals(round(result[[existentCallIdToClient1Group2[4]]]$Amount,0),3157895)
  checkEquals(round(result[[existentCallIdToClient1Group2[4]]]$Quantity,0),2463158)
  
}
if(0){
testCostOnlyQuantityLimitAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsWhichDirectToTheClient<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group3
  clientId <- existentClientId1
  pref <- prefForCostOnlyAllocationAlgo
  
  # modify the database before running allocation function
  executeCypher(path=modifyAssetQuantityCypherPath1)
  executeCypher(path=modifyAssetCostCypherPath1)
  
  # test function: allocationAlgo(callId,clientId,pref)
  input <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$input
  result<- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$output
  
  # restore the database after getting the result from allocation funciton
  executeCypher(path=restoreAssetQuantityCypherPath1)
  executeCypher(path=restoreAssetCostCypherPath1)
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  assetIdCall1<- result[[existentCallIdToClient1Group3[1]]]$Asset
  checkTrue(setequal(assetIdCall1,c('GBP','US912796HW25')))
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$Name[assetIdCall1=='GBP'],'British Pound')
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$Name[assetIdCall1=='US912796HW25'],'U.S. Treasury Bills')
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$NetAmount[assetIdCall1=='GBP'],50)
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$NetAmount[assetIdCall1=='US912796HW25'],9950)
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$Amount[assetIdCall1=='GBP'],2),52.63)
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$Amount[assetIdCall1=='US912796HW25'],10000)
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$quantity[assetIdCall1=='GBP'],3),42.105)
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$quantity[assetIdCall1=='US912796HW25'],100)
  
  
  assetIdCall2 <- result[[existentCallIdToClient1Group3[2]]]$Asset
  checkTrue(setequal(assetIdCall2,c('CAD','GBP','JPY')))
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Name[assetIdCall2=='CAD'],'Canadian dollar')
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Name[assetIdCall2=='GBP'],'British Pound')
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Name[assetIdCall2=='JPY'],'Japanese yen')
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$NetAmount[assetIdCall2=='CAD'],2),10298.51)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$NetAmount[assetIdCall2=='GBP'],2),110.28)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$NetAmount[assetIdCall2=='JPY'],2),4591.21)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Amount[assetIdCall2=='CAD'],2),11194.03)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Amount[assetIdCall2=='GBP'],2),110.28)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Amount[assetIdCall2=='JPY'],2),4591.21)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$quantity[assetIdCall2=='CAD'],2),15000.00)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$quantity[assetIdCall2=='GBP'],3),88.225)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$quantity[assetIdCall2=='JPY'],3),472894.737)
  
  assetIdCall3<- result[[existentCallIdToClient1Group3[3]]]$Asset
  checkTrue(setequal(assetIdCall3,'USD'))
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$Name[assetIdCall3=='USD'],'US dollar')
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$NetAmount[assetIdCall3=='USD'],20000)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Amount[assetIdCall3=='USD'],1),20000)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$quantity[assetIdCall3=='USD'],1),20000)
  
  assetIdCall4<- result[[existentCallIdToClient1Group3[4]]]$Asset
  checkTrue(setequal(assetIdCall4,'USD'))
  checkEquals(result[[existentCallIdToClient1Group3[4]]]$Name[assetIdCall4=='USD'],'US dollar')
  checkEquals(result[[existentCallIdToClient1Group3[4]]]$NetAmount[assetIdCall4=='USD'],250)
  checkEquals(round(result[[existentCallIdToClient1Group3[4]]]$Amount[assetIdCall4=='USD'],2),263.16)
  checkEquals(round(result[[existentCallIdToClient1Group3[4]]]$Amount[assetIdCall4=='USD'],2),263.16)
  
  assetIdCall5<- result[[existentCallIdToClient1Group3[5]]]$Asset
  checkTrue(setequal(assetIdCall5,'37833100'))
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$Name[assetIdCall5=='37833100'],'APPL INC')
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$NetAmount[assetIdCall5=='37833100'],1),30057.3)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Amount[assetIdCall5=='37833100'],0),42939)
  
  assetIdCall6<- result[[existentCallIdToClient1Group3[6]]]$Asset
  checkTrue(setequal(assetIdCall6,'USD'))
  checkEquals(result[[existentCallIdToClient1Group3[6]]]$Name[assetIdCall6=='USD'],'US dollar')
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$NetAmount[assetIdCall6=='USD'],2),15000)
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$Amount[assetIdCall6=='USD'],0),15000)
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$quantity[assetIdCall6=='USD'],0),15000)
  
  assetIdCall7<- result[[existentCallIdToClient1Group3[7]]]$Asset
  checkTrue(setequal(assetIdCall7,c('USD','JPY')))
  checkEquals(result[[existentCallIdToClient1Group3[7]]]$Name[assetIdCall7=='USD'],'US dollar')
  checkEquals(result[[existentCallIdToClient1Group3[7]]]$Name[assetIdCall7=='JPY'],'Japanese yen')
  checkEquals(round(result[[existentCallIdToClient1Group3[7]]]$NetAmount[assetIdCall7=='USD'],2),4736.84)
  checkEquals(round(result[[existentCallIdToClient1Group3[7]]]$NetAmount[assetIdCall7=='JPY'],2),263.16)
  checkEquals(round(result[[existentCallIdToClient1Group3[7]]]$Amount[assetIdCall7=='USD'],2),4736.84)
  checkEquals(round(result[[existentCallIdToClient1Group3[7]]]$Amount[assetIdCall7=='JPY'],2),263.16)
  checkEquals(round(result[[existentCallIdToClient1Group3[7]]]$quantity[assetIdCall7=='USD'],2),4736.84)
  checkEquals(round(result[[existentCallIdToClient1Group3[7]]]$quantity[assetIdCall7=='JPY'],2),27105.26)
  

  assetIdCall8<- result[[existentCallIdToClient1Group3[8]]]$Asset
  checkTrue(setequal(assetIdCall8,'USD'))
  checkEquals(result[[existentCallIdToClient1Group3[8]]]$Name[assetIdCall8=='USD'],'US dollar')
  checkEquals(result[[existentCallIdToClient1Group3[8]]]$NetAmount[assetIdCall8=='USD'],10000)
  checkEquals(round(result[[existentCallIdToClient1Group3[8]]]$Amount[assetIdCall8=='USD'],0),10000)
  checkEquals(round(result[[existentCallIdToClient1Group3[8]]]$quantity[assetIdCall8=='USD'],0),10000)
}
}
