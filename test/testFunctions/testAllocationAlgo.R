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
existentCallIdToClient1Group1 <- c('mc1','mc2')
existentCallIdToClient1Group2 <- c('mc1','mc2','mc3','mc5')
existentCallIdToClient1Group3 <- c('mc1','mc2','mc3','mc4','mc5','mc8')
existentCallIdToClient1Group4 <- c('mc4','mc8','mc12','mc13','mc16')
existentCallIdToClient1Group5 <- c('mc2','mc9','mc10','mc11','mc14','mc15','mc17','mc19','mc20')

prefForCostOnlyAllocationAlgo <- c(0,0,1)
prefForLiquidityOnlyAllocationAlgo <- c(0,1,0)

modifyDBToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modifyAssetQuantityToSimulateCostOnlyNoConstraintAllocationAlgo.load'
restoreDBDueToSimulateCostOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restoreAssetQuantityDueToSimulateCostOnlyNoConstraintAllocationAlgo.load'

modifyDBToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modifyAssetQuantityToSimulateLiquidityOnlyNoConstraintAllocationAlgo.load'
restoreDBDueToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restoreAssetQuantityDueToSimulateLiquidityOnlyNoConstraintAllocationAlgo.load'


modifyAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetQuantityToSimulateCostOnlyQuantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetQuantityDueToSimulateCostOnlyQuantityLimitAllocationAlgo.load'

modifyAssetQuantityCypherPath2 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify2AssetQuantityToSimulateCostOnlyQuantityLimitAllocationAlgo.load'
restoreAssetQuantityCypherPath2 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore2AssetQuantityDueToSimulateCostOnlyQuantityLimitAllocationAlgo.load'

modifyAssetInternalCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetInternalCostToTestAllocationAlgo.load'
restoreAssetInternalCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetInternalCostToTestAllocationAlgo.load'

modifyAssetExternalCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/modify1AssetExternalCostToTestAllocationAlgo.load'
restoreAssetExternalCostCypherPath1 <- 'https://raw.githubusercontent.com/AcuoFS/acuo-allocation/master/test/testFiles/restore1AssetExternalCostToTestAllocationAlgo.load'

###### TEST FUNCTIONS ##############################################
testCostOnlyNoConstraintAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsGroup2<-function(){
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
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$`NetAmount(USD)`,1000000)
  checkEquals(round(result[[existentCallIdToClient1Group2[1]]]$Amount,0),1052632)
  checkEquals(round(result[[existentCallIdToClient1Group2[1]]]$Quantity,0),821053)
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$CustodianAccount,'custac4')
  
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$Name,'British Pound')
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$`NetAmount(USD)`,1500000)
  checkEquals(round(result[[existentCallIdToClient1Group2[2]]]$Amount,0),1578947)
  checkEquals(round(result[[existentCallIdToClient1Group2[2]]]$Quantity,0),1231579)
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$CustodianAccount,'custac4')
  
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$Asset,'CAD')
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$Name,'Canadian Dollar')
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$`NetAmount(USD)`,2000000)
  checkEquals(round(result[[existentCallIdToClient1Group2[3]]]$Amount,0),2105263)
  checkEquals(round(result[[existentCallIdToClient1Group2[3]]]$Quantity,0),2800000)
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$Currency,'CAD')
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$CustodianAccount,'custac5')
  
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$Name,'British Pound')
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$`NetAmount(USD)`,3000000)
  checkEquals(round(result[[existentCallIdToClient1Group2[4]]]$Amount,0),3157895)
  checkEquals(round(result[[existentCallIdToClient1Group2[4]]]$Quantity,0),2463158)
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$CustodianAccount,'custac4')
  
}

testCostOnlyNoConstraintAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsGroup4<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group4
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
  
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Asset,'CAD')
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Name,'Canadian Dollar')
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$`NetAmount(USD)`,250)
  checkEquals(round(result[[existentCallIdToClient1Group4[1]]]$Amount,0),263)
  checkEquals(round(result[[existentCallIdToClient1Group4[1]]]$Quantity,0),350)
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Currency,'CAD')
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$CustodianAccount,'custac5')
  
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Name,'British Pound')
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$`NetAmount(USD)`,1000000)
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$Amount,0),1052632)
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$Quantity,0),821053)
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$CustodianAccount,'custac4')
  
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Asset,'US30231G1022')
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Name,'EXXON MOBIL CORP')
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$`NetAmount(USD)`,500000)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Amount,0),714286)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Quantity,0),8542)
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Haircut,0.3)
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Currency,'USD')
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$CustodianAccount,'custac3')
  
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Asset,'US30231G1022')
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Name,'EXXON MOBIL CORP')
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$`NetAmount(USD)`,700000)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$Amount,0),1000000)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$Quantity,0),11959)
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Haircut,0.3)
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Currency,'USD')
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$CustodianAccount,'custac3')
  
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Asset,'US30231G1022')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name,'EXXON MOBIL CORP')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`,1000000)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount,0),1428571)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity,0),17084)
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Haircut,0.3)
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Currency,'USD')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount,'custac3')
}

testCostOnlyQuantityLimitAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsGroup3<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group3
  clientId <- existentClientId1
  pref <- prefForCostOnlyAllocationAlgo
  
  # modify the database before running allocation function
  executeCypher(path=modifyAssetQuantityCypherPath1)
  executeCypher(path=modifyAssetInternalCostCypherPath1)
  executeCypher(path=modifyAssetExternalCostCypherPath1)
  
  # test function: allocationAlgo(callId,clientId,pref)
  input <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$input
  result<- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$output
  
  # restore the database after getting the result from allocation funciton
  executeCypher(path=restoreAssetQuantityCypherPath1)
  executeCypher(path=restoreAssetInternalCostCypherPath1)
  executeCypher(path=restoreAssetExternalCostCypherPath1)
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  assetIdCall1<- result[[existentCallIdToClient1Group3[1]]]$Asset
  checkTrue(setequal(assetIdCall1,c('EUR','SGD')))
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$Name[assetIdCall1=='EUR'],'Euro')
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$Name[assetIdCall1=='SGD'],'Singapore Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$`NetAmount(USD)`[assetIdCall1=='EUR'],2),661971.83)
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$`NetAmount(USD)`[assetIdCall1=='SGD'],2),338028.17)
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$Amount[assetIdCall1=='EUR'],2),696812.45)
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$Amount[assetIdCall1=='SGD'],2),355819.13)
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$Quantity[assetIdCall1=='EUR'],2),648035.58)
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$Quantity[assetIdCall1=='SGD'],2),505263.16)
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$CustodianAccount[assetIdCall1=='EUR'],'custac4')
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$CustodianAccount[assetIdCall1=='SGD'],'custac4')
  
  assetIdCall2 <- result[[existentCallIdToClient1Group3[2]]]$Asset
  checkTrue(setequal(assetIdCall2,c('GBP','JPY')))
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Name[assetIdCall2=='GBP'],'British Pound')
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Name[assetIdCall2=='JPY'],'Japanese Yen')
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$`NetAmount(USD)`[assetIdCall2=='GBP'],2),1483318.70)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$`NetAmount(USD)`[assetIdCall2=='JPY'],2),16681.30)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Amount[assetIdCall2=='GBP'],2),1561388.11)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Amount[assetIdCall2=='JPY'],2),17559.26)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Quantity[assetIdCall2=='GBP'],3),1217882.723)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Quantity[assetIdCall2=='JPY'],3),2000000.000)
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$CustodianAccount[assetIdCall2=='GBP'],'custac4')
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$CustodianAccount[assetIdCall2=='JPY'],'custac4')
  
  assetIdCall3<- result[[existentCallIdToClient1Group3[3]]]$Asset
  checkTrue(setequal(assetIdCall3,c('AUD','CAD','HKD')))
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$Name[assetIdCall3=='AUD'],'Australian Dollar')
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$Name[assetIdCall3=='CAD'],'Canadian Dollar')
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$Name[assetIdCall3=='HKD'],'Hongkong Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$`NetAmount(USD)`[assetIdCall3=='AUD'],2),326833.21)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$`NetAmount(USD)`[assetIdCall3=='CAD'],2),1428321.43)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$`NetAmount(USD)`[assetIdCall3=='HKD'],2),244845.36)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Amount[assetIdCall3=='AUD'],2),344034.96)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Amount[assetIdCall3=='CAD'],2),1503496.24)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Amount[assetIdCall3=='HKD'],2),257731.96)
  
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Quantity[assetIdCall3=='AUD'],2),461006.84)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Quantity[assetIdCall3=='CAD'],2),1999650.00)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Quantity[assetIdCall3=='HKD'],2),2000000.00)
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$CustodianAccount[assetIdCall3=='AUD'],'custac5')
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$CustodianAccount[assetIdCall3=='CAD'],'custac5')
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$CustodianAccount[assetIdCall3=='HKD'],'custac5')
  
  assetIdCall4<- result[[existentCallIdToClient1Group3[4]]]$Asset
  checkTrue(setequal(assetIdCall4,'CAD'))
  checkEquals(result[[existentCallIdToClient1Group3[4]]]$Name[assetIdCall4=='CAD'],'Canadian Dollar')
  checkEquals(result[[existentCallIdToClient1Group3[4]]]$`NetAmount(USD)`[assetIdCall4=='CAD'],250)
  checkEquals(round(result[[existentCallIdToClient1Group3[4]]]$Amount[assetIdCall4=='CAD'],2),263.16)
  checkEquals(round(result[[existentCallIdToClient1Group3[4]]]$Quantity[assetIdCall4=='CAD'],2),350)
  checkEquals(result[[existentCallIdToClient1Group3[4]]]$Haircut[assetIdCall4=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[4]]]$CustodianAccount[assetIdCall4=='CAD'],'custac5')
  
  assetIdCall5<- result[[existentCallIdToClient1Group3[5]]]$Asset
  checkTrue(setequal(assetIdCall5,c('EUR','GBP','USD')))
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$Name[assetIdCall5=='EUR'],'Euro')
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$Name[assetIdCall5=='GBP'],'British Pound')
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$Name[assetIdCall5=='USD'],'US Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$`NetAmount(USD)`[assetIdCall5=='EUR'],2),1381038.92)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$`NetAmount(USD)`[assetIdCall5=='GBP'],2),952578.74)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$`NetAmount(USD)`[assetIdCall5=='USD'],2),666382.34)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Amount[assetIdCall5=='EUR'],2),1453725.18)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Amount[assetIdCall5=='GBP'],2),1002714.46)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Amount[assetIdCall5=='USD'],2),666382.34)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Quantity[assetIdCall5=='EUR'],2),1351964.42)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Quantity[assetIdCall5=='GBP'],2),782117.28)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Quantity[assetIdCall5=='USD'],2),666382.34)
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$CustodianAccount[assetIdCall5=='EUR'],'custac4')
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$CustodianAccount[assetIdCall5=='GBP'],'custac4')
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$CustodianAccount[assetIdCall5=='USD'],'custac4')
  
  assetIdCall6<- result[[existentCallIdToClient1Group3[6]]]$Asset
  checkTrue(setequal(assetIdCall6,'SGD'))
  checkEquals(result[[existentCallIdToClient1Group3[6]]]$Name[assetIdCall6=='SGD'],'Singapore Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$`NetAmount(USD)`[assetIdCall6=='SGD'],2),1000000)
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$Amount[assetIdCall6=='SGD'],2),1052631.58)
  checkEquals(result[[existentCallIdToClient1Group3[6]]]$Haircut[assetIdCall6=='SGD'],0.05)
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$Quantity[assetIdCall6=='SGD'],2),1494736.84)
  checkEquals(result[[existentCallIdToClient1Group3[6]]]$CustodianAccount[assetIdCall6=='SGD'],'custac4')
  
}

testCostOnlyQuantityLimitAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsGroup4<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group4
  clientId <- existentClientId1
  pref <- prefForCostOnlyAllocationAlgo
  
  # modify the database before running allocation function
  executeCypher(path=modifyAssetQuantityCypherPath2)
  executeCypher(path=modifyAssetInternalCostCypherPath1)
  executeCypher(path=modifyAssetExternalCostCypherPath1)
  
  # test function: allocationAlgo(callId,clientId,pref)
  input <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$input
  result<- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$output
  
  # restore the database after getting the result from allocation funciton
  executeCypher(path=restoreAssetQuantityCypherPath2)
  executeCypher(path=restoreAssetInternalCostCypherPath1)
  executeCypher(path=restoreAssetExternalCostCypherPath1)
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  assetIdCall1<- result[[existentCallIdToClient1Group4[1]]]$Asset
  checkTrue(setequal(assetIdCall1,c('CAD')))
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Asset[assetIdCall1=='CAD'],'CAD')
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Name[assetIdCall1=='CAD'],'Canadian Dollar')
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$`NetAmount(USD)`[assetIdCall1=='CAD'],250)
  checkEquals(round(result[[existentCallIdToClient1Group4[1]]]$Amount[assetIdCall1=='CAD'],2),263.16)
  checkEquals(round(result[[existentCallIdToClient1Group4[1]]]$Quantity[assetIdCall1=='CAD'],2),350)
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Haircut[assetIdCall1=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Currency[assetIdCall1=='CAD'],'CAD')
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$CustodianAccount[assetIdCall1=='CAD'],'custac5')
  
  assetIdCall2<- result[[existentCallIdToClient1Group4[2]]]$Asset
  checkTrue(setequal(assetIdCall2,'SGD'))
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Name[assetIdCall2=='SGD'],'Singapore Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$`NetAmount(USD)`[assetIdCall2=='SGD'],2),1000000)
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$Amount[assetIdCall2=='SGD'],2),1052631.58)
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Haircut[assetIdCall2=='SGD'],0.05)
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$Quantity[assetIdCall2=='SGD'],2),1494736.84)
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$CustodianAccount[assetIdCall2=='SGD'],'custac4')
  
  
  assetIdCall3<- result[[existentCallIdToClient1Group4[3]]]$Asset
  checkTrue(setequal(assetIdCall3,c('FR0000570947','GB00B4YRFP41','US38141G1040')))
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Name[assetIdCall3=='FR0000570947'],'FRANCE O.A.T. STRIP')
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Name[assetIdCall3=='GB00B4YRFP41'],'UK TREASURY BILL GBP')
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Name[assetIdCall3=='US38141G1040'],'GOLDMAN SACHS GROUP INC')
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$`NetAmount(USD)`[assetIdCall3=='FR0000570947'],2),89880.80)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$`NetAmount(USD)`[assetIdCall3=='GB00B4YRFP41'],2),355417.70)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$`NetAmount(USD)`[assetIdCall3=='US38141G1040'],2),54701.50)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Amount[assetIdCall3=='FR0000570947'],2),100989.66)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Amount[assetIdCall3=='GB00B4YRFP41'],2),394908.56)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Amount[assetIdCall3=='US38141G1040'],2),78145.00)
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Haircut[assetIdCall3=='FR0000570947'],0.11)
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Haircut[assetIdCall3=='GB00B4YRFP41'],0.10)
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Haircut[assetIdCall3=='US38141G1040'],0.30)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Quantity[assetIdCall3=='FR0000570947'],2),375681.54)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Quantity[assetIdCall3=='GB00B4YRFP41'],2),30802867.38)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Quantity[assetIdCall3=='US38141G1040'],2),500.00)
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$CustodianAccount[assetIdCall3=='FR0000570947'],'custac2')
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$CustodianAccount[assetIdCall3=='GB00B4YRFP41'],'custac2')
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$CustodianAccount[assetIdCall3=='US38141G1040'],'custac3')
  
  assetIdCall4<- result[[existentCallIdToClient1Group4[4]]]$Asset
  checkTrue(setequal(assetIdCall4,c('FR0000570905','GB00B4YRFP41')))
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Name[assetIdCall4=='FR0000570905'],'FRANCE O.A.T. STRIP')
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Name[assetIdCall4=='GB00B4YRFP41'],'UK TREASURY BILL GBP')
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$`NetAmount(USD)`[assetIdCall4=='FR0000570905'],2),478494.62)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$`NetAmount(USD)`[assetIdCall4=='GB00B4YRFP41'],2),221505.38)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$Amount[assetIdCall4=='FR0000570905'],2),537634.41)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$Amount[assetIdCall4=='GB00B4YRFP41'],2),246117.08)
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Haircut[assetIdCall4=='FR0000570905'],0.11)
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Haircut[assetIdCall4=='GB00B4YRFP41'],0.10)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$Quantity[assetIdCall4=='FR0000570905'],2),2000000.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$Quantity[assetIdCall4=='GB00B4YRFP41'],2),19197132.62)
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$CustodianAccount[assetIdCall4=='FR0000570905'],'custac2')
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$CustodianAccount[assetIdCall4=='GB00B4YRFP41'],'custac2')
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Currency[assetIdCall4=='FR0000570905'],'EUR')
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Currency[assetIdCall4=='GB00B4YRFP41'],'GBP')
  
  
  assetIdCall5<- result[[existentCallIdToClient1Group4[5]]]$Asset
  checkTrue(setequal(assetIdCall5,c('SGD','US30231G1022','US3696041033','US4592001014','US46625H1005','US5801351017')))
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='SGD'],'Singapore Dollar')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US30231G1022'],'EXXON MOBIL CORP')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US3696041033'],'GENERAL ELECTRIC CO')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US4592001014'],'INTL BUSINESS MACHINES CORP')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US46625H1005'],'JPMORGAN CHASE & CO')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US5801351017'],'MCDONALD\'S CORP')
  
  
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='SGD'],2),843336.5)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US30231G1022'],2),29267.0)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US3696041033'],2),10881.5)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US4592001014'],2),52020.5)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US46625H1005'],2),21161.0)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US5801351017'],2),43333.5)
  
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='SGD'],2),887722.63)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US30231G1022'],2),41810.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US3696041033'],2),15545.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US4592001014'],2),74315.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US46625H1005'],2),30230.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US5801351017'],2),61905.00)
  
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='SGD'],2),1260566.14)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US30231G1022'],2),500.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US3696041033'],2),500.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US4592001014'],2),500.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US46625H1005'],2),500.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US5801351017'],2),500.00)
  
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='SGD'],'custac4')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US30231G1022'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US3696041033'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US4592001014'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US46625H1005'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US5801351017'],'custac3')
  
}

testLiquidityOnlyNoConstraintAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsGroup2<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group2
  clientId <- existentClientId1
  pref <- prefForLiquidityOnlyAllocationAlgo
  
  # modify the database before running allocation function
  executeCypher(path=modifyDBToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath)
  
  # test function: allocationAlgo(callId,clientId,pref)
  input <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$input
  result <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$output
  
  # restore the database after getting the result from allocation funciton
  executeCypher(path=restoreDBDueToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath)
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdToClient1Group2[1]]]$`NetAmount(USD)`,2),1000000)
  checkEquals(round(result[[existentCallIdToClient1Group2[1]]]$Amount,2),1052631.58)
  checkEquals(round(result[[existentCallIdToClient1Group2[1]]]$Quantity,2),821052.63)
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[1]]]$CustodianAccount,'custac4')
  
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdToClient1Group2[2]]]$`NetAmount(USD)`,2),1500000)
  checkEquals(round(result[[existentCallIdToClient1Group2[2]]]$Amount,2),1578947.37)
  checkEquals(round(result[[existentCallIdToClient1Group2[2]]]$Quantity,2),1231578.95)
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[2]]]$CustodianAccount,'custac4')
  
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$Asset,'CAD')
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$Name,'Canadian Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group2[3]]]$`NetAmount(USD)`,2),2000000)
  checkEquals(round(result[[existentCallIdToClient1Group2[3]]]$Amount,2),2105263.16)
  checkEquals(round(result[[existentCallIdToClient1Group2[3]]]$Quantity,2),2800000)
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$Currency,'CAD')
  checkEquals(result[[existentCallIdToClient1Group2[3]]]$CustodianAccount,'custac5')
  
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdToClient1Group2[4]]]$`NetAmount(USD)`,2),3000000)
  checkEquals(round(result[[existentCallIdToClient1Group2[4]]]$Amount,2),3157894.74)
  checkEquals(round(result[[existentCallIdToClient1Group2[4]]]$Quantity,2),2463157.90)
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdToClient1Group2[4]]]$CustodianAccount,'custac4')
  
}

testLiquidityOnlyNoConstraintAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsGroup4<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group4
  clientId <- existentClientId1
  pref <- prefForLiquidityOnlyAllocationAlgo
  
  # modify the database before running allocation function
  executeCypher(path=modifyDBToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath)
  
  # test function: allocationAlgo(callId,clientId,pref)
  input <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$input
  result <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$output
  
  # restore the database after getting the result from allocation funciton
  executeCypher(path=restoreDBDueToSimulateLiquidityOnlyNoConstraintAllocationAlgoCypherPath)
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Asset,'CAD')
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Name,'Canadian Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group4[1]]]$`NetAmount(USD)`,2),250)
  checkEquals(round(result[[existentCallIdToClient1Group4[1]]]$Amount,2),263.16)
  checkEquals(round(result[[existentCallIdToClient1Group4[1]]]$Quantity,2),350)
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Currency,'CAD')
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$CustodianAccount,'custac5')
  
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$`NetAmount(USD)`,2),1000000)
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$Amount,2),1052631.58)
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$Quantity,2),821052.63)
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Haircut,0.05)
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$CustodianAccount,'custac4')
  
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Asset,'US30231G1022')
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Name,'EXXON MOBIL CORP')
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$`NetAmount(USD)`,2),500000)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Amount,2),714285.71)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Quantity,2),8542.04)
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Haircut,0.3)
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Currency,'USD')
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$CustodianAccount,'custac3')
  
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Asset,'US30231G1022')
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Name,'EXXON MOBIL CORP')
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$`NetAmount(USD)`,2),700000)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$Amount,2),1000000)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$Quantity,2),11958.86)
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Haircut,0.3)
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Currency,'USD')
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$CustodianAccount,'custac3')
  
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Asset,'US30231G1022')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name,'EXXON MOBIL CORP')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`,1000000)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount,2),1428571.43)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity,2),17084.09)
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Haircut,0.3)
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Currency,'USD')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount,'custac3')
}

testLiquidityOnlyQuantityLimitAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsGroup3<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group3
  clientId <- existentClientId1
  pref <- prefForLiquidityOnlyAllocationAlgo
  
  # modify the database before running allocation function
  executeCypher(path=modifyAssetQuantityCypherPath1)
  
  # test function: allocationAlgo(callId,clientId,pref)
  input <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$input
  result<- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$output
  
  # restore the database after getting the result from allocation funciton
  executeCypher(path=restoreAssetQuantityCypherPath1)
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  assetIdCall1<- result[[existentCallIdToClient1Group3[1]]]$Asset
  checkTrue(setequal(assetIdCall1,c('GBP')))
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$Name[assetIdCall1=='GBP'],'British Pound')
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$`NetAmount(USD)`[assetIdCall1=='GBP'],2),1000000)
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$Amount[assetIdCall1=='GBP'],2),1052631.58)
  checkEquals(round(result[[existentCallIdToClient1Group3[1]]]$Quantity[assetIdCall1=='GBP'],2),821052.63)
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$Haircut[assetIdCall1=='GBP'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[1]]]$CustodianAccount[assetIdCall1=='GBP'],'custac4')
  
  assetIdCall2 <- result[[existentCallIdToClient1Group3[2]]]$Asset
  checkTrue(setequal(assetIdCall2,c('CAD','GBP','SGD')))
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Name[assetIdCall2=='CAD'],'Canadian Dollar')
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Name[assetIdCall2=='GBP'],'British Pound')
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Name[assetIdCall2=='SGD'],'Singapore Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$`NetAmount(USD)`[assetIdCall2=='CAD'],2),91077.24)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$`NetAmount(USD)`[assetIdCall2=='GBP'],2),478908.19)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$`NetAmount(USD)`[assetIdCall2=='SGD'],2),930014.57)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Amount[assetIdCall2=='CAD'],2),95870.78)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Amount[assetIdCall2=='GBP'],2),504113.88)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Amount[assetIdCall2=='SGD'],2),978962.71)
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Haircut[assetIdCall2=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Haircut[assetIdCall2=='GBP'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$Haircut[assetIdCall2=='SGD'],0.05)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Quantity[assetIdCall2=='CAD'],2),127508.13)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Quantity[assetIdCall2=='GBP'],2),393208.83)
  checkEquals(round(result[[existentCallIdToClient1Group3[2]]]$Quantity[assetIdCall2=='SGD'],2),1390127.05)
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$CustodianAccount[assetIdCall2=='CAD'],'custac5')
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$CustodianAccount[assetIdCall2=='GBP'],'custac4')
  checkEquals(result[[existentCallIdToClient1Group3[2]]]$CustodianAccount[assetIdCall2=='SGD'],'custac4')
  
  assetIdCall3<- result[[existentCallIdToClient1Group3[3]]]$Asset
  checkTrue(setequal(assetIdCall3,c('AUD','CAD','HKD')))
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$Name[assetIdCall3=='AUD'],'Australian Dollar')
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$Name[assetIdCall3=='CAD'],'Canadian Dollar')
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$Name[assetIdCall3=='HKD'],'Hongkong Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$`NetAmount(USD)`[assetIdCall3=='AUD'],2),661805.81)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$`NetAmount(USD)`[assetIdCall3=='CAD'],2),1337244.19)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$`NetAmount(USD)`[assetIdCall3=='HKD'],2),950.00)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Amount[assetIdCall3=='AUD'],2),696637.69)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Amount[assetIdCall3=='CAD'],2),1407625.47)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Amount[assetIdCall3=='HKD'],2),1000.00)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Quantity[assetIdCall3=='AUD'],2),933494.51)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Quantity[assetIdCall3=='CAD'],2),1872141.87)
  checkEquals(round(result[[existentCallIdToClient1Group3[3]]]$Quantity[assetIdCall3=='HKD'],2),7760.00)
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$Haircut[assetIdCall3=='AUD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$Haircut[assetIdCall3=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$Haircut[assetIdCall3=='HKD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$CustodianAccount[assetIdCall3=='AUD'],'custac5')
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$CustodianAccount[assetIdCall3=='CAD'],'custac5')
  checkEquals(result[[existentCallIdToClient1Group3[3]]]$CustodianAccount[assetIdCall3=='HKD'],'custac5')
  
  assetIdCall4<- result[[existentCallIdToClient1Group3[4]]]$Asset
  checkTrue(setequal(assetIdCall4,'CAD'))
  checkEquals(result[[existentCallIdToClient1Group3[4]]]$Name[assetIdCall4=='CAD'],'Canadian Dollar')
  checkEquals(result[[existentCallIdToClient1Group3[4]]]$`NetAmount(USD)`[assetIdCall4=='CAD'],250)
  checkEquals(round(result[[existentCallIdToClient1Group3[4]]]$Amount[assetIdCall4=='CAD'],2),263.16)
  checkEquals(round(result[[existentCallIdToClient1Group3[4]]]$Quantity[assetIdCall4=='CAD'],2),350)
  checkEquals(result[[existentCallIdToClient1Group3[4]]]$Haircut[assetIdCall4=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[4]]]$CustodianAccount[assetIdCall4=='CAD'],'custac5')
  
  assetIdCall5<- result[[existentCallIdToClient1Group3[5]]]$Asset
  checkTrue(setequal(assetIdCall5,c('EUR','GBP')))
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$Name[assetIdCall5=='EUR'],'Euro')
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$Name[assetIdCall5=='GBP'],'British Pound')
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$`NetAmount(USD)`[assetIdCall5=='EUR'],2),2043010.75)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$`NetAmount(USD)`[assetIdCall5=='GBP'],2),956989.25)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Amount[assetIdCall5=='EUR'],2),2150537.63)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Amount[assetIdCall5=='GBP'],2),1007357.10)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Quantity[assetIdCall5=='EUR'],2),2000000.00)
  checkEquals(round(result[[existentCallIdToClient1Group3[5]]]$Quantity[assetIdCall5=='GBP'],2),785738.54)
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$Haircut[assetIdCall5=='EUR'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$Haircut[assetIdCall5=='GBP'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$CustodianAccount[assetIdCall5=='EUR'],'custac4')
  checkEquals(result[[existentCallIdToClient1Group3[5]]]$CustodianAccount[assetIdCall5=='GBP'],'custac4')
  
  assetIdCall6<- result[[existentCallIdToClient1Group3[6]]]$Asset
  checkTrue(setequal(assetIdCall6,c('AUD','HKD')))
  checkEquals(result[[existentCallIdToClient1Group3[6]]]$Name[assetIdCall6=='AUD'],'Australian Dollar')
  checkEquals(result[[existentCallIdToClient1Group3[6]]]$Name[assetIdCall6=='HKD'],'Hongkong Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$`NetAmount(USD)`[assetIdCall6=='AUD'],2),756104.64)
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$`NetAmount(USD)`[assetIdCall6=='HKD'],2),243895.36)
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$Amount[assetIdCall6=='AUD'],2),795899.62)
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$Amount[assetIdCall6=='HKD'],2),256731.96)
  checkEquals(result[[existentCallIdToClient1Group3[6]]]$Haircut[assetIdCall6=='AUD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group3[6]]]$Haircut[assetIdCall6=='HKD'],0.05)
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$Quantity[assetIdCall6=='AUD'],2),1066505.49)
  checkEquals(round(result[[existentCallIdToClient1Group3[6]]]$Quantity[assetIdCall6=='HKD'],2),1992240.00)
  checkEquals(result[[existentCallIdToClient1Group3[6]]]$CustodianAccount[assetIdCall6=='AUD'],'custac5')
  checkEquals(result[[existentCallIdToClient1Group3[6]]]$CustodianAccount[assetIdCall6=='HKD'],'custac5')
  
}

testLiquidityOnlyQuantityLimitAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsGroup4<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group4
  clientId <- existentClientId1
  pref <- prefForLiquidityOnlyAllocationAlgo
  
  # modify the database before running allocation function
  executeCypher(path=modifyAssetQuantityCypherPath2)
  
  # test function: allocationAlgo(callId,clientId,pref)
  input <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$input
  result<- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$output
  
  # restore the database after getting the result from allocation funciton
  executeCypher(path=restoreAssetQuantityCypherPath2)
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  assetIdCall1<- result[[existentCallIdToClient1Group4[1]]]$Asset
  checkTrue(setequal(assetIdCall1,c('CAD')))
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Asset[assetIdCall1=='CAD'],'CAD')
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Name[assetIdCall1=='CAD'],'Canadian Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group4[1]]]$`NetAmount(USD)`[assetIdCall1=='CAD'],2),250)
  checkEquals(round(result[[existentCallIdToClient1Group4[1]]]$Amount[assetIdCall1=='CAD'],2),263.16)
  checkEquals(round(result[[existentCallIdToClient1Group4[1]]]$Quantity[assetIdCall1=='CAD'],2),350)
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Haircut[assetIdCall1=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$Currency[assetIdCall1=='CAD'],'CAD')
  checkEquals(result[[existentCallIdToClient1Group4[1]]]$CustodianAccount[assetIdCall1=='CAD'],'custac5')
  
  assetIdCall2<- result[[existentCallIdToClient1Group4[2]]]$Asset
  checkTrue(setequal(assetIdCall1,c('CAD')))
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Asset[assetIdCall2=='CAD'],'CAD')
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Name[assetIdCall2=='CAD'],'Canadian Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$`NetAmount(USD)`[assetIdCall2=='CAD'],2),1000000)
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$Amount[assetIdCall2=='CAD'],2),1052631.58)
  checkEquals(round(result[[existentCallIdToClient1Group4[2]]]$Quantity[assetIdCall2=='CAD'],0),1400000)
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Haircut[assetIdCall2=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$Currency[assetIdCall2=='CAD'],'CAD')
  checkEquals(result[[existentCallIdToClient1Group4[2]]]$CustodianAccount[assetIdCall2=='CAD'],'custac5')
  
  assetIdCall3<- result[[existentCallIdToClient1Group4[3]]]$Asset
  checkTrue(setequal(assetIdCall3,c('SG3260987684')))
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Name[assetIdCall3=='SG3260987684'],'SINGAPORE GOVERNMENT')
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$`NetAmount(USD)`[assetIdCall3=='SG3260987684'],2),500000)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Amount[assetIdCall3=='SG3260987684'],2),571428.57)
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Haircut[assetIdCall3=='SG3260987684'],0.125)
  checkEquals(round(result[[existentCallIdToClient1Group4[3]]]$Quantity[assetIdCall3=='SG3260987684'],2),811.43)
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$Currency[assetIdCall3=='SG3260987684'],'SGD')
  checkEquals(result[[existentCallIdToClient1Group4[3]]]$CustodianAccount[assetIdCall3=='SG3260987684'],'custac2')
  
  assetIdCall4<- result[[existentCallIdToClient1Group4[4]]]$Asset
  checkTrue(setequal(assetIdCall3,c('SG3260987684')))
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Name[assetIdCall4=='SG3260987684'],'SINGAPORE GOVERNMENT')
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$`NetAmount(USD)`[assetIdCall4=='SG3260987684'],2),700000)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$Amount[assetIdCall4=='SG3260987684'],2),800000)
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Haircut[assetIdCall4=='SG3260987684'],0.125)
  checkEquals(round(result[[existentCallIdToClient1Group4[4]]]$Quantity[assetIdCall4=='SG3260987684'],2),1136)
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$Currency[assetIdCall4=='SG3260987684'],'SGD')
  checkEquals(result[[existentCallIdToClient1Group4[4]]]$CustodianAccount[assetIdCall4=='SG3260987684'],'custac2')
  
  assetIdCall5<- result[[existentCallIdToClient1Group4[5]]]$Asset
  checkTrue(setequal(assetIdCall5,c('EUR','US30231G1022','US3696041033','US38141G1040','US4592001014','US46625H1005','US5801351017')))
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='EUR'],'Euro')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US30231G1022'],'EXXON MOBIL CORP')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US3696041033'],'GENERAL ELECTRIC CO')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US38141G1040'],'GOLDMAN SACHS GROUP INC')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US4592001014'],'INTL BUSINESS MACHINES CORP')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US46625H1005'],'JPMORGAN CHASE & CO')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$Name[assetIdCall5=='US5801351017'],'MCDONALD\'S CORP')
  
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='EUR'],2),788635.0)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US30231G1022'],2),29267.0)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US3696041033'],2),10881.5)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US38141G1040'],2),54701.5)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US4592001014'],2),52020.5)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US46625H1005'],2),21161.0)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$`NetAmount(USD)`[assetIdCall5=='US5801351017'],2),43333.5)
  
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='EUR'],2),830142.11)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US30231G1022'],2),41810.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US3696041033'],2),15545.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US38141G1040'],2),78145.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US4592001014'],2),74315.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US46625H1005'],2),30230.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Amount[assetIdCall5=='US5801351017'],2),61905.00)
  
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='EUR'],2),772032.16)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US30231G1022'],2),500.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US3696041033'],2),500.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US38141G1040'],2),500.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US4592001014'],2),500.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US46625H1005'],2),500.00)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Quantity[assetIdCall5=='US5801351017'],2),500.00)
  
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Haircut[assetIdCall5=='EUR'],2),0.05)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Haircut[assetIdCall5=='US30231G1022'],2),0.3)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Haircut[assetIdCall5=='US3696041033'],2),0.3)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Haircut[assetIdCall5=='US38141G1040'],2),0.3)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Haircut[assetIdCall5=='US4592001014'],2),0.3)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Haircut[assetIdCall5=='US46625H1005'],2),0.3)
  checkEquals(round(result[[existentCallIdToClient1Group4[5]]]$Haircut[assetIdCall5=='US5801351017'],2),0.3)
  
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='EUR'],'custac4')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US30231G1022'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US3696041033'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US38141G1040'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US4592001014'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US46625H1005'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group4[5]]]$CustodianAccount[assetIdCall5=='US5801351017'],'custac3')
  
}

testLiquidityOnlyQuantityLimitAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsGroup5<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callId <- existentCallIdToClient1Group5
  clientId <- existentClientId1
  pref <- prefForLiquidityOnlyAllocationAlgo
  
  # test function: allocationAlgo(callId,clientId,pref)
  input <- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$input
  result<- allocationAlgo(callId=callId,clientId=clientId,pref=pref)$output
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  assetIdCall1<- result[[existentCallIdToClient1Group5[1]]]$Asset
  checkTrue(setequal(assetIdCall1,'CAD'))
  checkEquals(result[[existentCallIdToClient1Group5[1]]]$Name[assetIdCall1=='CAD'],'Canadian Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group5[1]]]$`NetAmount(USD)`[assetIdCall1=='CAD'],2),1500000)
  checkEquals(round(result[[existentCallIdToClient1Group5[1]]]$Amount[assetIdCall1=='CAD'],2),1578947.37)
  checkEquals(round(result[[existentCallIdToClient1Group5[1]]]$Quantity[assetIdCall1=='CAD'],2),2100000)
  checkEquals(result[[existentCallIdToClient1Group5[1]]]$Haircut[assetIdCall1=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group5[1]]]$Currency[assetIdCall1=='CAD'],'CAD')
  checkEquals(result[[existentCallIdToClient1Group5[1]]]$CustodianAccount[assetIdCall1=='CAD'],'custac5')
  
  assetIdCall2<- result[[existentCallIdToClient1Group5[2]]]$Asset
  checkTrue(setequal(assetIdCall2,'CAD'))
  checkEquals(result[[existentCallIdToClient1Group5[2]]]$Name[assetIdCall2=='CAD'],'Canadian Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group5[2]]]$`NetAmount(USD)`[assetIdCall2=='CAD'],2),1000000)
  checkEquals(round(result[[existentCallIdToClient1Group5[2]]]$Amount[assetIdCall2=='CAD'],2),1052631.58)
  checkEquals(round(result[[existentCallIdToClient1Group5[2]]]$Quantity[assetIdCall2=='CAD'],2),1400000)
  checkEquals(result[[existentCallIdToClient1Group5[2]]]$Haircut[assetIdCall2=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group5[2]]]$CustodianAccount[assetIdCall2=='CAD'],'custac5')
  
  assetIdCall3<- result[[existentCallIdToClient1Group5[3]]]$Asset
  checkTrue(setequal(assetIdCall3,c('AUD','CAD')))
  checkEquals(result[[existentCallIdToClient1Group5[3]]]$Name[assetIdCall3=='AUD'],'Australian Dollar')
  checkEquals(result[[existentCallIdToClient1Group5[3]]]$Name[assetIdCall3=='CAD'],'Canadian Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group5[3]]]$`NetAmount(USD)`[assetIdCall3=='AUD'],2),928571.43)
  checkEquals(round(result[[existentCallIdToClient1Group5[3]]]$`NetAmount(USD)`[assetIdCall3=='CAD'],2),71428.57)
  checkEquals(round(result[[existentCallIdToClient1Group5[3]]]$Amount[assetIdCall3=='AUD'],2),977443.61)
  checkEquals(round(result[[existentCallIdToClient1Group5[3]]]$Amount[assetIdCall3=='CAD'],2),75187.97)
  checkEquals(round(result[[existentCallIdToClient1Group5[3]]]$Quantity[assetIdCall3=='AUD'],2),1309774.44)
  checkEquals(round(result[[existentCallIdToClient1Group5[3]]]$Quantity[assetIdCall3=='CAD'],2),100000.00)
  checkEquals(result[[existentCallIdToClient1Group5[3]]]$Haircut[assetIdCall3=='AUD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group5[3]]]$Haircut[assetIdCall3=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group5[3]]]$Currency[assetIdCall3=='AUD'],'AUD')
  checkEquals(result[[existentCallIdToClient1Group5[3]]]$Currency[assetIdCall3=='CAD'],'CAD')
  checkEquals(result[[existentCallIdToClient1Group5[3]]]$CustodianAccount[assetIdCall3=='AUD'],'custac5')
  checkEquals(result[[existentCallIdToClient1Group5[3]]]$CustodianAccount[assetIdCall3=='CAD'],'custac5')
  
  assetIdCall4<- result[[existentCallIdToClient1Group5[4]]]$Asset
  checkTrue(setequal(assetIdCall4,'CAD'))
  checkEquals(result[[existentCallIdToClient1Group5[4]]]$Name[assetIdCall4=='CAD'],'Canadian Dollar')
  checkEquals(round(result[[existentCallIdToClient1Group5[4]]]$`NetAmount(USD)`[assetIdCall4=='CAD'],2),1000000)
  checkEquals(round(result[[existentCallIdToClient1Group5[4]]]$Amount[assetIdCall4=='CAD'],2),1052631.58)
  checkEquals(round(result[[existentCallIdToClient1Group5[4]]]$Quantity[assetIdCall4=='CAD'],2),1400000)
  checkEquals(result[[existentCallIdToClient1Group5[4]]]$Haircut[assetIdCall4=='CAD'],0.05)
  checkEquals(result[[existentCallIdToClient1Group5[4]]]$CustodianAccount[assetIdCall4=='CAD'],'custac5')
  
  
  assetIdCall5<- result[[existentCallIdToClient1Group5[5]]]$Asset
  checkTrue(setequal(assetIdCall5,c('DE0001141737','FR0000570905')))
  checkEquals(result[[existentCallIdToClient1Group5[5]]]$Name[assetIdCall5=='DE0001141737'],'BUNDESOBLIGATION')
  checkEquals(result[[existentCallIdToClient1Group5[5]]]$Name[assetIdCall5=='FR0000570905'],'FRANCE O.A.T. STRIP')
  checkEquals(round(result[[existentCallIdToClient1Group5[5]]]$`NetAmount(USD)`[assetIdCall5=='DE0001141737'],2),742905.68)
  checkEquals(round(result[[existentCallIdToClient1Group5[5]]]$`NetAmount(USD)`[assetIdCall5=='FR0000570905'],2),257094.32)
  checkEquals(round(result[[existentCallIdToClient1Group5[5]]]$Amount[assetIdCall5=='DE0001141737'],2),834725.48)
  checkEquals(round(result[[existentCallIdToClient1Group5[5]]]$Amount[assetIdCall5=='FR0000570905'],2),288870.03)
  checkEquals(result[[existentCallIdToClient1Group5[5]]]$Haircut[assetIdCall5=='DE0001141737'],0.11)
  checkEquals(result[[existentCallIdToClient1Group5[5]]]$Haircut[assetIdCall5=='FR0000570905'],0.11)
  checkEquals(round(result[[existentCallIdToClient1Group5[5]]]$Quantity[assetIdCall5=='DE0001141737'],2),77629469.70)
  checkEquals(round(result[[existentCallIdToClient1Group5[5]]]$Quantity[assetIdCall5=='FR0000570905'],2),1074596.49)
  checkEquals(result[[existentCallIdToClient1Group5[5]]]$Currency[assetIdCall5=='DE0001141737'],'EUR')
  checkEquals(result[[existentCallIdToClient1Group5[5]]]$Currency[assetIdCall5=='FR0000570905'],'EUR')
  checkEquals(result[[existentCallIdToClient1Group5[5]]]$CustodianAccount[assetIdCall5=='DE0001141737'],'custac1')
  checkEquals(result[[existentCallIdToClient1Group5[5]]]$CustodianAccount[assetIdCall5=='FR0000570905'],'custac2')
  
  assetIdCall6<- result[[existentCallIdToClient1Group5[6]]]$Asset
  checkTrue(setequal(assetIdCall6,c('DE0001104636','DE0001141737','XS1045278410')))
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$Name[assetIdCall6=='DE0001104636'],'BUNDESSCHATZANWEISUNGEN')
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$Name[assetIdCall6=='DE0001141737'],'BUNDESOBLIGATION')
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$Name[assetIdCall6=='XS1045278410'],'BK OF ENGLAND EURO NOTE')
  checkEquals(round(result[[existentCallIdToClient1Group5[6]]]$`NetAmount(USD)`[assetIdCall6=='DE0001104636'],2),573186.43)
  checkEquals(round(result[[existentCallIdToClient1Group5[6]]]$`NetAmount(USD)`[assetIdCall6=='DE0001141737'],2),214083.57)
  checkEquals(round(result[[existentCallIdToClient1Group5[6]]]$`NetAmount(USD)`[assetIdCall6=='XS1045278410'],2),162730.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[6]]]$Amount[assetIdCall6=='DE0001104636'],2),644029.70)
  checkEquals(round(result[[existentCallIdToClient1Group5[6]]]$Amount[assetIdCall6=='DE0001141737'],2),240543.34)
  checkEquals(round(result[[existentCallIdToClient1Group5[6]]]$Amount[assetIdCall6=='XS1045278410'],2),182842.70)
  checkEquals(round(result[[existentCallIdToClient1Group5[6]]]$Quantity[assetIdCall6=='DE0001104636'],2),59894761.83)
  checkEquals(round(result[[existentCallIdToClient1Group5[6]]]$Quantity[assetIdCall6=='DE0001141737'],2),22370530.31)
  checkEquals(round(result[[existentCallIdToClient1Group5[6]]]$Quantity[assetIdCall6=='XS1045278410'],2),182.84)
  
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$Haircut[assetIdCall6=='DE0001104636'],0.11)
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$Haircut[assetIdCall6=='DE0001141737'],0.11)
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$Haircut[assetIdCall6=='XS1045278410'],0.11)
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$Currency[assetIdCall6=='DE0001104636'],'EUR')
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$Currency[assetIdCall6=='DE0001141737'],'EUR')
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$Currency[assetIdCall6=='XS1045278410'],'USD')
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$CustodianAccount[assetIdCall6=='DE0001104636'],'custac1')
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$CustodianAccount[assetIdCall6=='DE0001141737'],'custac1')
  checkEquals(result[[existentCallIdToClient1Group5[6]]]$CustodianAccount[assetIdCall6=='XS1045278410'],'custac2')
  
  
  assetIdCall7<- result[[existentCallIdToClient1Group5[7]]]$Asset
  checkTrue(setequal(assetIdCall7,c('US30231G1022','US3696041033','US38141G1040','US4592001014','US46625H1005','US5801351017','XS1045278410')))
  
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$`NetAmount(USD)`[assetIdCall7=='US30231G1022'],2),58534.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$`NetAmount(USD)`[assetIdCall7=='US3696041033'],2),21763.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$`NetAmount(USD)`[assetIdCall7=='US38141G1040'],2),109403.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$`NetAmount(USD)`[assetIdCall7=='US4592001014'],2),104041.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$`NetAmount(USD)`[assetIdCall7=='US46625H1005'],2),42322.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$`NetAmount(USD)`[assetIdCall7=='US5801351017'],2),86667.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$`NetAmount(USD)`[assetIdCall7=='XS1045278410'],2),727270)
  
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Amount[assetIdCall7=='US30231G1022'],2),83620.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Amount[assetIdCall7=='US3696041033'],2),31090.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Amount[assetIdCall7=='US38141G1040'],2),156290.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Amount[assetIdCall7=='US4592001014'],2),148630.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Amount[assetIdCall7=='US46625H1005'],2),60460.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Amount[assetIdCall7=='US5801351017'],2),123810.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Amount[assetIdCall7=='XS1045278410'],2),817157.30)
  
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Quantity[assetIdCall7=='US30231G1022'],2),1000.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Quantity[assetIdCall7=='US3696041033'],2),1000.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Quantity[assetIdCall7=='US38141G1040'],2),1000.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Quantity[assetIdCall7=='US4592001014'],2),1000.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Quantity[assetIdCall7=='US46625H1005'],2),1000.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Quantity[assetIdCall7=='US5801351017'],2),1000.00)
  checkEquals(round(result[[existentCallIdToClient1Group5[7]]]$Quantity[assetIdCall7=='XS1045278410'],2),817.16)
  
  
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$CustodianAccount[assetIdCall7=='US30231G1022'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$CustodianAccount[assetIdCall7=='US3696041033'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$CustodianAccount[assetIdCall7=='US38141G1040'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$CustodianAccount[assetIdCall7=='US4592001014'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$CustodianAccount[assetIdCall7=='US46625H1005'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$CustodianAccount[assetIdCall7=='US5801351017'],'custac3')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$CustodianAccount[assetIdCall7=='XS1045278410'],'custac2')
  
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$Currency[assetIdCall7=='US30231G1022'],'USD')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$Currency[assetIdCall7=='US3696041033'],'USD')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$Currency[assetIdCall7=='US38141G1040'],'USD')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$Currency[assetIdCall7=='US4592001014'],'USD')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$Currency[assetIdCall7=='US46625H1005'],'USD')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$Currency[assetIdCall7=='US5801351017'],'USD')
  checkEquals(result[[existentCallIdToClient1Group5[7]]]$Currency[assetIdCall7=='XS1045278410'],'USD')
  
  assetIdCall8<- result[[existentCallIdToClient1Group5[8]]]$Asset
  checkTrue(setequal(assetIdCall8,c('DE0001104636','SG3260987684')))
  checkEquals(result[[existentCallIdToClient1Group5[8]]]$Name[assetIdCall8=='DE0001104636'],'BUNDESSCHATZANWEISUNGEN')
  checkEquals(result[[existentCallIdToClient1Group5[8]]]$Name[assetIdCall8=='SG3260987684'],'SINGAPORE GOVERNMENT')
  checkEquals(round(result[[existentCallIdToClient1Group5[8]]]$`NetAmount(USD)`[assetIdCall8=='DE0001104636'],2),383802.82)
  checkEquals(round(result[[existentCallIdToClient1Group5[8]]]$`NetAmount(USD)`[assetIdCall8=='SG3260987684'],2),116197.18)
  checkEquals(round(result[[existentCallIdToClient1Group5[8]]]$Amount[assetIdCall8=='DE0001104636'],2),431239.12)
  checkEquals(round(result[[existentCallIdToClient1Group5[8]]]$Amount[assetIdCall8=='SG3260987684'],2),132796.78)
  checkEquals(round(result[[existentCallIdToClient1Group5[8]]]$Quantity[assetIdCall8=='DE0001104636'],2),40105238.17)
  checkEquals(round(result[[existentCallIdToClient1Group5[8]]]$Quantity[assetIdCall8=='SG3260987684'],2),188.57)
  checkEquals(result[[existentCallIdToClient1Group5[8]]]$Haircut[assetIdCall8=='DE0001104636'],0.11)
  checkEquals(result[[existentCallIdToClient1Group5[8]]]$Haircut[assetIdCall8=='SG3260987684'],0.125)
  checkEquals(result[[existentCallIdToClient1Group5[8]]]$CustodianAccount[assetIdCall8=='DE0001104636'],'custac1')
  checkEquals(result[[existentCallIdToClient1Group5[8]]]$CustodianAccount[assetIdCall8=='SG3260987684'],'custac2')
  
  
  assetIdCall9<- result[[existentCallIdToClient1Group5[9]]]$Asset
  checkTrue(setequal(assetIdCall9,c('SG3260987684')))
  checkEquals(round(result[[existentCallIdToClient1Group5[9]]]$`NetAmount(USD)`[assetIdCall9=='SG3260987684'],2),500000)
  checkEquals(round(result[[existentCallIdToClient1Group5[9]]]$Amount[assetIdCall9=='SG3260987684'],2),571428.57)
  checkEquals(round(result[[existentCallIdToClient1Group5[9]]]$Quantity[assetIdCall9=='SG3260987684'],2),811.43)
  checkEquals(result[[existentCallIdToClient1Group5[9]]]$CustodianAccount[assetIdCall9=='SG3260987684'],'custac2')
  checkEquals(result[[existentCallIdToClient1Group5[9]]]$Currency[assetIdCall9=='SG3260987684'],'SGD')
}



