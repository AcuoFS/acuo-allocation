library('RUnit')
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
existentCallIdToClient1Group2 <- c("mc1","mc2","mc3")
existentCallIdToClient1Group3 <- c("mc1","mc2","mc3","mc4","mc5","mc8","mc9","mc10","mc11","mc12","mc16")


nonexistentAssetIdGroup1 <- 'hdilasdij'
nonexistentAssetIdGroup2 <- c('adsa','adil','23er','adhl')
existentAssetIdGroup1<- c('USD','CAD')
existentAssetIdGroup2<- c("US912796HW25","JP1051271G37","GBP","USD","US46625H1005","CAD","JPY","US1912161007")

###### TEST FUNCTIONS ##############################################
testCallDetailsByPassingAnEmptyListOfCallIds <- function(){
 
  # test input: an empty List of  margin call ids
  callId <- emptyList

  # test function: callInfoByCallId(callId)
  result <- callInfoByCallId(callId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)

}

testCallDetailsByPassingAListOfNonexistentCallIds <- function(){
  # test input: a List of nonexistent margin call ids
  callId <- nonexistentCallIdGroup1
  
  # test function: callInfoByCallId(callId)
  result <- callInfoByCallId(callId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)
}

testCallDetailsByPassingAListOfExistentCallIds <- function(){ 
  
  # test input: a List of existent margin call ids
  callIdInput <- existentCallIdToClient1Group3
  
  # test function: callInfoByCallId(callId)
  result <- callInfoByCallId(callIdInput) # function output
  callIdOutput <- result$id
  
  # test output: result should has unitValue specified below
  
  # check whether all call ids have been retrieved
  checkTrue(setequal(callIdOutput,callIdInput))
  callId <- callIdOutput
  
  # check call details
  checkTrue(is.element('mc1',callId))
  checkEquals(result$callAmount[callId=='mc1'],1000000)
  
  checkTrue(is.element('mc2',callId))
  checkEquals(result$callAmount[callId=='mc2'],1500000)
  
  checkTrue(is.element('mc3',callId))
  checkEquals(result$callAmount[callId=='mc3'],2000000)
  
  checkTrue(is.element('mc4',callId))
  checkEquals(result$callAmount[callId=='mc4'],250)
  
  checkTrue(is.element('mc5',callId))
  checkEquals(result$callAmount[callId=='mc5'],3000000)
  
  checkTrue(is.element('mc8',callId))
  checkEquals(result$callAmount[callId=='mc8'],1000000)
  
  checkTrue(is.element('mc9',callId))
  checkEquals(result$callAmount[callId=='mc9'],1000000)
  
  checkTrue(is.element('mc10',callId))
  checkEquals(result$callAmount[callId=='mc10'],1000000)
  
  checkTrue(is.element('mc11',callId))
  checkEquals(result$callAmount[callId=='mc11'],1000000)
  
}

testAssetDetailsByPassingAnEmptyListOfAssetIds <- function(){
  
  # test input: an empty List of asset ids
  assetId <- emptyList
  
  # test function: assetInfoByAssetId(assetId)
  result <- assetInfoByAssetId(assetId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)
  
}

testAssetDetailsByPassingAListOfNonexistentAssetIds <- function(){
  # test input: a List of nonexistent margin asset ids
  assetId <- nonexistentAssetIdGroup1
  
  # test function: assetInfoByAssetId(assetId)
  result <- assetInfoByAssetId(assetId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)
}

testAssetDetailsByPassingAListOfExistentAssetIds <- function(){ 
  
  # test input: a List of existent asset ids
  assetIdInput <- existentAssetIdGroup2
  
  # test function: assetInfoByAssetId(assetId)
  result <- assetInfoByAssetId(assetIdInput) # function output
  assetIdOutput <- result$id
  
  # test output: result should has unitValue specified below
  
  # check whether all asset ids have been retrieved
  checkTrue(setequal(assetIdOutput,assetIdInput))
  assetId <- assetIdOutput
  
  # check asset details
  checkTrue(is.element('USD',assetId))
  checkEquals(result$name[assetId=='USD'],'US Dollar')
  checkEquals(result$unitValue[assetId=='USD'],1)
  checkEquals(result$ICADCode[assetId=='USD'],'US-CASH')
  checkEquals(result$currency[assetId=='USD'],'USD')
  checkEquals(result$type[assetId=='USD'],'CASH')
  
  checkTrue(is.element('GBP',assetId))
  checkEquals(result$name[assetId=='GBP'],'British Pound')
  checkEquals(result$unitValue[assetId=='GBP'],1)
  checkEquals(result$ICADCode[assetId=='GBP'],'GB-CASH')
  checkEquals(result$currency[assetId=='GBP'],'GBP')
  checkEquals(result$type[assetId=='GBP'],'CASH')
  
  checkTrue(is.element('JPY',assetId))
  checkEquals(result$name[assetId=='JPY'],'Japanese Yen')
  checkEquals(result$unitValue[assetId=='JPY'],1)
  checkEquals(result$ICADCode[assetId=='JPY'],'JP-CASH')
  checkEquals(result$currency[assetId=='JPY'],'JPY')
  checkEquals(result$type[assetId=='JPY'],'CASH')
  
  checkTrue(is.element('CAD',assetId))
  checkEquals(result$name[assetId=='CAD'],'Canadian Dollar')
  checkEquals(result$unitValue[assetId=='CAD'],1)
  checkEquals(result$ICADCode[assetId=='CAD'],'CA-CASH')
  checkEquals(result$currency[assetId=='CAD'],'CAD')
  checkEquals(result$type[assetId=='CAD'],'CASH')
  
  checkTrue(is.element('JP1051271G37',assetId))
  checkEquals(result$name[assetId=='JP1051271G37'],'JAPAN (5 YEAR ISSUE)')
  checkEquals(result$unitValue[assetId=='JP1051271G37'],50000)
  checkEquals(result$ICADCode[assetId=='JP1051271G37'],'JP-JGB')
  checkEquals(result$currency[assetId=='JP1051271G37'],'JPY')
  checkEquals(result$type[assetId=='JP1051271G37'],'BOND')
  
  checkTrue(is.element('US46625H1005',assetId))
  checkEquals(result$name[assetId=='US46625H1005'],'JPMORGAN CHASE & CO')
  checkEquals(result$unitValue[assetId=='US46625H1005'],60.46)
  checkEquals(result$ICADCode[assetId=='US46625H1005'],'US-EQUITY-S&P500')
  checkEquals(result$currency[assetId=='US46625H1005'],'USD')
  checkEquals(result$type[assetId=='US46625H1005'],'EQUITY')
  
  checkTrue(is.element('US912796HW25',assetId))
  checkEquals(result$name[assetId=='US912796HW25'],'TREASURY BILL')
  checkEquals(result$unitValue[assetId=='US912796HW25'],100)
  checkEquals(result$ICADCode[assetId=='US912796HW25'],'US-TBILL')
  checkEquals(result$currency[assetId=='US912796HW25'],'USD')
  checkEquals(result$type[assetId=='US912796HW25'],'TBILL')
  
  checkTrue(is.element('US1912161007',assetId))
  checkEquals(result$name[assetId=='US1912161007'],'COCA-COLA CO/THE')
  checkEquals(result$unitValue[assetId=='US1912161007'],45.67)
  checkEquals(result$ICADCode[assetId=='US1912161007'],'US-EQUITY-S&P500')
  checkEquals(result$currency[assetId=='US1912161007'],'USD')
  checkEquals(result$type[assetId=='US1912161007'],'EQUITY')
  
  
  
}

testClientEliAssetDetailsByPassingAnEmptyClientId <- function(){
  
  # test input: an empty List of asset ids
  clientId <- emptyList
  
  # test function: eliAssetByClientId(clientId)
  result <- eliAssetByClientId(clientId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)
  
}

testClientEliAssetDetailsByPassingANonexistentClientId <- function(){
  # test input: a nonexistent cilent id
  clientId <- nonexistentClientId1
  
  # test function: eliAssetByClientId(clientId)
  result <- eliAssetByClientId(clientId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)
}

testClientEliAssetDetailsByPassingAExistentClientIdc1 <- function(){ 
  # test input: a existent client id
  clientId <- existentClientId1
  
  # test function: eliAssetByClientId(clientId)
  result <- eliAssetByClientId(clientId) # function output
  assetId <- result$assetId
  
  # result should has unitValue specified below
  checkTrue(is.element('USD',assetId))
  checkEquals(result$internalCost[which(assetId=='USD')],0.01)
  checkEquals(result$opptCost[which(assetId=='USD')],0.01)
  checkEquals(result$unitValue[which(assetId=='USD')],1)
  checkEquals(result$currency[which(assetId=='USD')],'USD')
  checkEquals(result$totalQuantity[which(assetId=='USD')],5000000)
  
  checkTrue(is.element('GBP',assetId))
  checkEquals(result$internalCost[which(assetId=='GBP')],0.006)
  checkEquals(result$opptCost[which(assetId=='GBP')],0.006)
  checkEquals(result$totalQuantity[which(assetId=='GBP')],5000000)
  
  checkTrue(is.element('JPY',assetId))
  checkEquals(result$internalCost[which(assetId=='JPY')],0.006)
  checkEquals(result$unitValue[which(assetId=='JPY')],1)
  checkEquals(result$currency[which(assetId=='JPY')],'JPY')
  checkEquals(result$totalQuantity[which(assetId=='JPY')],5000000)
  
  checkTrue(is.element('CAD',assetId))
  checkEquals(result$internalCost[which(assetId=='CAD')],0.01)
  checkEquals(result$currency[which(assetId=='CAD')],'CAD')
  checkEquals(result$totalQuantity[which(assetId=='CAD')],5000000)
  
  checkTrue(is.element('US46625H1005',assetId))
  checkEquals(result$internalCost[which(assetId=='US46625H1005')],0.001)
  checkEquals(result$unitValue[which(assetId=='US46625H1005')],60.46)
  checkEquals(result$totalQuantity[which(assetId=='US46625H1005')],1000)
  
  checkTrue(is.element('US912796JE09',assetId))
  checkEquals(result$internalCost[which(assetId=='US912796JE09')],0.002)
  checkEquals(result$opptCost[which(assetId=='US912796JE09')],0.002)
  checkEquals(result$unitValue[which(assetId=='US912796JE09')],100)
  checkEquals(result$currency[which(assetId=='US912796JE09')],'USD')
  checkEquals(result$totalQuantity[which(assetId=='US912796JE09')],10000)
  
}

testClientEliAssetDetailsByPassingAExistentClientIdc2 <- function(){
  # test input: a existent client id
  clientId <- existentClientId2
  
  # test function: eliAssetByClientId(clientId)
  result <- eliAssetByClientId(clientId) # function output
  assetId <- result$assetId
  
  # result should has unitValue specified below
  checkEquals(assetId,NULL)
}

testClientEliAssetDetailsByPassingMoreThanOneExistentClientIds <- function(){
  # test input: more than one client ids
  clientId <- c(existentClientId1,existentClientId2)
  
  # test function: eliAssetByClientId(clientId)
  result <- eliAssetByClientId(clientId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)
}

testClientAvailAssetDetailsByPassingAnEmptyListOfCallIdAndAExistentClientId<- function(){
  
  # test input: an empty List of call
  callId <- emptyList
  clientId <- existentClientId1
  
  # test function: availAssetByCallId(callId,clientId)
  result <- availAssetByCallIdAndClientId(callId,clientId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)
  
}

testClientAvailAssetDetailsByPassingAListOfExistentCallIdsAndAnEmptyClientId<- function(){
  
  # test input: an empty List of call
  callId <- existentCallIdToClient1Group1
  clientId <- emptyList
  
  # test function: availAssetByCallId(callId,clientId)
  result <- availAssetByCallIdAndClientId(callId,clientId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)
  
}

testClientAvailAssetDetailsByPassingANonexistentListOfCallIdsAndAExistentClientId <- function(){
  # test input: a nonexistent List of call id
  callId <- nonexistentCallIdGroup2
  clientId <- existentClientId1
  
  # test function: availAssetByCallId(callId,clientId)
  result <- availAssetByCallIdAndClientId(callId,clientId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)
}

testClientAvailAssetDetailsByPassingAExistentClientIdAndAListOfExistentCallIdsButNotDirectToTheClient <- function(){
  # test input: a nonexistent List of call ids but not direct to the client; a existent client id,
  callId <- existentCallIdToClient1Group1
  clientId <- existentClientId2
  
  # test function: availAssetByCallId(callId,clientId)
  result <- availAssetByCallIdAndClientId(callId,clientId) # function output
  
  # test ouput: result should be NULL
  checkEquals(result,NULL)
}

testClientAvailAssetDetailsByPassingAExistentClientIdAndAListOfExistentCallIdsWhichDirectToTheClient <- function(){ 
  # test input: a List of existent calls ids direct to the client; a existent client id
  callIdInput <- existentCallIdToClient1Group3
  clientId <- existentClientId1
  
  # test function: availAssetByCallIdAndClientId(callId,clientId)
  result <- availAssetByCallIdAndClientId(callIdInput,clientId) # function output
  callIdOutput <- result$callId
  assetId <- result$assetId
  custAccount<- result$CustodianAccount
  
  # result should has unitValue specified below
  
  # check whether all input call ids have been retrieved
  checkTrue(setequal(callIdOutput,callIdInput))
  callId <- callIdOutput
  
  # check whether all available assets and custodian accounts for each margin call are returned
  checkTrue(setequal(result$assetId[callId=='mc2'],c("EUR","GBP","USD","JPY","SGD","HKD","CAD","AUD")))
  checkTrue(setequal(result$assetId[callId=='mc4'],c("HKD","CAD","AUD")))
  checkTrue(setequal(result$assetId[callId=='mc5'],c("EUR","GBP","USD","JPY","SGD")))
  checkTrue(setequal(toupper(result$assetId[callId=='mc8']),c("EUR","GBP","USD","JPY","SGD","HKD","CAD","AUD")))
  checkTrue(setequal(result$assetId[callId=='mc9'],c("EUR","GBP","USD","JPY","SGD","HKD","CAD","AUD")))
  checkTrue(setequal(result$assetId[callId=='mc10'],c("HKD","CAD","AUD")))
  checkTrue(setequal(result$assetId[callId=='mc11'],c("HKD","CAD","AUD")))
  checkTrue(setequal(result$assetId[callId=='mc12'],c("FR0000570947","US46625H1005","US38141G1040","SG7Y76964295","SG7S29941612","SG7U33949433","XS1045278410","SG3260987684",
                                                      "FR0012634558","GB00B4YRFP41","XS1378790262","FR0000570905","FR0120746609","FR0013101466","DE0001104636","DE0001141737",
                                                      "DE0001119584","JP1023481F17","JP1023561F93","JP1023621G33","JP1051041C55","JP1051161DC4","JP1051271G37","JP1103121AC2",
                                                      "CAD","US912796JE09","HKD","AUD","EUR","GBP","USD","JPY","SGD","US5801351017","US4592001014","US3696041033","US30231G1022")))
  checkTrue(setequal(result$assetId[callId=='mc16'],c("USD","US38141G1040","US3696041033","US46625H1005","EUR","GBP","JPY","SGD","US5801351017","US30231G1022","US4592001014")))
  
  checkTrue(setequal(result$CustodianAccount[callId=='mc4'],c('custac5')))
  checkTrue(setequal(result$CustodianAccount[callId=='mc5'],c('custac4')))
  checkTrue(setequal(result$CustodianAccount[callId=='mc8'],c('custac5','custac4')))
  checkTrue(setequal(result$CustodianAccount[callId=='mc12'],c('custac1','custac2','custac3','custac5','custac4')))
  checkTrue(setequal(result$CustodianAccount[callId=='mc16'],c('custac3','custac4')))
  
  # check whether the same asset from the same custodian account 
  # and available for different margin calls has the same totalQuantity and other properties
  checkTrue(setequal(result$totalQuantity[assetId=='USD' & custAccount=='custac4'],5000000))
  checkTrue(setequal(result$internalCost[assetId=='USD' & custAccount=='custac4'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='USD' & custAccount=='custac4'],0.01))
  checkTrue(setequal(result$currency[assetId=='USD' & custAccount=='custac4'],'USD'))
  
  checkTrue(setequal(result$totalQuantity[assetId=='GBP' & custAccount=='custac4'],5000000))
  checkTrue(setequal(result$internalCost[assetId=='GBP' & custAccount=='custac4'],0.006))
  checkTrue(setequal(result$opptCost[assetId=='GBP' & custAccount=='custac4'],0.006))
  checkTrue(setequal(result$currency[assetId=='GBP' & custAccount=='custac4'],'GBP'))
  
  checkTrue(setequal(result$totalQuantity[assetId=='JPY' & custAccount=='custac4'],5000000))
  checkTrue(setequal(result$internalCost[assetId=='JPY' & custAccount=='custac4'],0.006))
  checkTrue(setequal(result$opptCost[assetId=='JPY' & custAccount=='custac4'],0.006))
  checkTrue(setequal(result$currency[assetId=='JPY' & custAccount=='custac4'],'JPY'))
  
  checkTrue(setequal(result$totalQuantity[assetId=='CAD' & custAccount=='custac5'],5000000))
  checkTrue(setequal(result$internalCost[assetId=='CAD' & custAccount=='custac5'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='CAD' & custAccount=='custac5'],0.01))
  checkTrue(setequal(result$currency[assetId=='CAD' & custAccount=='custac5'],'CAD'))
  
  checkTrue(setequal(result$totalQuantity[assetId=='SGD' & custAccount=='custac4'],5000000))
  checkTrue(setequal(result$internalCost[assetId=='SGD' & custAccount=='custac4'],0.006))
  checkTrue(setequal(result$opptCost[assetId=='SGD' & custAccount=='custac4'],0.006))
  checkTrue(setequal(result$currency[assetId=='SGD' & custAccount=='custac4'],'SGD'))
  
  checkTrue(setequal(result$totalQuantity[assetId=='AUD' & custAccount=='custac5'],5000000))
  checkTrue(setequal(result$internalCost[assetId=='AUD' & custAccount=='custac5'],0.018))
  checkTrue(setequal(result$opptCost[assetId=='AUD' & custAccount=='custac5'],0.018))
  checkTrue(setequal(result$currency[assetId=='AUD' & custAccount=='custac5'],'AUD'))
  
  checkTrue(setequal(result$totalQuantity[assetId=='EUR' & custAccount=='custac4'],5000000))
  checkTrue(setequal(result$internalCost[assetId=='EUR' & custAccount=='custac4'],0.004))
  checkTrue(setequal(result$opptCost[assetId=='EUR' & custAccount=='custac4'],0.004))
  checkTrue(setequal(result$currency[assetId=='EUR' & custAccount=='custac4'],'EUR'))
  
}





