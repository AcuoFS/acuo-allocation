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
existentCallIdToClient1Group3 <- c("mc1","mc2","mc3","mc4","mc5","mc6","mc7","mc8","mc9","mc10","mc11")

nonexistentAssetIdGroup1 <- 'hdilasdij'
nonexistentAssetIdGroup2 <- c('adsa','adil','23er','adhl')
existentAssetIdGroup1<- c('USD','CAD')
existentAssetIdGroup2<- c("US912796HW25","37833100","GBP","USD","46625H100","CAD","JPY","Gold")

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
  
  # test output: result should has value specified below
  
  # check whether all call ids have been retrieved
  checkTrue(setequal(callIdOutput,callIdInput))
  callId <- callIdOutput
  
  # check call details
  checkTrue(is.element('mc1',callId))
  checkEquals(result$callAmount[callId=='mc1'],10000)
  
  checkTrue(is.element('mc2',callId))
  checkEquals(result$callAmount[callId=='mc2'],15000)
  
  checkTrue(is.element('mc3',callId))
  checkEquals(result$callAmount[callId=='mc3'],20000)
  
  checkTrue(is.element('mc4',callId))
  checkEquals(result$callAmount[callId=='mc4'],250)
  
  checkTrue(is.element('mc5',callId))
  checkEquals(result$callAmount[callId=='mc5'],30000)
  
  checkTrue(is.element('mc6',callId))
  checkEquals(result$callAmount[callId=='mc6'],15000)
  
  checkTrue(is.element('mc7',callId))
  checkEquals(result$callAmount[callId=='mc7'],5000)
  
  checkTrue(is.element('mc8',callId))
  checkEquals(result$callAmount[callId=='mc8'],10000)
  
  checkTrue(is.element('mc9',callId))
  checkEquals(result$callAmount[callId=='mc9'],10000)
  
  checkTrue(is.element('mc10',callId))
  checkEquals(result$callAmount[callId=='mc10'],10000)
  
  checkTrue(is.element('mc11',callId))
  checkEquals(result$callAmount[callId=='mc11'],10000)
  
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
  
  # test output: result should has value specified below
  
  # check whether all asset ids have been retrieved
  checkTrue(setequal(assetIdOutput,assetIdInput))
  assetId <- assetIdOutput
  
  # check asset details
  checkTrue(is.element('USD',assetId))
  checkEquals(result$name[assetId=='USD'],'US dollar')
  checkEquals(result$value[assetId=='USD'],1)
  checkEquals(result$isdaType[assetId=='USD'],'Cash')
  checkEquals(result$currency[assetId=='USD'],'USD')
  checkEquals(result$type[assetId=='USD'],'Cash')
  
  checkTrue(is.element('GBP',assetId))
  checkEquals(result$name[assetId=='GBP'],'British Pound')
  checkEquals(result$value[assetId=='GBP'],1)
  checkEquals(result$isdaType[assetId=='GBP'],'Cash')
  checkEquals(result$currency[assetId=='GBP'],'GBP')
  checkEquals(result$type[assetId=='GBP'],'Cash')
  
  checkTrue(is.element('JPY',assetId))
  checkEquals(result$name[assetId=='JPY'],'Japanese yen')
  checkEquals(result$value[assetId=='JPY'],1)
  checkEquals(result$isdaType[assetId=='JPY'],'Cash')
  checkEquals(result$currency[assetId=='JPY'],'JPY')
  checkEquals(result$type[assetId=='JPY'],'Cash')
  
  checkTrue(is.element('CAD',assetId))
  checkEquals(result$name[assetId=='CAD'],'Canadian dollar')
  checkEquals(result$value[assetId=='CAD'],1)
  checkEquals(result$isdaType[assetId=='CAD'],'Cash')
  checkEquals(result$currency[assetId=='CAD'],'CAD')
  checkEquals(result$type[assetId=='CAD'],'Cash')
  
  checkTrue(is.element('37833100',assetId))
  checkEquals(result$name[assetId=='37833100'],'APPL INC')
  checkEquals(result$value[assetId=='37833100'],117)
  checkEquals(result$isdaType[assetId=='37833100'],'Corporate Equity')
  checkEquals(result$currency[assetId=='37833100'],'USD')
  checkEquals(result$type[assetId=='37833100'],'Equity')
  
  checkTrue(is.element('46625H100',assetId))
  checkEquals(result$name[assetId=='46625H100'],'JPM US Equity')
  checkEquals(result$value[assetId=='46625H100'],65)
  checkEquals(result$isdaType[assetId=='46625H100'],'Corporate Equity')
  checkEquals(result$currency[assetId=='46625H100'],'USD')
  checkEquals(result$type[assetId=='46625H100'],'Equity')
  
  checkTrue(is.element('US912796HW25',assetId))
  checkEquals(result$name[assetId=='US912796HW25'],'U.S. Treasury Bills')
  checkEquals(result$value[assetId=='US912796HW25'],100)
  checkEquals(result$isdaType[assetId=='US912796HW25'],'MM Instruments')
  checkEquals(result$currency[assetId=='US912796HW25'],'USD')
  checkEquals(result$type[assetId=='US912796HW25'],'Tbill')
  
  checkTrue(is.element('Gold',assetId))
  checkEquals(result$name[assetId=='Gold'],'Gold')
  checkEquals(result$value[assetId=='Gold'],10)
  checkEquals(result$isdaType[assetId=='Gold'],'Gold')
  checkEquals(result$currency[assetId=='Gold'],'GLD')
  checkEquals(result$type[assetId=='Gold'],'Gold')
  
  
  
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
  
  # result should has value specified below
  checkTrue(is.element('USD',assetId))
  checkEquals(result$internalCost[which(assetId=='USD')],0.03)
  checkEquals(result$opptCost[which(assetId=='USD')],0.04)
  checkEquals(result$value[which(assetId=='USD')],1)
  checkEquals(result$currency[which(assetId=='USD')],'USD')
  checkEquals(result$quantity[which(assetId=='USD')],20000000)
  checkEquals(result$availableQuantities[which(assetId=='USD')],20000000)
  
  checkTrue(is.element('GBP',assetId))
  checkEquals(result$internalCost[which(assetId=='GBP')],0.01)
  checkEquals(result$opptCost[which(assetId=='GBP')],0.05)
  checkEquals(result$quantity[which(assetId=='GBP')],100000)
  checkEquals(result$availableQuantities[which(assetId=='GBP')],100000)
  
  checkTrue(is.element('JPY',assetId))
  checkEquals(result$internalCost[which(assetId=='JPY')],0.01)
  checkEquals(result$value[which(assetId=='JPY')],1)
  checkEquals(result$currency[which(assetId=='JPY')],'JPY')
  checkEquals(result$quantity[which(assetId=='JPY')],5000000)
  checkEquals(result$availableQuantities[which(assetId=='JPY')],5000000)
  
  checkTrue(is.element('CAD',assetId))
  checkEquals(result$internalCost[which(assetId=='CAD')],0.01)
  checkEquals(result$currency[which(assetId=='CAD')],'CAD')
  checkEquals(result$quantity[which(assetId=='CAD')],40000000)
  checkEquals(result$availableQuantities[which(assetId=='CAD')],40000000)
  
  checkTrue(is.element('46625H100',assetId))
  checkEquals(result$internalCost[which(assetId=='46625H100')],0.02)
  checkEquals(result$value[which(assetId=='46625H100')],65)
  checkEquals(result$quantity[which(assetId=='46625H100')],200000)
  checkEquals(result$availableQuantities[which(assetId=='46625H100')],200000)
  
  checkTrue(is.element('37833100',assetId))
  checkEquals(result$internalCost[which(assetId=='37833100')],0.02)
  checkEquals(result$opptCost[which(assetId=='37833100')],0.02)
  checkEquals(result$quantity[which(assetId=='37833100')],500000)
  checkEquals(result$availableQuantities[which(assetId=='37833100')],500000)
  
  checkTrue(is.element('US912796HW25',assetId))
  checkEquals(result$internalCost[which(assetId=='US912796HW25')],0.03)
  checkEquals(result$opptCost[which(assetId=='US912796HW25')],0.01)
  checkEquals(result$value[which(assetId=='US912796HW25')],100)
  checkEquals(result$currency[which(assetId=='US912796HW25')],'USD')
  checkEquals(result$quantity[which(assetId=='US912796HW25')],500000)
  checkEquals(result$availableQuantities[which(assetId=='US912796HW25')],500000)
  
}

testClientEliAssetDetailsByPassingAExistentClientIdc2 <- function(){
  # test input: a existent client id
  clientId <- existentClientId2
  
  # test function: eliAssetByClientId(clientId)
  result <- eliAssetByClientId(clientId) # function output
  assetId <- result$assetId
  
  # result should has value specified below
  checkTrue(is.element('USD',assetId))
  checkEquals(result$internalCost[which(assetId=='USD')],0.03)
  checkEquals(result$opptCost[which(assetId=='USD')],0.04)
  checkEquals(result$value[which(assetId=='USD')],1)
  checkEquals(result$currency[which(assetId=='USD')],'USD')
  checkEquals(result$quantity[which(assetId=='USD')],3000000)
  checkEquals(result$availableQuantities[which(assetId=='USD')],3000000)
  
  checkTrue(is.element('Gold',assetId))
  checkEquals(result$internalCost[which(assetId=='Gold')],0.03)
  checkEquals(result$opptCost[which(assetId=='Gold')],0.01)
  checkEquals(result$currency[which(assetId=='Gold')],'GLD')
  checkEquals(result$value[which(assetId=='Gold')],10)
  checkEquals(result$quantity[which(assetId=='Gold')],500000)
  checkEquals(result$availableQuantities[which(assetId=='Gold')],500000)
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
  
  # result should has value specified below
  
  # check whether all input call ids have been retrieved
  checkTrue(setequal(callIdOutput,callIdInput))
  callId <- callIdOutput
  
  # check whether all available assets and custodian accounts for each margin call are returned
  checkTrue(setequal(result$assetId[callId=='mc2'],c('USD','GBP','JPY','CAD')))
  checkTrue(setequal(result$assetId[callId=='mc4'],c('USD','GBP','JPY')))
  checkTrue(setequal(result$assetId[callId=='mc5'],c('37833100','46625H100')))
  checkTrue(setequal(result$assetId[callId=='mc6'],c('USD','GBP','JPY','CAD')))
  checkTrue(setequal(toupper(result$assetId[callId=='mc8']),c('USD')))
  checkTrue(setequal(result$assetId[callId=='mc9'],c('USD')))
  checkTrue(setequal(result$assetId[callId=='mc10'],c('USD')))
  checkTrue(setequal(result$assetId[callId=='mc11'],c('USD')))
  
  checkTrue(setequal(result$CustodianAccount[callId=='mc4'],c('custac2','custac5','custac1')))
  checkTrue(setequal(result$CustodianAccount[callId=='mc5'],c('custac3','custac4')))
  checkTrue(setequal(result$CustodianAccount[callId=='mc8'],c('custac5')))
  
  # check whether the same asset from the same custodian account 
  # and available for different margin calls has the same quantity and other properties
  checkTrue(setequal(result$quantity[assetId=='USD' & custAccount=='custac5'],20000000))
  checkTrue(setequal(result$internalCost[assetId=='USD' & custAccount=='custac5'],0.03))
  checkTrue(setequal(result$opptCost[assetId=='USD' & custAccount=='custac5'],0.04))
  checkTrue(setequal(result$currency[assetId=='USD' & custAccount=='custac5'],'USD'))
  
  checkTrue(setequal(result$quantity[assetId=='GBP' & custAccount=='custac1'],50000))
  checkTrue(setequal(result$internalCost[assetId=='GBP' & custAccount=='custac1'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='GBP' & custAccount=='custac1'],0.05))
  checkTrue(setequal(result$currency[assetId=='GBP' & custAccount=='custac1'],'GBP'))
  checkTrue(setequal(result$quantity[assetId=='GBP' & custAccount=='custac2'],50000))
  checkTrue(setequal(result$internalCost[assetId=='GBP' & custAccount=='custac2'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='GBP' & custAccount=='custac2'],0.05))
  checkTrue(setequal(result$currency[assetId=='GBP' & custAccount=='custac2'],'GBP'))
  
  checkTrue(setequal(result$quantity[assetId=='JPY' & custAccount=='custac5'],5000000))
  checkTrue(setequal(result$internalCost[assetId=='JPY' & custAccount=='custac5'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='JPY' & custAccount=='custac5'],0.02))
  checkTrue(setequal(result$currency[assetId=='JPY' & custAccount=='custac5'],'JPY'))
  
  checkTrue(setequal(result$quantity[assetId=='CAD' & custAccount=='custac6'],40000000))
  checkTrue(setequal(result$internalCost[assetId=='CAD' & custAccount=='custac6'],0.01))
  checkTrue(setequal(result$opptCost[assetId=='CAD' & custAccount=='custac6'],0.01))
  checkTrue(setequal(result$currency[assetId=='CAD' & custAccount=='custac6'],'CAD'))
  
  checkTrue(setequal(result$quantity[assetId=='46625H100' & custAccount=='custac4'],200000))
  checkTrue(setequal(result$internalCost[assetId=='46625H100' & custAccount=='custac4'],0.02))
  checkTrue(setequal(result$opptCost[assetId=='46625H100' & custAccount=='custac4'],0.02))
  checkTrue(setequal(result$currency[assetId=='46625H100' & custAccount=='custac4'],'USD'))
  
  checkTrue(setequal(result$quantity[assetId=='37833100' & custAccount=='custac3'],500000))
  checkTrue(setequal(result$internalCost[assetId=='37833100' & custAccount=='custac3'],0.02))
  checkTrue(setequal(result$opptCost[assetId=='37833100' & custAccount=='custac3'],0.02))
  checkTrue(setequal(result$currency[assetId=='37833100' & custAccount=='custac3'],'USD'))
  
  checkTrue(setequal(result$quantity[assetId=='US912796HW25' & custAccount=='custac1'],500000))
  checkTrue(setequal(result$internalCost[assetId=='US912796HW25' & custAccount=='custac1'],0.03))
  checkTrue(setequal(result$opptCost[assetId=='US912796HW25' & custAccount=='custac1'],0.01))
  checkTrue(setequal(result$currency[assetId=='US912796HW25' & custAccount=='custac1'],'USD'))
  
}




