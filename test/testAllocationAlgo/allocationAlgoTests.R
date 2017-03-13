source('src/allocationFunction.R')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS INPUT FOR TESTING, PLEASE DO NOT CHANGE ############
emptyList <- c()

nonexistentClientId1 <- 'c001'
existentClientId1 <- '999'

nonexistentCallIdsGroup1 <- c('asdd','null','adhajk')
existentCallIdsToClient1Group1 <- c('mcp1')
existentCallIdsToClient1Group2 <- c('mcp1','mcp5','mcp7')
existentCallIdsToClient1Group3 <- c('mcp1','mcp5','mcp38','mcp50')

prefForCostOnly <- c(0,0,10)
prefForLiquidityOnly <- c(0,10,0)
prefForOperationOnly <- c(10,0,0)
prefRandom1 <- c(6,3,9)
prefRandom2 <- c(3,8,10)

call.limit1 <- c(7,7,7)
call.limit2 <- c(7,7,30)

time.limit1 <- 1
time.limit2 <- 3
time.limit3 <- 10

# the real data from database
tempGetInputROri <- function(clientId,callIds){
  
  # get info #
  callInfo <- callInfoByCallId(callIds)
  availAssets <- availAssetByCallIdAndClientId(callIds,clientId) # available asset for the margin call
  
  asset.custac.id <- paste(availAssets$assetId,availAssets$CustodianAccount,sep='-')
  availAssets$assetCustacId <- asset.custac.id
  assetCustacIds <- unique(asset.custac.id)
  
  assetIds <- as.character(data.frame(strsplit(assetCustacIds,'-'))[1,])
  assetInfo <- assetInfoByAssetId(assetIds)
  assetInfo <- assetInfo[match(assetIds,assetInfo$id),]
  
  return(list(callIds=callIds,assetCustacIds=assetCustacIds,callInfo=callInfo,availAssets=availAssets,
              assetInfo=assetInfo))
}

# change the asset quantity, add custodianAccountTest
tempGetInputROW374 <- function(clientId,callIds){
  
  # get info #
  callInfo <- callInfoByCallId(callIds)
  availAssets <- availAssetByCallIdAndClientId(callIds,clientId) # available asset for the margin call
  
  # change quantity for testing
  availAssets$quantity <- availAssets$quantity/5
  # add custodianAccount for testing
  availAssets <- rbind(availAssets,availAssets)
  availAssets$CustodianAccount[1:length(availAssets[,1])/2] <- 'custodianAccountTest'
  
  asset.custac.id <- paste(availAssets$assetId,availAssets$CustodianAccount,sep='-')
  availAssets$assetCustacId <- asset.custac.id
  assetCustacIds <- unique(asset.custac.id)
  
  assetIds <- as.character(data.frame(strsplit(assetCustacIds,'-'))[1,])
  assetInfo <- assetInfoByAssetId(assetIds)
  assetInfo <- assetInfo[match(assetIds,assetInfo$id),]
  
  return(list(callIds=callIds,assetCustacIds=assetCustacIds,callInfo=callInfo,availAssets=availAssets,
              assetInfo=assetInfo))
}

###### TEST FUNCTIONS ##############################################
testCostOnlyAllocationAlgoInputROriExistentClientIdExistentCallIdsGroup2<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  
  callIds <- existentCallIdsToClient1Group2
  clientId <- existentClientId1
  pref <- prefForCostOnly
  call.limit <- call.limit1
  time.limit <- time.limit2
  
  input.list <- tempGetInputROri(clientId,callIds)
  callIds<-input.list$callIds;assetCustacIds<-input.list$assetCustacIds;callInfo<-input.list$callInfo;
  availAssets<-input.list$availAssets;  assetInfo<-input.list$assetInfo
  
  # test function: allocationAlgo(...)
  algoOutput <-allocationAlgo(callIds,assetCustacIds,callInfo,availAssets,assetInfo,pref,time.limit,call.limit)
  result <- algoOutput$output

  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$`NetAmount(USD)`,2),18000)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$NetAmount,2),14040)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$`Amount(USD)`,2),18000)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$Amount,2),14040)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$Quantity,2),14040)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$FXRate,0.78)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$CustodianAccount,'CustodianAccount1D')
  
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$`NetAmount(USD)`,2),21500)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$NetAmount,2),16770)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$`Amount(USD)`,2),21500)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$Amount,2),16770)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$Quantity,2),16770)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$FXRate,0.78)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$CustodianAccount,'CustodianAccount1D')
  
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$`NetAmount(USD)`,2),30000)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$NetAmount,2),23400)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$`Amount(USD)`,2),30000)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$Amount,2),23400)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$Quantity,2),23400)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$FXRate,0.78)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$CustodianAccount,'CustodianAccount1D')
}

testPrefRandom1AllocationAlgoInputROriExistentClientIdExistentCallIdsGroup2<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callIds <- existentCallIdsToClient1Group2
  clientId <- existentClientId1
  pref <- prefRandom1
  call.limit <- call.limit1
  time.limit <- time.limit2
  
  input.list <- tempGetInputROri(clientId,callIds)
  callIds <-input.list$callIds; assetCustacIds<-input.list$assetCustacIds; callInfo<-input.list$callInfo;
  availAssets<-input.list$availAssets;  assetInfo <-input.list$assetInfo
 
  # test function: allocationAlgo(callId,clientId,pref)
  algoOutput <-allocationAlgo(callIds,assetCustacIds,callInfo,availAssets,assetInfo,pref,time.limit,call.limit)
  result <- algoOutput$output
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Asset,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Name,'US Dollar')
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$`NetAmount(USD)`,2),19200)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$NetAmount,2),19200)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$`Amount(USD)`,2),19200)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$Amount,2),19200)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$Quantity,2),19200)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Currency,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$FXRate,1)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$marginType,'Variation')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$CustodianAccount,'CustodianAccount1D')
  
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Asset,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Name,'US Dollar')
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$`NetAmount(USD)`,2),22933.34)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$NetAmount,2),22933.34)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$`Amount(USD)`,2),22933.34)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$Amount,2),22933.34)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$Quantity,2),22933.34)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Currency,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$FXRate,1)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$marginType,'Variation')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$CustodianAccount,'CustodianAccount1D')
  
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Asset,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Name,'US Dollar')
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$`NetAmount(USD)`,2),32000)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$NetAmount,2),32000)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$`Amount(USD)`,2),32000)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$Amount,2),32000)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$Quantity,2),32000)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Currency,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$FXRate,1)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$marginType,'Variation')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$CustodianAccount,'CustodianAccount1D')
}

testPrefRandom1AllocationAlgoInputROW374ExistentClientIdExistentCallIdsGroup3<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callIds <- existentCallIdsToClient1Group3
  clientId <- existentClientId1
  pref <- prefRandom1
  call.limit <- call.limit1
  time.limit <- time.limit2
  
  input.list <- tempGetInputROW374(clientId,callIds)
  callIds <-input.list$callIds; assetCustacIds<-input.list$assetCustacIds; callInfo<-input.list$callInfo;
  availAssets<-input.list$availAssets;  assetInfo <-input.list$assetInfo
  
  # test function: allocationAlgo(callId,clientId,pref)
  algoOutput <-allocationAlgo(callIds,assetCustacIds,callInfo,availAssets,assetInfo,pref,time.limit,call.limit)
  result <- algoOutput$output
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  checkEquals(result[[existentCallIdsToClient1Group3[1]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group3[1]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdsToClient1Group3[1]]]$`NetAmount(USD)`,2),19200)
  checkEquals(round(result[[existentCallIdsToClient1Group3[1]]]$NetAmount,2),14976)
  checkEquals(round(result[[existentCallIdsToClient1Group3[1]]]$`Amount(USD)`,2),19200)
  checkEquals(round(result[[existentCallIdsToClient1Group3[1]]]$Amount,2),14976)
  checkEquals(round(result[[existentCallIdsToClient1Group3[1]]]$Quantity,2),14976)
  checkEquals(result[[existentCallIdsToClient1Group3[1]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group3[1]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group3[1]]]$FXRate,0.78)
  checkEquals(result[[existentCallIdsToClient1Group3[1]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group3[1]]]$marginType,'Variation')
  checkEquals(result[[existentCallIdsToClient1Group3[1]]]$CustodianAccount,'custodianAccountTest')
  
  
  checkEquals(result[[existentCallIdsToClient1Group3[2]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group3[2]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdsToClient1Group3[2]]]$`NetAmount(USD)`,2),22933.33)
  checkEquals(round(result[[existentCallIdsToClient1Group3[2]]]$NetAmount,2),17888)
  checkEquals(round(result[[existentCallIdsToClient1Group3[2]]]$`Amount(USD)`,2),22933.33)
  checkEquals(round(result[[existentCallIdsToClient1Group3[2]]]$Amount,2),17888)
  checkEquals(round(result[[existentCallIdsToClient1Group3[2]]]$Quantity,2),17888)
  checkEquals(result[[existentCallIdsToClient1Group3[2]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group3[2]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group3[2]]]$FXRate,0.78)
  checkEquals(result[[existentCallIdsToClient1Group3[2]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group3[2]]]$marginType,'Variation')
  checkEquals(result[[existentCallIdsToClient1Group3[2]]]$CustodianAccount,'custodianAccountTest')
  
  
  checkEquals(result[[existentCallIdsToClient1Group3[3]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group3[3]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdsToClient1Group3[3]]]$`NetAmount(USD)`,2),85542.41)
  checkEquals(round(result[[existentCallIdsToClient1Group3[3]]]$NetAmount,2),66723.08)
  checkEquals(round(result[[existentCallIdsToClient1Group3[3]]]$`Amount(USD)`,2),85542.41)
  checkEquals(round(result[[existentCallIdsToClient1Group3[3]]]$Amount,2),66723.08)
  checkEquals(round(result[[existentCallIdsToClient1Group3[3]]]$Quantity,2),66723.08)
  checkEquals(result[[existentCallIdsToClient1Group3[3]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group3[3]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group3[3]]]$FXRate,0.78)
  checkEquals(result[[existentCallIdsToClient1Group3[3]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group3[3]]]$marginType,'Variation')
  checkEquals(result[[existentCallIdsToClient1Group3[3]]]$CustodianAccount,'custodianAccountTest')
  
  checkEquals(result[[existentCallIdsToClient1Group3[4]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group3[4]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdsToClient1Group3[4]]]$`NetAmount(USD)`,2),1407436.81)
  checkEquals(round(result[[existentCallIdsToClient1Group3[4]]]$NetAmount,2),1097800.71)
  checkEquals(round(result[[existentCallIdsToClient1Group3[4]]]$`Amount(USD)`,2),1407436.81)
  checkEquals(round(result[[existentCallIdsToClient1Group3[4]]]$Amount,2),1097800.71)
  checkEquals(round(result[[existentCallIdsToClient1Group3[4]]]$Quantity,2),1097800.71)
  checkEquals(result[[existentCallIdsToClient1Group3[4]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group3[4]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group3[4]]]$FXRate,0.78)
  checkEquals(result[[existentCallIdsToClient1Group3[4]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group3[4]]]$marginType,'Initial')
  checkEquals(result[[existentCallIdsToClient1Group3[4]]]$CustodianAccount,'custodianAccountTest')
}
