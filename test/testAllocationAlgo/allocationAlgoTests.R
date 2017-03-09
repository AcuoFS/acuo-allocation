source('src/allocationFunction.R')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS INPUT FOR TESTING, PLEASE DO NOT CHANGE ############
emptyList <- c()

nonexistentClientId1 <- 'c001'
existentClientId1 <- '999'

nonexistentCallIdsGroup1 <- c('asdd','null','adhajk')
existentCallIdsToClient1Group1 <- c('mcp1')
existentCallIdsToClient1Group2 <- c('mcp1','mcp5','mcp7')
existentCallIdsToClient1Group3 <- c('mc1','mc2','mc3','mc4','mc5','mc8')
existentCallIdsToClient1Group4 <- c('mc4','mc8','mc12','mc13','mc16')
existentCallIdsToClient1Group5 <- c('mc2','mc9','mc10','mc11','mc14','mc15','mc17','mc19','mc20')

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

tempGetInputR <- function(clientId,callIds){
  
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

###### TEST FUNCTIONS ##############################################
testCostOnlyAllocationAlgoByPassingAExistentClientIdAndAListOfExistentCallIdsGroup2<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  
  callIds <- existentCallIdsToClient1Group2
  clientId <- existentClientId1
  pref <- prefForCostOnly
  call.limit <- call.limit1
  time.limit <- time.limit2
  
  input.list <- tempGetInputR(clientId,callIds)
  callIds<-input.list$callIds;assetCustacIds<-input.list$assetCustacIds;callInfo<-input.list$callInfo;
  availAssets<-input.list$availAssets;  assetInfo<-input.list$assetInfo

  # test function: allocationAlgo(...)
  algoOutput <-allocationAlgo(callIds,assetCustacIds,callInfo,availAssets,assetInfo,pref,time.limit,call.limit)
  result <- algoOutput$output

  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$`NetAmount(USD)`,2),18309)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$NetAmount,2),14281.02)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$`Amount(USD)`,2),18309)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$Amount,2),14281.02)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$Quantity,2),14281.02)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$FXRate,0.78)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$CustodianAccount,'CustodianAccount1D')
  
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$`NetAmount(USD)`,2),21665)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$NetAmount,2),16898.7)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$`Amount(USD)`,2),21665)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$Amount,2),16898.7)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$Quantity,2),16898.7)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$FXRate,0.78)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$CustodianAccount,'CustodianAccount1D')
  
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Asset,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Name,'British Pound')
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$`NetAmount(USD)`,2),30480)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$NetAmount,2),23774.4)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$`Amount(USD)`,2),30480)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$Amount,2),23774.4)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$Quantity,2),23774.4)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Currency,'GBP')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$FXRate,0.78)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$CustodianAccount,'CustodianAccount1D')
}

testPrefRandom1AllocationAlgoByPassingAndAnExistentClientIdAndAListOfExistentCallIdsGroup2<-function(){
  # test input: a existent client id; a list of existent margin call ids, which direct to the client
  callIds <- existentCallIdsToClient1Group2
  clientId <- existentClientId1
  pref <- prefRandom1
  call.limit <- call.limit1
  time.limit <- time.limit2
  
  input.list <- tempGetInputR(clientId,callIds)
  callIds <-input.list$callIds; assetCustacIds<-input.list$assetCustacIds; callInfo<-input.list$callInfo;
  availAssets<-input.list$availAssets;  assetInfo <-input.list$assetInfo
 
  # test function: allocationAlgo(callId,clientId,pref)
  algoOutput <-allocationAlgo(callIds,assetCustacIds,callInfo,availAssets,assetInfo,pref,time.limit,call.limit)
  result <- algoOutput$output
  
  # test output:
  # check whether each margin call has been fulfilled with the correct asset and amount
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Asset,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Name,'US Dollar')
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$`NetAmount(USD)`,2),19529.61)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$NetAmount,2),19529.61)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$`Amount(USD)`,2),19529.61)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$Amount,2),19529.61)
  checkEquals(round(result[[existentCallIdsToClient1Group2[1]]]$Quantity,2),19529.61)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$Currency,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$FXRate,1)
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$marginType,'Variation')
  checkEquals(result[[existentCallIdsToClient1Group2[1]]]$CustodianAccount,'CustodianAccount1D')
  
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Asset,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Name,'US Dollar')
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$`NetAmount(USD)`,2),23109.34)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$NetAmount,2),23109.34)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$`Amount(USD)`,2),23109.34)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$Amount,2),23109.34)
  checkEquals(round(result[[existentCallIdsToClient1Group2[2]]]$Quantity,2),23109.34)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$Currency,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$FXRate,1)
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$marginType,'Variation')
  checkEquals(result[[existentCallIdsToClient1Group2[2]]]$CustodianAccount,'CustodianAccount1D')
  
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Asset,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Name,'US Dollar')
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$`NetAmount(USD)`,2),32512)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$NetAmount,2),32512)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$`Amount(USD)`,2),32512)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$Amount,2),32512)
  checkEquals(round(result[[existentCallIdsToClient1Group2[3]]]$Quantity,2),32512)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Haircut,0)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$Currency,'USD')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$FXRate,1)
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$venue,'SG')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$marginType,'Variation')
  checkEquals(result[[existentCallIdsToClient1Group2[3]]]$CustodianAccount,'CustodianAccount1D')
}

