
########### FUNCTION INPUT FROM JAVA LAYER Start #######################
# 1. callId_vec
# 2. clientId
# 3. uesr preference: 
#   3.1. objective vector: pref_vec
#   3.2. operation limit
#   3.2.1 movements limit per margin statement: operLimitMs
#   3.2.2 whether the limit on the ms is fungible: fungible
# 4. callInfo_df <- callInfoByCallId(callId_vec)
# 5. availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId)
# 6. resource_vec <- unique(paste(availAsset_df$assetId,availAsset_df$CustodianAccount,sep='-'))
# 7. assetId_vec <- assetId_vec <- matrix(unlist(strsplit(resource_vec,'-')),nrow=2)[1,]
# 8. assetInfo_df <- assetInfoByAssetId(assetId_vec)
########### FUNCTION INPUT FROM JAVA LAYER END ##########################


#### Sources Start #########
setwd("E:/ACUO/projects/acuo-allocation/")
source('src/functionsOfDBRequestByExecutingCypher.R')
source("src/allocationScenarios.R")
source("src/allocationByGroup.R")
source("src/coreAlgo.R")
source("src/allocationApproaches.R")
source("src/generalFunctions/infoConstruction.R")
source("src/generalFunctions/nameConstruction.R")
source("src/generalFunctions/modelConstruction.R")
source("src/generalFunctions/assetSufficiency.R")
source("src/generalFunctions/objectiveParameters.R")
source("src/generalFunctions/resultFormatConversion.R")
source("src/generalFunctions/resourceUpdate.R")
source("src/generalFunctions/resultValidation.R")
source("src/generalFunctions/resultAdjustment.R")
source("src/generalFunctions/utils.R")
source("src/extremeScenarioHandling/OneMovement.R")
source("src/resultAnalysis/resultAnalysis.R")
source("src/callLpSolve.R")
#### Sources END ###########

#### Input Prepare Start ###########
msId_vec <- c(    "7d866f21",
                  "d391d349",
                  "4fc3a05f",
                  "4b8f815")

callId_vec <- unlist(CallIdByMsId(msId_vec))
#agreementId_vec <- c('a1','a34')
#callId_vec <- unname(unlist(callIdByAgreementId(agreementId_vec)))
clientId = '999';
pref_vec = c(5.4,3.5);
operLimitMs <- 2
fungible <- FALSE

#### callInfo_df ####
callInfo_df <- CallInfoByCallId(callId_vec)
if(length(unlist(callInfo_df))==0){
  stop('Empty callInfo_df input!')
}
callInfo_df <- UnifyFxBaseUsdInCallInfo(callInfo_df)
callInfo_df <- ConvertCallAmountToBaseCcyInCallInfo(callInfo_df)

#### availAsset_df ####
oriAvailAsset_df <- AvailAssetByCallIdAndClientId(callId_vec,clientId) # available asset for the margin call
if(length(unlist(oriAvailAsset_df))==0){
  stop('Empty availAsset_df input!')
}
#### assetInfo ####
assetId_vec <- unique(oriAvailAsset_df$assetId)
assetInfo_df <- AssetInfoByAssetId(assetId_vec)
if(length(unlist(assetInfo_df))==0){
  stop('Empty assetInfo_df input!')
}
assetInfo_df <- assetInfo_df[order(assetInfo_df$id),]
assetInfo_df <- UnifyFxBaseUsdInAssetInfo(assetInfo_df)
assetInfo_df <- AddMinUnitValueInBaseCcyToAssetInfo(assetInfo_df)
print(assetInfo_df)

#### remove unexpected data ####
# remove assets that have 0 unitValue from assetInfo_df and oriAvailAsset_df
rmIdxAsset <- which(assetInfo_df$unitValue==0)
if(length(rmIdxAsset)>0){
  rmIdxAvail <- which(oriAvailAsset_df$assetId %in% assetInfo_df$id[rmIdxAsset])
  oriAvailAsset_df <- oriAvailAsset_df[-rmIdxAvail,]
  assetInfo_df <- assetInfo_df[-rmIdxAsset,]
}
# remove assets that have 0 or less quantity from assetInfo_df and oriAvailAsset_df
rmIdxAvail <- which(oriAvailAsset_df$quantity<=0)
if(length(rmIdxAvail)>0){
  rmIdxAsset <- which(assetInfo_df$id %in% oriAvailAsset_df$assetId[rmIdxAvail])
  oriAvailAsset_df <- oriAvailAsset_df[-rmIdxAvail,]
  assetInfo_df <- assetInfo_df[-rmIdxAsset,]
}


#### resource_df and availAsset_df ####
info_list <- ResourceInfoAndAvailAsset(assetInfo_df,oriAvailAsset_df)
resource_df <- info_list$resource_df
availAsset_df <- info_list$availAsset_df

#if(is.na(all(assetInfo_df$FXRate))){
#  warning('FXRate contains NA! Use Static FX rates for this test!')
#  ccy_vec <- c('USD', 'EUR', 'GBP', 'SGD', 'JPY', 'HKD', 'AUD', 'CNY', 'KRW', 'CAD', 'NOK', 'SEK', 'NZD')
#  FXRate_vec <- c(1,0.92,0.80,1.39,110.6,7.77,1.32,6.86,1112,1.34,8.68,9.02,1.42)
#  for(k in 1:length(assetInfo_df[,1])){
#    idxTemp <- which(assetInfo_df$currency[k] == ccy_vec)
#    assetInfo_df$FXRate[k] <- FXRate_vec[idxTemp]
#  }
#}



#### Scenario Analysis Output Start #####################
algoVersion <- 2
preAllocateEnable <- T
compareEnable <- F
startPoints <- 1
controls <- list(preAllocateEnable=preAllocateEnable,compareEnable=compareEnable, startPoints=startPoints)

# scenario 1: Algo Suggestion
result_mat <- AllocationScenario1(callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
                    algoVersion,controls,ifNewAlloc=T)
callOutput_list <- ResultMat2CallList(result_mat,callInfo_df,availAsset_df,resource_df)
msOutput_list <- CallList2MsList(callOutput_list,callInfo_df)
resultAnalysis1 <- DeriveResultAnalytics(availAsset_df,resource_df,callInfo_df,callOutput_list)
resultS1 <- list(callOutput=callOutput_list,msOutput=msOutput_list,resultAnalysis=resultAnalysis1)

ResultList2Df(callOutput_list,callInfo_df$id)

# scenario 2: Post Settlement Currency
# output2 <- AllocationScenario2(callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
#                                algoVersion,ifNewAlloc=T)
# resultAnalysis2 <- DeriveResultAnalytics(availAsset_df,resource_df,callInfo_df,output2$callOutput)
# resultS2 <- list(callOutput=output2$callOutput_list,msOutput=output2$msOutput_list,resultAnalysis=resultAnalysis2)


# scenario 3: post least liquid assets
# output3 <- AllocationScenario3(callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
#                                algoVersion,ifNewAlloc=T)
# resultAnalysis3 <- DeriveResultAnalytics(availAsset_df,resource_df,callInfo_df,output3$callOutput)
# resultS3 <- list(callOutput=output3$callOutput_list,msOutput=output3$msOutput_list,resultAnalysis=resultAnalysis3)

scenarios <- list()
scenarios[['Algo']] <- resultS1
#scenarios[['SettleCCY']] <- result2
#scenarios[['LeastLiquid']] <- result3
#### Scenario Analysis Output END #######################

#### Noted: 
#### Things to Consider for Settlement Currency Scenario Start ################
# limit the available assets to settlement currency
# Whether to use the real asset data(available quantity) in the inventory
# or use the hypothetic asset to get the result first?
#
# Using the real asset data may have some issues when
# 1. the settlement currency is not in the inventory?
# it's very rare that the client doesn't have the settlement currency 
# but if that is the case we will need to create that asset and fetch 
# the info (costs) from client(cannot be implemented in the allocation step) 
# and external service
# 2. the settlement currency is not sufficient? 
# this we can use red line to represent the insufficient amount
#
# However, to get started with most scenarios, we assume that the client has
# sufficient amount of settlment currencies.
#### Things to Consider for Settlement Currency Scenario END ###################
