
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
source("src/allocationAndAnalysis.R")
source("src/allocationFunction.R")
source("src/extremeScenarioHandling/OneMovement.R")
source("src/extremeScenarioHandling/utils.R")
source("src/allocationByGroup/allocationByGroup.R")
source("src/coreAlgo.R")
source("src/generalFunctions.R")
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
operLimitMs <- 1 
fungible <- FALSE

#### callInfo_df ####
callInfo_df <- CallInfoByCallId(callId_vec)
if(length(unlist(callInfo_df))==0){
  stop('Empty callInfo_df input!')
}
callInfo_df <- CallInfoFxConversion(callInfo_df)
# margin statements with money IN calls
#idxTemp_vec <- which(callInfo_df$direction == 'IN')
#if(length(idxTemp_vec)>0){
#  errormsg <- paste(paste(unique(callInfo_df$marginStatement[idxTemp_vec]),collapse = ','),'is(are) money IN statement(s)!')
#  stop(errormsg)
#}

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
assetInfo_df<-assetInfo_df[order(assetInfo_df$id),]
assetInfo_df <- AssetInfoFxConversion(assetInfo_df)
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
# remove assets in availAsset_df but not in assetInfo_df
#rmIdxAvail <- which(is.na(match(availAsset_df$assetId,assetInfo_df$id)))
#if(length(rmIdxAvail)>0){
#  availAsset_df <- availAsset_df[-rmIdxAvail,]
#}


#### resource_df and availAsset_df ####
info_list <- ResourceInfoAndAvailAsset(assetInfo_df,oriAvailAsset_df)
resource_df <- info_list$resource_df
availAsset_df <- info_list$availAsset_df

updateInfo <- UpdateResourceInfoAndAvailAsset(resource_df,availAsset_df,length(callInfo_df$id))
resource_df <- updateInfo$resource_df
availAsset_df <- updateInfo$availAsset_df

#if(is.na(all(assetInfo_df$FXRate))){
#  warning('FXRate contains NA! Use Static FX rates for this test!')
#  ccy_vec <- c('USD', 'EUR', 'GBP', 'SGD', 'JPY', 'HKD', 'AUD', 'CNY', 'KRW', 'CAD', 'NOK', 'SEK', 'NZD')
#  FXRate_vec <- c(1,0.92,0.80,1.39,110.6,7.77,1.32,6.86,1112,1.34,8.68,9.02,1.42)
#  for(k in 1:length(assetInfo_df[,1])){
#    idxTemp <- which(assetInfo_df$currency[k] == ccy_vec)
#    assetInfo_df$FXRate[k] <- FXRate_vec[idxTemp]
#  }
#}

#### Input Prepare END #############



#### Scenario Analysis Output Start #####################
scenarios <- list()
algoVersion <- 2


# scenario 1: Algo Suggestion
result1 <- CallAllocation(algoVersion,scenario=1,
                          callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
                          ifNewAlloc=T,list())

# scenario 2: Post Settlement Currency
#result2 <- CallAllocation(algoVersion,scenario=2,
#                          callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
#                          ifNewAlloc=T,list())

# scenario 3: post least liquid assets
#result3 <- CallAllocation(algoVersion,scenario=3,
#                          callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
#                          ifNewAlloc=T,list())

scenarios[['Algo']] <- result1
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
