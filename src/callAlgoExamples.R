
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
source('src/functionsOfDBRequestByExecutingCypher.R')

source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/generalFunctions.R")
source("src/callLpSolve.R")

source("src/otherFunctions/infoFunctions.R")
source("src/otherFunctions/analysisFunctions.R")
source("src/otherFunctions/checkFunctions.R")
source("src/otherFunctions/convertFunctions.R")
source("src/otherFunctions/modelFunctions.R")
source("src/otherFunctions/qtyFunctions.R")
source("src/otherFunctions/improveFunctions.R")
source("src/otherFunctions/modelFunctions.R")

#### Sources END ###########

#### Input Prepare Start ###########
msId_vec <- c("4b5fd5d3",
              "b5199313")
callId_vec = unlist(callIdByMsId(msId_vec))
#agreementId_vec <- c('a1','a34')
#callId_vec <- unname(unlist(callIdByAgreementId(agreementId_vec)))
clientId = '999';
pref_vec = c(5.4,3.5);

#### callInfo_df
callInfo_df <- callInfoByCallId(callId_vec)

if(length(unlist(callInfo_df))==0){
  stop('Empty callInfo_df input!')
}

idxTemp_vec <- which(callInfo_df$direction == 'IN')
if(length(idxTemp_vec)>0){
  errormsg <- paste(paste(unique(callInfo_df$marginStatement[idxTemp_vec]),collapse = ','),'is(are) money IN statement(s)!')
  stop(errormsg)
}

callInfo_df<- callInfo_df[match(callId_vec,callInfo_df$id),]

#### availAsset_df
availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId) # available asset for the margin call
if(length(unlist(availAsset_df))==0){
  stop('Empty availAsset_df input!')
}
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

### avoid negative amount
availAsset_df$quantity[which(availAsset_df$quantity<0)] <- 0
###

#### added lines for testing purposes 
# availAsset_df$quantity <- availAsset_df$quantity/4 # change tempQuantity_vec for testing
# availAsset_df <- rbind(availAsset_df,availAsset_df) # add custodianAccount for testing
# availAsset_df$CustodianAccount[1:(length(availAsset_df[,1])/2)] <- 'custodianAccountTest'
#### end 

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec

resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(SplitResource(resource_vec,'asset'))
assetInfo_df <- assetInfoByAssetId(assetId_vec)
if(length(unlist(assetInfo_df))==0){
  stop('Empty assetInfo_df input!')
}
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

if(is.na(all(assetInfo_df$FXRate))){
  warning('FXRate contains NA! Use Static FX rates for this test!')
  ccy_vec <- c('USD', 'EUR', 'GBP', 'SGD', 'JPY', 'HKD', 'AUD', 'CNY', 'KRW', 'CAD', 'NOK', 'SEK', 'NZD')
  FXRate_vec <- c(1,0.92,0.80,1.39,110.6,7.77,1.32,6.86,1112,1.34,8.68,9.02,1.42)
  for(k in 1:length(assetInfo_df[,1])){
    idxTemp <- which(assetInfo_df$currency[k] == ccy_vec)
    assetInfo_df$FXRate[k] <- FXRate_vec[idxTemp]
  }
}

#### resource_df
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)

#### Input Prepare END #############


#### Scenario Analysis Output Start #####################
scenarios <- list()
algoVersion <- 2
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
fungible <- FALSE

# scenario 1: Algo Suggestion

result1 <- CallAllocation(algoVersion,scenario=1,callId_vec,resource_vec,
                          callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                          ifNewAlloc=T,list())

# scenario 2: Post Settlement Currency
result2 <- CallAllocation(algoVersion,scenario=2,callId_vec,resource_vec,
                          callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                          ifNewAlloc=T,list())

# scenario 3: post least liquid assets
result3 <- CallAllocation(algoVersion,scenario=3,callId_vec,resource_vec,
                          callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                          ifNewAlloc=T,list())
scenarios[['Algo']] <- result1
scenarios[['SettleCCY']] <- result2
scenarios[['LeastLiquid']] <- result3
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
