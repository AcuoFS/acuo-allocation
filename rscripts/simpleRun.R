options(stringsAsFactors = FALSE)

if(length(unlist(callIds))==0){
  stop('Empty callIds input!')
}

if(length(unlist(pref))==0){
  stop('Empty pref input!')
}

if(length(unlist(callInfoByCallId))==0){
  stop('Empty callInfoByCallId input!')
}

if(length(unlist(availAssetByCallIdAndClientId))==0){
  stop('Empty availAssetByCallIdAndClientId input!')
}

if(length(unlist(assetInfoByAssetId))==0){
  stop('Empty assetInfoByAssetId input!')
}

callId_vec <- callIds
pref_vec <- pref
callInfo_df <- callInfoByCallId
callInfo_df$callAmount <- abs(as.numeric(callInfo_df$callAmount)) # make sure the callAmount is non-negative

callInfo_df$callAmountOri <- callInfo_df$callAmount # call amount in principal currency
callInfo_df$callAmount <- callInfo_df$callAmount/callInfo_df$FXRate # call amount in USD

#print(callInfo_df)

availAsset_df <- availAssetByCallIdAndClientId
availAsset_df <- availAsset_df[order(availAsset_df$callId),]
availAsset_df$quantity[which(availAsset_df$quantity<0)] <- 0 ### dont allow negative quantity, temp

assetInfo_df <- assetInfoByAssetId

###### 3 lines added fot testing purposes, comment them after tests ##################
# changing the asset quantity and adding new custodian account to make the optimal assets insufficient,
# so that the allocation function will call the lpSolver.
# availAsset_df$quantity <- availAsset_df$quantity/5
# availAsset_df <- rbind(availAsset_df,availAsset_df)
# availAsset_df$CustodianAccount[1:length(availAsset_df[,1])/2] <- 'custodianAccountTest'
######## end #############################################################################

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(availAsset_df$assetId)
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)


## FX rate conversion ##
# keep the original fx rate in assetInfo$oriFXRate and callInfo$oriFXRate
# fx used for calculation: 1 USD can change how much foreign currency

assetInfo_df$oriFXRate <- assetInfo_df$FXRate
if(!is.null(assetInfo_df$from)&&!is.null(assetInfo_df$to)){
  idxTo <- which(assetInfo_df$to=="USD")
  assetInfo_df$FXRate[idxTo] <- 1/assetInfo_df$FXRate[idxTo] 
}
callInfo_df$oriFXRate <- callInfo_df$FXRate
if(!is.null(callInfo_df$from)&&!is.null(callInfo_df$to)){
  idxTo <- which(callInfo_df$to=="USD")
  callInfo_df$FXRate[idxTo] <- 1/callInfo_df$FXRate[idxTo] 
}

# venue: all SG
venue_vec <- rep('SG',length(availAsset_df[,1]))
availAsset_df$venue <- venue_vec
#print(availAsset_df)
###### END ####################################


#print('callId_vec'); print(callId_vec)
## CALL THE ALLOCATION FUNCTION ###########
algoVersion <- 2
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
fungible <- FALSE


callId_vec <- unlist(callId_vec)
result <- CallAllocation(algoVersion,scenario=1,callId_vec,resource_vec,
                          callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                          ifNewAlloc=T,list())
#result_df <- ResultList2Df(result$callOutput,callId_vec)
#print(result_df)

result <- result
