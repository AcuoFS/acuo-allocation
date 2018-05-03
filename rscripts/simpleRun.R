options(stringsAsFactors = FALSE)

#### parameters checking ####
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

#### rename the parameters ####
callId_vec <- callIds
pref_vec <- pref
operLimitMs <- 2
fungible <- FALSE
callInfo_df <- callInfoByCallId
assetInfo_df <- assetInfoByAssetId
availAsset_df <- availAssetByCallIdAndClientId

#### remove unexpected data ####
# remove assets that have 0 unitValue from assetInfo_df and availAsset_df
rmIdxAsset <- which(assetInfo_df$unitValue==0)
if(length(rmIdxAsset)>0){
  rmIdxAvail <- which(availAsset_df$assetId %in% assetInfo_df$id[rmIdxAsset])
  availAsset_df <- availAsset_df[-rmIdxAvail,]
  assetInfo_df <- assetInfo_df[-rmIdxAsset,]
}
# remove assets that have 0 or less quantity from assetInfo_df and availAsset_df
rmIdxAvail <- which(availAsset_df$quantity<=0)
if(length(rmIdxAvail)>0){
  rmIdxAsset <- which(assetInfo_df$id %in% availAsset_df$assetId[rmIdxAvail])
  availAsset_df <- availAsset_df[-rmIdxAvail,]
  assetInfo_df <- assetInfo_df[-rmIdxAsset,]
}
# remove assets in availAsset_df but not in assetInfo_df
rmIdxAvail <- which(is.na(match(availAsset_df$assetId,assetInfo_df$id)))
if(length(rmIdxAvail)>0){
  availAsset_df <- availAsset_df[-rmIdxAvail,]
}

#### callInfo_df ####
callInfo_df$callAmount <- abs(as.numeric(callInfo_df$callAmount)) # make sure the callAmount is non-negative
callInfo_df <- CallInfoFxConversion(callInfo_df)

#### assetInfo_df ####
assetInfo_df <- AssetInfoFxConversion(assetInfo_df)


#### resource_df and availAsset_df ####
info_list <- ResourceInfoAndAvailAsset(assetInfo_df,availAsset_df)
resource_df <- info_list$resource_df
availAsset_df <- info_list$availAsset_df
###### END ####################################

#### CALL THE ALLOCATION FUNCTION ###########
algoVersion <- 2
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(operLimitMs,msNum)
operLimit<- sum(operLimitMs_vec)

result <- CallAllocation(algoVersion,scenario=1,
                          callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                          ifNewAlloc=T,list())
#result_df <- ResultList2Df(result$callOutput,callId_vec)

result <- result
