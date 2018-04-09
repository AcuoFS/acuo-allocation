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
operLimitMs <- 2
fungible <- FALSE
callInfo_df <- callInfoByCallId
availAsset_df <- availAssetByCallIdAndClientId
assetInfo_df <- assetInfoByAssetId


#print(callInfo_df)


availAsset_df <- availAsset_df[order(availAsset_df$callId),]
availAsset_df$quantity[which(availAsset_df$quantity<0)] <- 0 ### dont allow negative quantity, temp


# remove assets that have 0 unitValue from assetInfo_df and avalAsset_df

rmIdxAsset <- which(assetInfo_df$unitValue==0)
if(length(rmIdxAsset)>0){
  rmIdxAvail <- which(availAsset_df$assetId %in% assetInfo_df$id[rmIdxAsset])
  availAsset_df <- availAsset_df[-rmIdxAvail,]
  assetInfo_df <- assetInfo_df[-rmIdxAsset,]
}


assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(availAsset_df$assetId)

# assetInfo
assetInfo_df <- AssetInfoFxConversion(assetInfo)

# callInfo
callInfo_df <- CallInfoFxConversion(callInfo)

# resource info
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)

#availAsset info
availAsset_df <- AvailAsset(availAsset_df)

#print(availAsset_df)
###### END ####################################


#print('callId_vec'); print(callId_vec)
## CALL THE ALLOCATION FUNCTION ###########
algoVersion <- 2
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(operLimitMs,msNum)
operLimit<- sum(operLimitMs_vec)


result <- CallAllocation(algoVersion,scenario=1,
                          callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                          ifNewAlloc=T,list())
#result_df <- ResultList2Df(result$callOutput,callId_vec)
#print(result_df)

result <- result
