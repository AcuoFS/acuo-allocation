options(stringsAsFactors = FALSE)


callInfo_df <- callInfoByCallId
callInfo_df$callAmount <- abs(as.numeric(callInfo_df$callAmount)) # make sure the callAmount is non-negative
#callInfo_df$callAmountUSD <- abs(as.numeric(callInfo_df$callAmountUSD)) # make sure the callAmount is non-negative
#print(callInfo_df)

availAsset_df <- availAssetByCallIdAndClientId
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

###### 3 lines added fot testing purposes, comment them after tests ##################
# changing the asset quantity and adding new custodian account to make the optimal assets insufficient,
# so that the allocation function will call the lpSolver.
# availAsset_df$quantity <- availAsset_df$quantity/5
# availAsset_df <- rbind(availAsset_df,availAsset_df)
# availAsset_df$CustodianAccount[1:length(availAsset_df[,1])/2] <- 'custodianAccountTest'
######## end #############################################################################

assetCustacId_vec <- paste(availAsset_df$assetId,availAsset_df$CustodianAccount,sep='-')
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(availAsset_df$assetId)
assetInfo_df <- assetInfoByAssetId
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

###### Manually add FXRate and venue for DEMO purposes ########
# FX rate: value = 1 USD can change how much foreign currency
ccy_vec <- c('USD', 'EUR', 'GBP', 'SGD', 'JPY', 'HKD', 'AUD', 'CNY', 'KRW', 'CAD')
FXRate_vec <- c(1,0.92,0.80,1.39,110.6,7.77,1.32,6.86,1112,1.34)
for(k in 1:length(assetInfo_df[,1])){
    idxTemp <- which(assetInfo_df$currency[k] == ccy_vec)
    assetInfo_df$FXRate[k] <- FXRate_vec[idxTemp]
}

# venue: all SG
venue_vec <- rep('SG',length(availAsset_df[,1]))
availAsset_df$venue <- venue_vec
#print(availAsset_df)
###### END ####################################

callId_vec <- callIds
pref_vec <- pref


## CALL THE ALLOCATION FUNCTION ###########
algoVersion <- 2
operLimit<- 2*length(callId_vec)

#pref_vec <- c(10,0,0)

# result <- CallAllocation(algoVersion,callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit)
result <- CallAllocation(algoVersion,scenario=1,callId_vec,resource_vec,
                          callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit)

print(result)


