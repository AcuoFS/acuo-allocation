options(stringsAsFactors = FALSE)

callId_vec <- callIds
pref_vec <- pref
operLimit <- 2*length(callId_vec)
algoVersion <- 2
dsAssetId <- assetId
dsCallId_vec <- dsCallIds

currentSelection_list <- selections
print(currentSelection_list)
callInfo_df <- callInfoByCallId
callInfo_df$callAmount <- abs(as.numeric(callInfo_df$callAmount)) # make sure the callAmount is non-negative
#print(callInfo_df)

availAsset_df <- availAssetByCallIdAndClientId # available asset for the margin call
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(SplitResource(resource_vec,'asset'))
assetInfo_df <- assetInfoByAssetId
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

#### Input Prepare END ##############

#### Call Second Level Algo Start ###
result <- callSecondAllocation(
				algoVersion,
				callId_vec,
				resource_vec,
				callInfo_df,
				availAsset_df,
				assetInfo_df,
				pref_vec,
				operLimit,
                dsAssetId,
                dsCallId_vec,
                currentSelection_list)
result

#### Call Second Level Algo END #####