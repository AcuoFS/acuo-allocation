library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')
source('src/coreAlgo.R')
source('src/secondAllocationFunction.R')

# Secondary allocation algo
# Triggered after the user deselects an asset from a call

########## DATA BELOW SHOULD COME FROM THE JAVA LAYER ###########
callId_vec <- c('mcp47','mcp46','mcp42','mcp35','mcp34')
clientId <- '999'
pref_vec<-c(2,3,5)
deselectAssetId <- 'SGD'
deselectCallId <- 'mcp46'

########### Get the result from Algo for testing purposes ##############
callInfo_df <- callInfoByCallId(callId_vec); callId_vec <- unique(callInfo_df$id)
availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId) # available asset for the margin call
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

# change tempQuantity_vec for testing
availAsset_df$quantity <- availAsset_df$quantity/2
# add custodianAccount for testing
availAsset_df <- rbind(availAsset_df,availAsset_df)
availAsset_df$CustodianAccount[1:length(availAsset_df[,1])/2] <- 'custodianAccountTest'

assetCustacId_vec <- paste(availAsset_df$assetId,availAsset_df$CustodianAccount,sep='-')
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- as.character(data.frame(strsplit(resource_vec,'-'))[1,])
assetInfo_df <- assetInfoByAssetId(assetId_vec)
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]
## CALL THE ALLOCATION FUNCTION ###########
callLimit_vec <- c(7,7,7); timeLimit=3
result <- allocationAlgo(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,timeLimit,callLimit_vec)
############# END ################################

########## INPUT ##################################
# currentSelection_list is the current selection in the selection widget; should already exclude the delected asset
# the tempQuantity_vec of the asset is not shown on the UI, while it should be stored in the front-end but hidden, so it can be sent to the back-end
currentSelection_list <- result$output  
currentSelection_list[[deselectCallId]] <- currentSelection_list[[deselectCallId]][-which(currentSelection_list[[deselectCallId]]$Asset==deselectAssetId),]
availAsset_df <- availAsset_df[which(availAsset_df$callId==deselectCallId),] # available asset for the margin call
resource_vec <- unique(availAsset_df$assetCustacId)
assetId_vec <- as.character(data.frame(strsplit(resource_vec,'-'))[1,])
assetInfo_df <- assetInfoByAssetId(assetId_vec)

#callInfo_df <- callInfoByCallId(deselectCallId)
#msIds <- callInfo_df$marginStatement
############ END ###################################

result1 <- secondAllocationFunction(callId_vec,callInfo_df,resource_vec,pref_vec,deselectAssetId,deselectCallId,currentSelection_list,availAsset_df,assetInfo_df)
