library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')
source('src/secondAllocationFunction.R')

# Secondary allocation algo
# Triggered after the user deselects an asset from a call

########## DATA BELOW SHOULD COME FROM THE JAVA LAYER ###########
callIds <- c('mcp47','mcp46','mcp42','mcp35','mcp34')
clientId <- '999'
pref<-c(2,3,5)
deselectAssetId <- 'SGD'
deselectCallId <- 'mcp46'

########### Get the result from Algo for testing purposes ##############
callInfo <- callInfoByCallId(callIds); callIds <- unique(callInfo$id)
availAssets <- availAssetByCallIdAndClientId(callIds,clientId) # available asset for the margin call
availAssets <- availAssets[order(availAssets$callId),]

# change quantity for testing
availAssets$quantity <- availAssets$quantity/2
# add custodianAccount for testing
availAssets <- rbind(availAssets,availAssets)
availAssets$CustodianAccount[1:length(availAssets[,1])/2] <- 'custodianAccountTest'

asset.custac.id <- paste(availAssets$assetId,availAssets$CustodianAccount,sep='-')
availAssets$assetCustacId <- asset.custac.id
assetCustacIds <- unique(asset.custac.id)

assetIds <- as.character(data.frame(strsplit(assetCustacIds,'-'))[1,])
assetInfo <- assetInfoByAssetId(assetIds)
assetInfo <- assetInfo[match(assetIds,assetInfo$id),]
## CALL THE ALLOCATION FUNCTION ###########
call.limit <- c(7,7,7); time.limit=3
result <- allocationAlgo(callIds,assetCustacIds,callInfo,availAssets,assetInfo,pref,time.limit,call.limit)
############# END ################################

########## INPUT ##################################
# current.selection is the current selection in the selection widget; should already exclude the delected asset
# the quantity of the asset is not shown on the UI, while it should be stored in the front-end but hidden, so it can be sent to the back-end
current.selection <- result$output  
current.selection[[deselectCallId]] <- current.selection[[deselectCallId]][-which(current.selection[[deselectCallId]]$Asset==deselectAssetId),]
availAssets <- availAssets[which(availAssets$callId==deselectCallId),] # available asset for the margin call
assetCustacIds <- unique(availAssets$assetCustacId)
assetIds <- as.character(data.frame(strsplit(assetCustacIds,'-'))[1,])
assetInfo <- assetInfoByAssetId(assetIds)

#callInfo <- callInfoByCallId(deselectCallId)
#msIds <- callInfo$marginStatement
############ END ###################################

result1 <- secondAllocationFunction(callIds,callInfo,assetCustacIds,pref,deselectAssetId,deselectCallId,current.selection,availAssets,assetInfo)
