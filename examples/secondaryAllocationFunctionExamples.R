library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')
source('src/coreAlgo.R')
source('src/secondAllocationFunction.R')

# Secondary allocation algo
# Triggered after the user deselects an asset from a call

########## DATA BELOW SHOULD COME FROM THE JAVA LAYER ###########
callIds <- c('mcp10','mcp31','mcp42','mcp46','mcp47')
pref<-c(2,3,5)
clientId = '999';
deselectAssetId <- 'SGD'
deselectCallId <- 'mcp46'

# current.selection is the current selection in the selection widget; should already exclude the delected asset
# the quantity of the asset is not shown on the UI, while it should be stored in the front-end but hidden, so it can be sent to the back-end

########### Get the result from Algo for testing purposes ##############
callInfo <- callInfoByCallId(callIds)
availAssets <- availAssetByCallIdAndClientId(callIds,clientId) # available asset for the margin call
availAssets <- availAssets[order(availAssets$callId),]

assetIds <- unique(availAssets$assetId)
assetInfo <- assetInfoByAssetId(assetIds)
assetInfo <- assetInfo[match(assetIds,assetInfo$id),]

## CALL THE ALLOCATION FUNCTION ###########
call.limit <- c(7,7,7); time.limit=3
result <- allocationAlgo(callIds,assetIds,'999',callInfo,availAssets,assetInfo,pref,time.limit,call.limit)

############# END ################################

########## INPUT #################################
current.selection <- result$output  
current.selection[[deselectCallId]] <- current.selection[[deselectCallId]][-which(current.selection[[deselectCallId]]$Asset==deselectAssetId),]
availAssets <- availAssetByCallIdAndClientId(deselectCallId,clientId) # available asset for the margin call
assetIds <- unique(availAssets$assetId)
assetInfo <- assetInfoByAssetId(assetIds)
callInfo <- callInfoByCallId(deselectCallId)
############ END ##################################

result1 <- secondAllocationFunction(callIds,assetIds,pref,deselectAssetId,deselectCallId,current.selection,availAssets,callInfo,assetInfo)
