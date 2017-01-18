library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')
source('src/secondAllocationFunction.R')

# Secondary allocation algo
# Triggered after the user deselects an asset from a call

########## DATA BELOW SHOULD COME FROM THE JAVA LAYER ###########
callIds <- c('mc47','mc46','mc42','mc41','mc10')
clientId <- '999'
pref<-c(2,3,5)
deselectAssetId <- 'SGD'
deselectCallId <- 'mc46'
callInfo <- callInfoByCallId(deselectCallId)
# current.selection is the current selection in the selection widget; should already exclude the delected asset
# the quantity of the asset is not shown on the UI, while it should be stored in the front-end but hidden, so it can be sent to the back-end
current.selection <- allocationAlgo(callId=callIds,clientId=clientId,pref=pref)$output  
current.selection[[deselectCallId]] <- current.selection[[deselectCallId]][-which(current.selection[[deselectCallId]]$Asset==deselectAssetId),]
availAssets <- availAssetByCallIdAndClientId(deselectCallId,clientId) # available asset for the margin call
assetIds <- unique(availAssets$assetId)
assetInfo <- assetInfoByAssetId(assetIds)
############ END #################################################

result1 <- secondAllocationFunction(callIds,assetIds,clientId,pref,deselectAssetId,deselectCallId,current.selection,availAssets,callInfo,assetInfo)
