source('src/functionsOfDBRequestByExecutingCypher.R')
source("src/allocationFunction.R")
graph = startGraph("http://neo4j:7474/db/data")

# inputs
callIds <- c('mcp1', 'mcp50', 'mcp7')
pref <- c(10,10,0)
clientId <- '999'
time.limit <- 1
limit <- c(10,5,5)
callInfo <- callInfoByCallId(callIds)
availAssets <- availAssetByCallIdAndClientId(callIds,clientId)
availAssets <- availAssets[order(availAssets$callId),]
assetIds <- unique(availAssets$assetId)
assetInfo <- assetInfoByAssetId(assetIds)

#execution
result <- allocationAlgo(callIds,assetIds,clientId,callInfo,availAssets,assetInfo,pref,time.limit,limit)
print(result)
cat('status:',result$status,'\n')
cat('objective:',result$objective,'\n')