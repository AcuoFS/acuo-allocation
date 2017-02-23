
########### FUNCTION INPUT FROM JAVA LAYER ###########################
# call ids
# user id
# uesr preference
# callInfo <- callInfoByCallId(callIds)
# availAssets <- availAssetByCallIdAndClientId(callIds,clientId)
# assetIds
# assetInfo <- assetInfoByAssetId(assetId)
#######################################################################

# Data below should come from the Java, 
# but I need to test before the real connection built,
# so I use cypher queries to request DB to get data


source('src/functionsOfDBRequestByExecutingCypher.R')
source("src/allocationFunction.R")
source("src/coreAlgo.R")

# input #
clientId = '999';
callIds = c("mcp1","mcp5","mcp7")
callIds = c("mcp45","mcp50","mcp43")
callIds = c("mcp1","mcp5","mcp7","mcp38","mcp50")
callIds = c("mcp1","mcp5","mcp7","mcp38","mcp20","mcp22","mcp15","mcp30","mcp50","mcp51")
callIds = c("mcp32","mcp33","mcp37","mcp26","mcp38","mcp50");


# get info #
callInfo <- callInfoByCallId(callIds)
availAssets <- availAssetByCallIdAndClientId(callIds,clientId) # available asset for the margin call
availAssets <- availAssets[order(availAssets$callId),]

assetInfo <- assetInfoByAssetId(assetIds)
assetInfo <- assetInfo[match(assetIds,assetInfo$id),]

## CALL THE ALLOCATION FUNCTION ###########
call.limit <- c(7,7,10); time.limit=10
start.time <- proc.time()[3]
pref = c(5,8,3);
result <- allocationAlgo(callIds,assetIds,clientId,callInfo,availAssets,assetInfo,pref,time.limit,call.limit)
end.time <- proc.time()[3]
run.time <- end.time-start.time






