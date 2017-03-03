
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

# input #
callIds = c("mcp45","mcp50","mcp43")
callIds = c("mcp1","mcp5","mcp7","mcp50")
callIds = c("mcp1","mcp5","mcp7","mcp38","mcp20","mcp22","mcp15","mcp20","mcp22","mcp30","mcp50","mcp51")
callIds = c("mcp32","mcp33","mcp37","mcp26","mcp39","mcp50");
callIds = c("mcp10")
clientId = '999';

# get info #
callInfo <- callInfoByCallId(callIds)
availAssets <- availAssetByCallIdAndClientId(callIds,clientId) # available asset for the margin call
availAssets <- availAssets[order(availAssets$callId),]

# change quantity for testing
availAssets$quantity <- availAssets$quantity/5
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
#start.time <- proc.time()[3]
pref = c(5,8,3);
result <- allocationAlgo(callIds,assetCustacIds,callInfo,availAssets,assetInfo,pref,time.limit,call.limit)
output <- result$output
#end.time <- proc.time()[3]
#run.time <- end.time-start.time







