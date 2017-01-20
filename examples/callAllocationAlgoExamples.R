

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

callIds.group1 <- c('mcp10','mcp41','mcp42','mcp46','mcp47')
callIds.group2 <- c('mcp2','mcp4','mcp12','mcp20','mcp22','mcp23','mcp30','mcp40','mcp43','mcp44','mcp46','mcp48','mcp50')

source('src/functionsOfDBRequestByExecutingCypher.R')

callIds <- callIds.group2
clientId <- '999'
pref<-c(10,3,5)
callInfo <- callInfoByCallId(callIds)
availAssets <- availAssetByCallIdAndClientId(callIds,clientId) # available asset for the margin call
assetIds <- unique(availAssets$assetId)
assetInfo <- assetInfoByAssetId(assetIds)
################## END ################################################

## CALL THE ALLOCATION FUNCTION ###########
source("src/allocationFunction2.R")

allocationAlgo(callIds,assetIds,clientId,callInfo,availAssets,assetInfo,pref)



