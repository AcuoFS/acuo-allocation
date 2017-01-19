

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
callIds <- c('mc10','mc41','mc42','mc46','mc47')
clientId <- '999'
pref<-c(2,3,5)
callInfo <- callInfoByCallId(callIds)
availAssets <- availAssetByCallIdAndClientId(callIds,clientId) # available asset for the margin call
assetIds <- unique(availAssets$assetId)
assetInfo <- assetInfoByAssetId(assetIds)
################## END ################################################

## CALL THE ALLOCATION FUNCTION ###########
source("src/allocationFunction2.R")

allocationAlgo(callIds,assetIds,clientId,callInfo,availAssets,assetInfo,pref)



