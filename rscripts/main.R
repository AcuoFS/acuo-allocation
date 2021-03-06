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

callIds.all <- c('mcp1','mcp5','mcp7','mcp15','mcp19','mcp23','mcp25',                            # 7 lagacy VM
                 'mcp2','mcp4','mcp8','mcp10','mcp14','mcp16','mcp20','mcp22','mcp26','mcp30',    # 10 bilateral VM
                 'mcp31','mcp32','mcp33','mcp37','mcp38','mcp39','mcp42',#'mcp34','mcp35','mcp36', #7/ 10 cleared VM
                 'mcp43','mcp44','mcp45','mcp49','mcp50','mcp51','mcp54')#'mcp46','mcp47','mcp48',) # 7/10 cleared IM
idx.VM.all <- c(1:24)
idx.IM.all <- c(25:31)
all.num <- length(callIds.all)

clientId <- '999'

# preference order: operation, liquidity, and cost
pref.all <- list(c(10,0,0),c(10,10,0),c(10,0,10),
                 c(0,10,0),c(0,10,10),
                 c(0,0,10),
                 c(10,5,7),c(4,10,2),c(3,5,10),c(9,8,7))

index <- 0
for(i in 1:2){
  callIds.num <- i
  sample.num <- 10
  for(k in 1:sample.num){
    index <- (i-1)*10+k
    pref <- pref.all[[k]]
    idx.callIds <- sample(1:31,callIds.num)
    callIds <- callIds.all[idx.callIds]
    VM.num <- length(intersect(idx.callIds,idx.VM.all))
    callInfo <- callInfoByCallId(callIds)
    availAssets <- availAssetByCallIdAndClientId(callIds,clientId) # available asset for the margin call
    availAssets <- availAssets[order(availAssets$callId),]
    assetIds <- unique(availAssets$assetId)
    assetInfo <- assetInfoByAssetId(assetIds)

    ## CALL THE ALLOCATION FUNCTION ###########
    limit <- c(10,6,100)
    result <- allocationAlgo(callIds,assetIds,clientId,callInfo,availAssets,assetInfo,pref,limit)
  }
}
