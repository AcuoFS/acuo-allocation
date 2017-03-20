
########### FUNCTION INPUT FROM JAVA LAYER ###########################
# call ids
# user id
# uesr preference
# callInfo_df <- callInfoByCallId(callId_vec)
# availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId)
# assetId_vec
# assetInfo_df <- assetInfoByAssetId(assetId)
#######################################################################

# Data below should come from the Java, 
# but I need to test before the real connection built,
# so I use cypher queries to request DB to get data


source('src/functionsOfDBRequestByExecutingCypher.R')
source("src/allocationFunction.R")
source("src/coreAlgo.R")

# input #
callId_vec = c("mcp45","mcp50","mcp43")
callId_vec = c("mcp1","mcp5","mcp7","mcp50")
callId_vec = c("mcp1","mcp5","mcp7","mcp38","mcp20","mcp22","mcp15","mcp20","mcp22","mcp30","mcp50","mcp51")
callId_vec = c("mcp32","mcp33","mcp37","mcp26","mcp39","mcp50");
callId_vec = c("mcp50")
clientId = '999';

# get info #
callInfo_df <- callInfoByCallId(callId_vec)
availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId) # available asset for the margin call
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

# change tempQuantity_vec for testing
availAsset_df$quantity <- availAsset_df$quantity/5
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
#start.time <- proc.time()[3]
pref_vec = c(10,10,0);
result <- AllocationAlgo(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,timeLimit,callLimit_vec)
output <- result$output
#end.time <- proc.time()[3]
#run.time <- end.time-start.time






