
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
source("src/callLpSolve.R")

# input #
callId_vec = c("mcp50")
callId_vec = c("mcp1","mcp5","mcp7","mcp50")
callId_vec = c("mcp1","mcp5","mcp7","mcp38","mcp20","mcp22","mcp15","mcp30","mcp50","mcp51")
callId_vec = c("mcp32","mcp33","mcp37","mcp26","mcp39");
callId_vec = c("mcp45","mcp50","mcp43","mcp38")
callId_vec = c("mcp1","mcp31","mcp43","mcp10")

clientId = '999';

# get info #
callInfo_df <- callInfoByCallId(callId_vec); callInfo_df<- callInfo_df[match(callId_vec,callInfo_df$id),]
availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId) # available asset for the margin call
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

########## added lines for testing purposes ##############
# availAsset_df$quantity <- availAsset_df$quantity/10 # change tempQuantity_vec for testing
# availAsset_df <- rbind(availAsset_df,availAsset_df) # add custodianAccount for testing
# availAsset_df$CustodianAccount[1:length(availAsset_df[,1])/2] <- 'custodianAccountTest'
########## end ###########################################


# temp changes start
# availAsset_df$quantity[which(availAsset_df$assetId=='GBP')] = 1000000-0.01
# temp changes end
 
assetCustacId_vec <- paste(availAsset_df$assetId,availAsset_df$CustodianAccount,sep='-')
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- matrix(unlist(strsplit(resource_vec,'-')),nrow=2)[1,]
assetInfo_df <- assetInfoByAssetId(assetId_vec)
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]


## main function, interface of java #######
CallAllocation <- function(algoVersion,callId_vec,resource_vec,resourceOri_vec,callInfo_df,availAsset_df,availAssetOri_df,assetInfo_df,assetInfoOri_df,pref_vec,operLimit){
  inputLimit_vec <- c(7,7,7,5); 
  timeLimit=10; 
  callOrderMethod=3
  minMoveValue<- 1000;
  result <- AllocationAlgo(callId_vec,resource_vec,resourceOri_vec,callInfo_df,availAsset_df,availAssetOri_df,assetInfo_df,assetInfoOri_df,pref_vec,operLimit,
                           algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod)
  return(result)
}


## CALL THE ALLOCATION FUNCTION ###########
algoVersion <- 2
pref_vec = c(10,10,10);
operLimit<- 12
result1 <- CallAllocation(algoVersion,callId_vec,resource_vec,resource_vec,callInfo_df,availAsset_df,availAsset_df,assetInfo_df,assetInfo_df,pref_vec,operLimit)
result1

#### Scenario Analysis Output Start #####################
scenarios <- list()

# scenario 1: post cash only
pref_vec <- c(10,0,0)
#### Things to Consider for Cash Only Scenario Start ################
# limit the available assets to settlement currency
# Whether to use the real asset data(available quantity) in the inventory
# or use the hypothetic asset to get the result first?
#
# Using the real asset data may have some issues when
# 1. the settlement currency is not in the inventory?
# it's very rare that the client doesn't have the settlement currency 
# but if that is the case we will need to create that asset and fetch 
# the info (costs) from client(cannot be implemented in the allocation step) 
# and external service
# 2. the settlement currency is not sufficient? 
# this we can use red line to represent the insufficient amount
#
# However, to get started with most scenarios, we assume that the client has
# sufficient amount of settlment currencies.
#### Things to Consider for Cash Only Scenario END ###################

availAssetCash_df <- availAsset_df
resourceCash_vec <- resource_vec
assetInfoCash_df <- assetInfo_df
settleCcy_vec <- callInfo_df$currency

idxKeep_vec <- rep(0,length(availAssetCash_df$callId))
count <- 0
for(i in 1:length(callId_vec)){
  idxTemp_vec <- which(availAssetCash_df$callId==callId_vec[i] & availAsset_df$assetId==callInfo_df$currency[i])
  numTemp <- length(idxTemp_vec)
  count <- count+numTemp
  idxKeep_vec[(count-numTemp+1):count] <- idxTemp_vec
}
idxKeep_vec <- idxKeep_vec[1:count]
availAssetCash_df <- availAssetCash_df[idxKeep_vec,]
resourceCash_vec <- unique(availAssetCash_df$assetCustacId)
assetIdCash_vec <- matrix(unlist(strsplit(resourceCash_vec,'-')),nrow=2)[1,]
assetInfoCash_df <- assetInfoByAssetId(assetIdCash_vec)
assetInfoCash_df <- assetInfoCash_df[match(assetIdCash_vec,assetInfoCash_df$id),]


result2 <- CallAllocation(algoVersion,callId_vec,resourceCash_vec,resource_vec,callInfo_df,availAssetCash_df,availAsset_df,assetInfoCash_df,assetInfo_df,pref_vec,operLimit)
result2 

# scenario 2: post least liquid assets
pref_vec <- c(0,10,0)
result3 <- CallAllocation(algoVersion,callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit)
result3
#### Scenario Analysis Output END #######################

scenarios[['Suggestion']] <- result1
scenarios[['AllCash']] <- result2
scenarios[['LeastLiquid']] <- result3

