
########### FUNCTION INPUT FROM JAVA LAYER Start #######################
# 1. callId_vec
# 2. clientId
# 3. uesr preference: 
#   3.1. objective vector: pref_vec
#   3.2. operation limit: operLimit
# 4. callInfo_df <- callInfoByCallId(callId_vec)
# 5. availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId)
# 6. resource_vec <- unique(paste(availAsset_df$assetId,availAsset_df$CustodianAccount,sep='-'))
# 7. assetId_vec <- assetId_vec <- matrix(unlist(strsplit(resource_vec,'-')),nrow=2)[1,]
# 8. assetInfo_df <- assetInfoByAssetId(assetId_vec)
########### FUNCTION INPUT FROM JAVA LAYER END ##########################

#### Sources Start #########
source('src/functionsOfDBRequestByExecutingCypher.R')
source("src/allocationFunction.R")
source("src/coreAlgoTest.R")
source("src/generalFunctionsTest.R")
source("src/callLpSolve.R")
#### Sources END ###########

#### Input Prepare Start ###########
callId_vec = c("mcp38","mcp50")
callId_vec = c("mcp46","mcp50","mcp47","mcp38","mcp7","mcp34","mcp35")
callId_vec = c("ed07ef6f","41e029b2")
#agreementId_vec <- c('a1','a34')
#callId_vec <- unname(unlist(callIdByAgreementId(agreementId_vec)))
clientId = '999';
pref_vec = c(10,10,0);

#### get info
callInfo_df <- callInfoByCallId(callId_vec); callInfo_df<- callInfo_df[match(callId_vec,callInfo_df$id),]
availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId) # available asset for the margin call
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

#### added lines for testing purposes 
# availAsset_df$quantity <- availAsset_df$quantity/4 # change tempQuantity_vec for testing
# availAsset_df <- rbind(availAsset_df,availAsset_df) # add custodianAccount for testing
# availAsset_df$CustodianAccount[1:(length(availAsset_df[,1])/2)] <- 'custodianAccountTest'
#### end 

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(SplitResource(resource_vec,'asset'))
assetInfo_df <- assetInfoByAssetId(assetId_vec)
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]
#### Input Prepare END #############


#### Scenario Analysis Output Start #####################
scenarios <- list()
algoVersion <- 2
operLimitMs <- 2
operLimit<- operLimitMs*length(unique(callInfo_df$marginStatement))
fungible <- FALSE

# scenario 1: Algo Suggestion
result1 <- CallAllocation(algoVersion,scenario=1,callId_vec,resource_vec,
                          callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit,operLimitMs,fungible)

# scenario 2: Post Settlement Currency
result2 <- CallAllocation(algoVersion,scenario=2,callId_vec,resource_vec,
                          callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit,operLimitMs,fungible)

# scenario 3: post least liquid assets
result3 <- CallAllocation(algoVersion,scenario=3,callId_vec,resource_vec,
                          callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit,operLimitMs,fungible)
scenarios[['Algo']] <- result1
scenarios[['SettleCCY']] <- result2
scenarios[['LeastLiquid']] <- result3
#### Scenario Analysis Output END #######################

#### Noted: 
#### Things to Consider for Settlement Currency Scenario Start ################
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
#### Things to Consider for Settlement Currency Scenario END ###################
