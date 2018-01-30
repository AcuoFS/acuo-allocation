# Project: optimize settled collateral by substitution
# Create: Jan 4 2018
# Author: Yuan Silin
# Functionality: Optimize the settled collateral by substituting with the other assets 
# based on the user preferences

# set path
setwd("E:/ACUO/projects/acuo-allocation/")

#### Sources Start #########
source('src/substitution/reallocation.R')
source('src/substitution/dbExecutionForLocalTests.R')
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/generalFunctions.R')
source('src/manualAllocation/manualAllocationGeneralFunctions.R')
#### Sources END ###########


#### Reset Local Database ##########
DeleteTestData()
BuildTestData()

#### Input Prepare ###########

settledCollaterals <- SettledCollaterals()

clientId <- '999'
pref_vec <- c(5.4,3.5)
agreementNum <- length(unique(settledCollaterals$agreement))
operLimitAg_vec <- rep(2,agreementNum)
operLimit<- sum(operLimitAg_vec)
fungible <- FALSE


callId_vec <- unique(settledCollaterals$call)

#### callInfo_df
callInfo_df <- CallInfoByCallId(callId_vec)
# sort by call amount
callInfo_df <- callInfo_df[order(callInfo_df$callAmount,decreasing = T),]
callId_vec <- callInfo_df$id

# 4: availAsset_df
availAsset_df <- AvailAssetByCallIdAndClientId(clientId = clientId, callId = callId_vec)
availAsset_df$assetCustacId <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)

resource_vec <- unique(availAsset_df$assetCustacId)

# 5: assetInfo_df
assetId_vec <- unique(SplitResource(resource_vec,'asset'))
assetInfo_df <- AssetInfoByAssetId(assetId_vec)

# use the static fx in local DB
fxRate_df <- FxRates(assetInfo_df$currency)
assetInfo_df$FXRate <- as.double(fxRate_df$fxRate[match(assetInfo_df$currency,fxRate_df$currency)])

# resource_df & adjusted availAsset_df
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)


#### CALL Reallocation FUNCTION ####

result <- Reallocation(settledCollaterals,availAsset_df,callInfo_df,resource_df,
                       pref_vec,operLimit,operLimitAg_vec,fungible)


