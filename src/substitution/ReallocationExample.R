# Project: optimize settled collateral by substitution
# Create: Jan 4 2018
# Author: Yuan Silin
# Functionality: Optimize the settled collateral by substituting with the other assets 
# based on the user preferences

# set path
setwd("E:/ACUO/projects/acuo-allocation/")

#### Sources Start #########
source('src/substitution/dbExecutionForLocalTests.R')
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/generalFunctions.R')
source('src/manualAllocation/manualAllocationGeneralFunctions.R')
#### Sources END ###########


#### Reset Local Database ##########
DeleteTestData()
BuildTestData()


#### Input Prepare ###########
clientId <- '999'


settledCollaterals <- SettledCollaterals()

callId_vec <- unique(settledCollaterals$call)

#### callInfo_df
callInfo_df <- CallInfoByCallId(callId_vec)
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



# step 1
# generate cost/liquidity matrix
# objectives
objParams_list <- ConstructModelObj(callAmount_mat,minUnitValue_mat,haircut_mat,costBasis_mat,eli_mat,callInfo_df,
                                    callId_vec,resource_vec)
# optimal asset
optimalAsset_mat <- DeriveOptimalAssetsV2(quantity_mat,eli_mat,callAmount_mat,haircut_mat,minUnitValue_mat,
                                          pref_vec,objParams_list,callId_vec,resource_vec)





