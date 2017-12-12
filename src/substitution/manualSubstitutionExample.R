# Project: one for one asset manual substitution
# Create: 12 Dec 2017
# Author: Yuan Silin
# Functionality: Substitute the settled collateral with a specific amount with a new asset
# which may potentially reduce the costs of collateral based on the user preferences and constraints
#
# Inputs
# 1. subCollateral_df:
# 1.1 $asset
# 1.2 $custodianAccount
# 1.3 $call
# 1.4 $fxRate
# 1.5 $haircut
# 1.6 $amount
# 2. newAsset
# 3. newCustodianAccount
# 4. availAsset_df
# 5. assetInfo_df
# 6. callInfo_df 
#
# DB updates
# Create (:AssetTransfer) for new asset
# Update (:AssetTransfer) of original asset ?
# Update (:AssetPledge {status:pending}) -- add the amount of new asset
# Update (:AssetPledge {status:settled}) -- ? remove the amount of original asset
# Update (:Collateral {status:pending}) -- add the amount of new asset
# Update (:Collateral {status:settled}) -- ? remove the amount of original asset
#
# R will return the information for AssetTransfer, the rest updates will be handled by java layer
#
# UI has to call the backend again to refresh the new data
#
###

# set path
setwd("E:/ACUO/projects/acuo-allocation/")

#### Sources Start #########
source('src/substitution/dbExecutionForLocalTests.R')
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/generalFunctions.R')
source('src/manualAllocation/manualAllocationGeneralFunctions.R')
source('src/substitution/manualSubstitution.R')
#### Sources END ###########


#### Reset Local Database ##########
DeleteTestData()
BuildTestData()


#### Input Prepare ###########
clientId <- '999'

# load data from DB to mock the input 
settledCollaterals <- SettledCollaterals()

# 1: subCollateral_df
subCollateral_df <- settledCollaterals[1,]
subCollateral_df$fxRate <- as.double(subCollateral_df$fxRate)
oldResource <- PasteResource(subCollateral_df$asset,subCollateral_df$custodianAccount)
oldResourceAmount <- subCollateral_df$amount
amountSelect <- "Amount"
#amountSelect <- "AdjAmount"

# 2. newAsset
newAsset <- "SGD"

# 3. newCustodianAccount
newCustodianAccount <- "CustodianAccount1D"

newResource <- PasteResource(newAsset,newCustodianAccount)

# 4: availAsset_df
resource_vec <- c(oldResource,newResource)
availAsset_df <- AvailAssetByCallIdAndClientId(clientId = clientId, callId = subCollateral_df$call)
availAsset_df$assetCustacId <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)

availAsset_df <- availAsset_df[match(c(oldResource,newResource),assetCustacId_vec),]

# 5: assetInfo_df
assetId_vec <- unique(SplitResource(resource_vec,'asset'))
assetInfo_df <- AssetInfoByAssetId(assetId_vec)

# use the static fx in local DB
fxRate_df <- FxRates(assetInfo_df$currency)
assetInfo_df$FXRate <- as.double(fxRate_df$fxRate[match(assetInfo_df$currency,fxRate_df$currency)])

# 6: callInfo_df
callInfo_df <- CallInfoByCallId(subCollateral_df$call)

# resource_df & adjusted availAsset_df
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)

#### Run Substitution ##################
# scenario 1: sufficient new asset
# result
result1 <- ManualSubstitution(newResource,resource_df,availAsset_df,oldResource, oldResourceAmount,amountSelect,subCollateral_df)
result1

# scenario 2: insufficient new asset
trimResource_df <- resource_df
trimResource_df$qtyOri <- resource_df$qtyOri/1000
trimResource_df$qtyMin <- resource_df$qtyMin/1000
# result
result2 <- ManualSubstitution(newResource,trimResource_df,availAsset_df,oldResource, oldResourceAmountUSD,amountSelect,subCollateral_df)


