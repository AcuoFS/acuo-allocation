# Project: one for one asset manual substitution
# Create: 12 Dec 2017
# Author: Yuan Silin
# Functionality: Substitute the settled collateral with a with a new asset
# which may potentially reduce the costs of collateral based on the user preferences and constraints
#
# Inputs
#
# 1. newAsset
# 2. newCustodianAccount
# 3. newAssetAmount
# 4. availAsset_df
# 5. assetInfo_df
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
source('src/substitution/substitutionAdjustAmount.R')
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

# 1. newAsset
newAsset <- "SGD"

# 2. newCustodianAccount
newCustodianAccount <- "CustodianAccount1D"

newResource <- PasteResource(newAsset,newCustodianAccount)

# 3. newResourceAmount
newResourceAmount <- 59999.991
amountSelect <- "Amount"
#amountSelect <- "AdjAmount"

# 4: availAsset_df
resource_vec <- newResource
availAsset_df <- AvailAssetByCallIdAndClientId(clientId = clientId, callId = subCollateral_df$call)
availAsset_df$assetCustacId <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)

availAsset_df <- availAsset_df[match(newResource,assetCustacId_vec),]

# 5: assetInfo_df
assetId_vec <- newAsset
assetInfo_df <- AssetInfoByAssetId(newAsset)

# use the static fx in local DB
fxRate_df <- FxRates(assetInfo_df$currency)
assetInfo_df$FXRate <- as.double(fxRate_df$fxRate[match(assetInfo_df$currency,fxRate_df$currency)])

# resource_df & adjusted availAsset_df
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)


#### Run Substitution ##################
# scenario 1: sufficient new asset
# result
result <- SubstitutionAdjustAmount(newResource,newResourceAmount,amountSelect,resource_df,availAsset_df)
result


