# Project: one(or more) for one asset substitution via optimization
# Create: 27 Nov 2017
# Author: Yuan Silin
# Functionality: Substitute the settled collateral with different asset(s) 
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
# 2. user preference: 
# 2.1. pref_vec: vector, the objective scores for cost and liquidity
# 2.2. operLimit: operation limit, the maximum assets the user can use to substitute the current collateral
# 3. availAsset_df
# 4. assetInfo_df
# 5. callInfo_df 
###

# set path
setwd("E:/ACUO/projects/acuo-allocation/")

#### Sources Start #########
source('src/substitution/dbExecutionForLocalTests.R')
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')
source('src/coreAlgo.R')
source('src/callLpSolve.R')
source('src/generalFunctions.R')
source('src/substitution/substitutionOptimization.R')
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
# 2.1: pref_vec
pref_vec <- c(5.4,3.5)

# 2.2: operLimit, oper
operLimit <- 2
operLimitMs_vec <- operLimit # only one margin call
fungible <- TRUE

# 3: availAsset_df
availAsset_df <- AvailAssetByCallIdAndClientId(clientId = clientId, callId = subCollateral_df$call)

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

# 4: assetInfo_df
assetId_vec <- unique(SplitResource(resource_vec,'asset'))
assetInfo_df <- AssetInfoByAssetId(assetId_vec)

# use the static fx in local DB
fxRate_df <- FxRates(assetInfo_df$currency)
assetInfo_df$FXRate <- as.double(fxRate_df$fxRate[match(assetInfo_df$currency,fxRate_df$currency)])

# 5: callInfo_df
callInfo_df <- CallInfoByCallId(subCollateral_df$call)

# resource_df & adjusted availAsset_df
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)

#### Run Substitution ##################
# scenario 1: sufficient optimal assets
# result
result1 <- SubstitutionOptimization(algoVersion=2,availAsset_df,assetInfo_df,resource_df,callInfo_df,
                                   subCollateral_df,
                                  pref_vec,operLimit,operLimitMs_vec,fungible)

# scenario 2: insufficient optimal assets
trimResource_df <- resource_df
trimResource_df$qtyOri <- resource_df$qtyOri/1000
trimResource_df$qtyMin <- resource_df$qtyMin/1000
# result
result2 <- SubstitutionOptimization(algoVersion=2,availAsset_df,assetInfo_df,trimResource_df,callInfo_df,
                                    subCollateral_df,
                                    pref_vec,operLimit,operLimitMs_vec,fungible)


