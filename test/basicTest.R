
#### Sources Start #########
setwd("E:/ACUO/projects/acuo-allocation/")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/generalFunctions.R")
source("src/callLpSolve.R")

library("XLConnect")

#### Sources END ###########

callInfoPath <- "test/callInfo.xlsx"
assetInfoPath <- "test/assetInfo.xlsx"
availAssetPath <- "test/availAsset.xlsx"

callInfoWorkbook <- loadWorkbook(callInfoPath)
assetInfoWorkbook <- loadWorkbook(assetInfoPath)
availAssetWorkbook <- loadWorkbook(availAssetPath)


#### Input Prepare Start ###########
# read data
callInfo_df <- readWorksheet(callInfoWorkbook,sheet ="USD",startRow = 1,endRow = 2,header=TRUE)
assetInfo_df <- readWorksheet(assetInfoWorkbook,sheet ="assets",header=TRUE)
availAsset_df <- readWorksheet(availAssetWorkbook,sheet ="headers",header=TRUE)

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)

inputLimit_vec <- c(7,7,7,4)
timeLimit <- 1000
callOrderMethod <- 3
minMoveValue <- 0


algoVersion <- 2
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
fungible <- FALSE
pref_vec <- c(5,5)


#### Input Prepare END #############

result1 <- CallAllocation(algoVersion,scenario=1,callId_vec,resource_vec,
                          callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                          ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)

