
#### Sources Start #########
setwd("E:/ACUO/projects/acuo-allocation/")
source("src/generalFunctions.R")

library("XLConnect")

#### Sources END ###########

callInfoPath <- "test/callInfo.xlsx"
assetInfoPath <- "test/assetInfo.xlsx"
availAssetPath <- "test/availAsset.xlsx"

callInfoWorkbook <- loadWorkbook(callInfoPath)
assetInfoWorkbook <- loadWorkbook(assetInfoPath)
availAssetWorkbook <- loadWorkbook(availAssetPath)


#### Input Prepare Start ###########
inputLimit_vec <- c(7,7,7,4)
timeLimit <- 1000
callOrderMethod <- 3
minMoveValue <- 0
algoVersion <- 2
fungible <- FALSE
pref_vec <- c(5,5)

# read data
callInfo_df <- readWorksheet(callInfoWorkbook,sheet ="USD",startRow = 1,endRow = 2,header=TRUE)
assetInfo_df <- readWorksheet(assetInfoWorkbook,sheet ="assets",header=TRUE)
availAsset_df <- readWorksheet(availAssetWorkbook,sheet ="USD",startRow = 1,endRow = 4,header=TRUE)

availAsset_df$callId <- callInfo_df$id
assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)

msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)

save.image("E:/ACUO/projects/acuo-allocation/test/basicParams.RData")

#### Input Prepare END #############
# source("src/allocationFunction.R")
# result1 <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
#                          pref_vec,operLimit,operLimitMs_vec,fungible,
#                          ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)

