
#### Sources Start #########
setwd("E:/ACUO/projects/acuo-allocation/")
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
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
inputLimit_vec <- c(7,7,7,4)
timeLimit <- 10
callOrderMethod <- 3
minMoveValue <- 0
algoVersion <- 2
pref_vec <- c(5,5)


# read data
usdCallInfo_df <- readWorksheet(callInfoWorkbook,sheet ="USD",startRow = 1,endRow = 200,header=TRUE)
assetInfo_df <- readWorksheet(assetInfoWorkbook,sheet ="assets",header=TRUE)
usdAvailAsset_df <- readWorksheet(availAssetWorkbook,sheet ="USD",header=TRUE)

assetCustacId_vec <- PasteResource(usdAvailAsset_df$assetId,usdAvailAsset_df$CustodianAccount)
usdAvailAsset_df$assetCustacId <- assetCustacId_vec

## movement scenarios

# 1. 
fungible <- FALSE
operLimitMs <- 1

# 2. 
fungible <- FALSE
operLimitMs <- 2

# 3. 
fungible <- FALSE
operLimitMs <- 4

# 4. 
fungible <- TRUE
operLimitMs <- 1

# 5. 
fungible <- TRUE
operLimitMs <- 2

# 6. 
fungible <- TRUE
operLimitMs <- 4



msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(operLimitMs,msNum)
operLimit<- sum(operLimitMs_vec)



