
#### Sources Start #########
setwd("E:/ACUO/projects/acuo-allocation/test/v0.0.3/")
source("src/generalFunctions.R")
library("XLConnect")

#### Sources END ###########

callInfoPath <- "testCashInternal/callInfo.xlsx"
assetInfoPath <- "testCashInternal/assetInfo.xlsx"
availAssetPath <- "testCashInternal/availAsset.xlsx"
externalCostPath <- "testCashInternal/externalCost.xlsx"

callInfoWorkbook <- loadWorkbook(callInfoPath)
assetInfoWorkbook <- loadWorkbook(assetInfoPath)
availAssetWorkbook <- loadWorkbook(availAssetPath)
externalCostWorkbook <- loadWorkbook(externalCostPath)

#### Input Prepare Start ###########
inputLimit_vec <- c(7,7,7,4)
timeLimit <- 10
callOrderMethod <- 3
minMoveValue <- 0
algoVersion <- 2
pref_vec <- c(5,5)
fungible <- FALSE

# read data
usdCallInfo_df <- readWorksheet(callInfoWorkbook,sheet ="USD",startRow = 1,endRow = 200,header=TRUE)
assetInfo_df <- readWorksheet(assetInfoWorkbook,sheet ="assets",header=TRUE)
usdAvailAsset_df <- readWorksheet(availAssetWorkbook,sheet ="USD",header=TRUE)
externalCost_df <- readWorksheet(externalCostWorkbook,sheet="Sheet1",header=TRUE)

assetCustacId_vec <- PasteResource(usdAvailAsset_df$assetId,usdAvailAsset_df$CustodianAccount)
usdAvailAsset_df$assetCustacId <- assetCustacId_vec

## TWO assets amount combination ##
# 20 small, 20 medium, 10 large(6VM)
tempAvailAsset_df <- usdAvailAsset_df
tempAvailAsset_df$quantity[c(1:6,8:13,15:18)] <- usdAvailAsset_df$quantity[c(1:6,8:13,15:18)]*10
tempAvailAsset_df$quantity[c(19:28)] <- usdAvailAsset_df$quantity[c(19:28)]/10
availAssetS20M20L10_df <- tempAvailAsset_df

# 10 small, 35 medium, 5 large(4VM)
tempAvailAsset_df <- usdAvailAsset_df
tempAvailAsset_df$quantity[c(1:4,8:11,15)] <- usdAvailAsset_df$quantity[c(1:4,8:11,15)]*10
tempAvailAsset_df$quantity[c(19:38)] <- usdAvailAsset_df$quantity[c(19:38)]/10
availAssetS10M35L5_df <- tempAvailAsset_df

rm("tempAvailAsset_df")

# names
assetScenarios <- c("S20M20L10","S10M35L5")

# call number = 10, asset number = 50
callInfo_df <- usdCallInfo_df[c(61:66,101:104),] # callAmount = 50000
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
for(i in 1:length(assetScenarios)){
  availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
  availAsset_df$cashOrNoncash <- rep("",0)
  thisAvailAsset_df <- get(paste("availAsset",assetScenarios[i],"_df",sep=""))
  for(m in 1:length(callInfo_df$id)){
    tempAvailAsset_df <- thisAvailAsset_df[which(thisAvailAsset_df$callType==callInfo_df$marginType[m]),]
    tempAvailAsset_df$callId <- callInfo_df$id[m]
    
    # assign the external cost
    cashOrNoncash_vec <- assetInfo_df$cashOrNoncash[match(tempAvailAsset_df$assetId,assetInfo_df$id)]
    tempAvailAsset_df$cashOrNoncash <- cashOrNoncash_vec # for simplicity of cost setting
    cptyIdx <- which(callInfo_df$Cpty[m]==externalCost_df$Cpty)
    tempAvailAsset_df$externalCost <- externalCost_df[cptyIdx,]$externalCost[match(cashOrNoncash_vec,externalCost_df$cashOrNoncash[cptyIdx])]
    
    # combine
    availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
  }
  resource_vec <- unique(availAsset_df$assetCustacId)
  resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
  availAsset_df <- cbind(AvailAsset(availAsset_df),cashOrNoncash=availAsset_df$cashOrNoncash)
  rm(tempAvailAsset_df)
  rm(thisAvailAsset_df)
  save.image(paste("testCashInternal/RData/callNumber10AssetNumber",assetScenarios[i],".RData",sep=""))
}



