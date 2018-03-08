
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
timeLimit <- 3600
callOrderMethod <- 3
minMoveValue <- 0
algoVersion <- 2
pref_vec <- c(5,5)
fungible <- FALSE

# read data
usdCallInfo_df <- readWorksheet(callInfoWorkbook,sheet ="USD",startRow = 1,endRow = 200,header=TRUE)
assetInfo_df <- readWorksheet(assetInfoWorkbook,sheet ="assets",header=TRUE)
usdAvailAsset_df <- readWorksheet(availAssetWorkbook,sheet ="USD",header=TRUE)

assetCustacId_vec <- PasteResource(usdAvailAsset_df$assetId,usdAvailAsset_df$CustodianAccount)
usdAvailAsset_df$assetCustacId <- assetCustacId_vec

## 4 asset amount scenarios
# note that only 7 assets are available for VM

# 50 medium (orginal)
availAssetM50_df <- usdAvailAsset_df

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

# 10 small, 32 medium, 5 large(3VM), 3 super large(2VM)
tempAvailAsset_df <- usdAvailAsset_df
tempAvailAsset_df$quantity[c(1:2,8:9)] <- usdAvailAsset_df$quantity[c(1:2,8:9)]*10*10
tempAvailAsset_df$quantity[c(3:5,10:12,15:16)] <- usdAvailAsset_df$quantity[c(3:5,10:12,15:16)]*10
tempAvailAsset_df$quantity[c(19:38)] <- usdAvailAsset_df$quantity[c(19:38)]/10
availAssetS10M32L5SL3_df <- tempAvailAsset_df

rm("tempAvailAsset_df")

# names
assetScenarios <- c("M50","S20M20L10","S10M35L5","S10M32L5SL3")

# call number = 1, asset number = 50
callInfo_df <- usdCallInfo_df[61,] # callAmount = 50000
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(m in 1:length(callInfo_df$id)){
  for(i in 1:length(assetScenarios)){
    availAsset_df <- get(paste("availAsset",assetScenarios[i],"_df",sep=""))
    availAsset_df$callId <- callInfo_df$id[m]
    resource_vec <- unique(availAsset_df$assetCustacId)
    resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
    availAsset_df <- AvailAsset(availAsset_df)
    save.image(paste("test/testAssetAmount/callNumber1AssetNumber",assetScenarios[i],".RData",sep=""))
  }
}


# call number = 3, asset number = 50
callInfo_df <- usdCallInfo_df[60:62,] # callAmount = 50000
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
for(i in 1:length(assetScenarios)){
  availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
  thisAvailAsset_df <- get(paste("availAsset",assetScenarios[i],"_df",sep=""))
  for(m in 1:length(callInfo_df$id)){
    tempAvailAsset_df <- thisAvailAsset_df[which(thisAvailAsset_df$callType==callInfo_df$marginType[m]),]
    tempAvailAsset_df$callId <- callInfo_df$id[m]
    availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
  }
  resource_vec <- unique(availAsset_df$assetCustacId)
  resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
  availAsset_df <- AvailAsset(availAsset_df)
  rm(tempAvailAsset_df)
  rm(thisAvailAsset_df)
  save.image(paste("test/testAssetAmount/callNumber3AssetNumber",assetScenarios[i],".RData",sep=""))
}


# call number = 6, asset number = 50
callInfo_df <- usdCallInfo_df[61:66,] # callAmount = 50000
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
for(i in 1:length(assetScenarios)){
  availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
  thisAvailAsset_df <- get(paste("availAsset",assetScenarios[i],"_df",sep=""))
  for(m in 1:length(callInfo_df$id)){
    tempAvailAsset_df <- thisAvailAsset_df[which(thisAvailAsset_df$callType==callInfo_df$marginType[m]),]
    tempAvailAsset_df$callId <- callInfo_df$id[m]
    availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
  }
  resource_vec <- unique(availAsset_df$assetCustacId)
  resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
  availAsset_df <- AvailAsset(availAsset_df)
  rm(tempAvailAsset_df)
  rm(thisAvailAsset_df)
  save.image(paste("test/testAssetAmount/callNumber6AssetNumber",assetScenarios[i],".RData",sep=""))
}



# call number = 10, asset number = 50
callInfo_df <- usdCallInfo_df[c(61:66,101:104),] # callAmount = 50000
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
for(i in 1:length(assetScenarios)){
  availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
  thisAvailAsset_df <- get(paste("availAsset",assetScenarios[i],"_df",sep=""))
  for(m in 1:length(callInfo_df$id)){
    tempAvailAsset_df <- thisAvailAsset_df[which(thisAvailAsset_df$callType==callInfo_df$marginType[m]),]
    tempAvailAsset_df$callId <- callInfo_df$id[m]
    availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
  }
  resource_vec <- unique(availAsset_df$assetCustacId)
  resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
  availAsset_df <- AvailAsset(availAsset_df)
  rm(tempAvailAsset_df)
  rm(thisAvailAsset_df)
  save.image(paste("test/testAssetAmount/callNumber10AssetNumber",assetScenarios[i],".RData",sep=""))
}



# call number = 10, asset number = 50
callInfo_df <- usdCallInfo_df[c(61:66,101:114),] # callAmount = 50000
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
for(i in 1:length(assetScenarios)){
  availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
  thisAvailAsset_df <- get(paste("availAsset",assetScenarios[i],"_df",sep=""))
  for(m in 1:length(callInfo_df$id)){
    tempAvailAsset_df <- thisAvailAsset_df[which(thisAvailAsset_df$callType==callInfo_df$marginType[m]),]
    tempAvailAsset_df$callId <- callInfo_df$id[m]
    availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
  }
  resource_vec <- unique(availAsset_df$assetCustacId)
  resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
  availAsset_df <- AvailAsset(availAsset_df)
  rm(tempAvailAsset_df)
  rm(thisAvailAsset_df)
  save.image(paste("test/testAssetAmount/callNumber20AssetNumber",assetScenarios[i],".RData",sep=""))
}




