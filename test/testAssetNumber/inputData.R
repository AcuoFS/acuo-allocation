
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
pref_vec <- c(5,5)
fungible <- FALSE

# read data
usdCallInfo_df <- readWorksheet(callInfoWorkbook,sheet ="USD",startRow = 1,endRow = 200,header=TRUE)
assetInfo_df <- readWorksheet(assetInfoWorkbook,sheet ="assets",header=TRUE)
usdAvailAsset_df <- readWorksheet(availAssetWorkbook,sheet ="USD",header=TRUE)

assetCustacId_vec <- PasteResource(usdAvailAsset_df$assetId,usdAvailAsset_df$CustodianAccount)
usdAvailAsset_df$assetCustacId <- assetCustacId_vec

# call number = 1, asset number = 1
callInfo_df <- usdCallInfo_df[21,]
availAsset_df <- usdAvailAsset_df[1,]
availAsset_df$callId <- callInfo_df$id
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber1AssetNumber1.RData")

# call number = 1, asset number = 3
callInfo_df <- usdCallInfo_df[21,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  tempAvailAsset_df <- tempAvailAsset_df[1:3,]
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber1AssetNumber3.RData")

# call number = 1, asset number = 6
callInfo_df <- usdCallInfo_df[21,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  tempAvailAsset_df <- tempAvailAsset_df[1:6,]
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber1AssetNumber6.RData")

# call number = 1, asset number = 10
callInfo_df <- usdCallInfo_df[21,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  tempAvailAsset_df <- tempAvailAsset_df[1:10,]
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber1AssetNumber10.RData")

# call number = 1, asset number = 30
callInfo_df <- usdCallInfo_df[21,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  tempAvailAsset_df <- tempAvailAsset_df[1:30,]
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber1AssetNumber30.RData")

# call number = 3, asset number = 3
callInfo_df <- usdCallInfo_df[20:22,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  tempAvailAsset_df <- tempAvailAsset_df[1:3,]
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber3AssetNumber3.RData")

# call number = 3, asset number = 6
callInfo_df <- usdCallInfo_df[20:22,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  tempAvailAsset_df <- tempAvailAsset_df[1:6,]
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber3AssetNumber6.RData")

# call number = 3, asset number = 20
callInfo_df <- usdCallInfo_df[20:22,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  if(length(tempAvailAsset_df[,1])>20){
    tempAvailAsset_df <- tempAvailAsset_df[1:20,]
  }
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber3AssetNumber20.RData")

# call number = 3, asset number = 30
callInfo_df <- usdCallInfo_df[20:22,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  if(length(tempAvailAsset_df[,1])>30){
    tempAvailAsset_df <- tempAvailAsset_df[1:30,]
  }
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber3AssetNumber30.RData")

# call number = 6, asset number = 6
callInfo_df <- usdCallInfo_df[21:26,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  if(length(tempAvailAsset_df[,1])>6){
    tempAvailAsset_df <- tempAvailAsset_df[1:6,]
  }
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber6AssetNumber6.RData")

# call number = 6, asset number = 12
callInfo_df <- usdCallInfo_df[21:26,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  if(length(tempAvailAsset_df[,1])>12){
    tempAvailAsset_df <- tempAvailAsset_df[1:12,]
  }
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber6AssetNumber12.RData")

# call number = 6, asset number = 20
callInfo_df <- usdCallInfo_df[21:26,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  if(length(tempAvailAsset_df[,1])>20){
    tempAvailAsset_df <- tempAvailAsset_df[1:20,]
  }
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumber6AssetNumber6.RData")

# call number medium = 3, asset number = 10
callInfo_df <- usdCallInfo_df[60:62,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  if(length(tempAvailAsset_df[,1])>10){
    tempAvailAsset_df <- tempAvailAsset_df[1:20,]
  }
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumberMedium3AssetNumber10.RData")

# call number medium = 3, asset number = 30
callInfo_df <- usdCallInfo_df[60:62,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  if(length(tempAvailAsset_df[,1])>30){
    tempAvailAsset_df <- tempAvailAsset_df[1:30,]
  }
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumberMedium3AssetNumber30.RData")

# call number medium = 6, asset number = 30
callInfo_df <- usdCallInfo_df[61:66,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  if(length(tempAvailAsset_df[,1])>30){
    tempAvailAsset_df <- tempAvailAsset_df[1:30,]
  }
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumberMedium6AssetNumber30.RData")

# call number large = 3, asset number = 30
callInfo_df <- usdCallInfo_df[100:102,]
availAsset_df <- usdAvailAsset_df[-(1:length(usdAvailAsset_df$callId)),]
for(i in 1:length(callInfo_df$id)){
  tempAvailAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType[i]),]
  if(length(tempAvailAsset_df[,1])>30){
    tempAvailAsset_df <- tempAvailAsset_df[1:30,]
  }
  tempAvailAsset_df$callId <- callInfo_df$id[i]
  availAsset_df <- rbind(availAsset_df,tempAvailAsset_df)
}
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
rm(tempAvailAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testAssetNumber/callNumberLarge3AssetNumber30.RData")



#### Input Prepare END #############

