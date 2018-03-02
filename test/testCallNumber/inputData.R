
#### Sources Start #########
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
timeLimit <- 1000
callOrderMethod <- 3
minMoveValue <- 0
algoVersion <- 2
pref_vec <- c(5,5)
fungible <- FALSE

# read data
usdCallInfo_df <- readWorksheet(callInfoWorkbook,sheet ="USD",startRow = 1,endRow = 100,header=TRUE)
assetInfo_df <- readWorksheet(assetInfoWorkbook,sheet ="assets",header=TRUE)
usdAvailAsset_df <- readWorksheet(availAssetWorkbook,sheet ="USD",header=TRUE)

assetCustacId_vec <- PasteResource(usdAvailAsset_df$assetId,usdAvailAsset_df$CustodianAccount)
usdAvailAsset_df$assetCustacId <- assetCustacId_vec

# VM call number = 0
callInfo_df <- usdCallInfo_df[-(1:length(usdCallInfo_df$id)),]
availAsset_df <- usdAvailAsset_df[1:3,]
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testCallNumber/callNumber0.RData")

# VM call number = 1
callInfo_df <- usdCallInfo_df[1,]
availAsset_df <- usdAvailAsset_df[which(usdAvailAsset_df$callType==callInfo_df$marginType),]
availAsset_df <- availAsset_df[1:3,]
availAsset_df$callId <- callInfo_df$id
resource_vec <- unique(availAsset_df$assetCustacId)
resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
save.image("test/testCallNumber/callNumberVM1.RData")

# VM call number = 2
callInfo_df <- usdCallInfo_df[1:2,]
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
save.image("test/testCallNumber/callNumberVM2.RData")

# VM call number = 3
callInfo_df <- usdCallInfo_df[1:3,]
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
save.image("test/testCallNumber/callNumberVM3.RData")

# VM call number = 6
callInfo_df <- usdCallInfo_df[1:6,]
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
save.image("test/testCallNumber/callNumberVM6.RData")

# VM call number = 10
callInfo_df <- usdCallInfo_df[1:10,]
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
save.image("test/testCallNumber/callNumberVM10.RData")

# VM call number = 15
callInfo_df <- usdCallInfo_df[1:15,]
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
save.image("test/testCallNumber/callNumberVM15.RData")

# VM call number = 20
callInfo_df <- usdCallInfo_df[1:20,]
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
save.image("test/testCallNumber/callNumberVM20.RData")

# Medium VM call number = 3
callInfo_df <- usdCallInfo_df[41:43,]
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
save.image("test/testCallNumber/callNumberMediumVM3.RData")

# VM call number = 1, IM call number = 1
callInfo_df <- usdCallInfo_df[21:22,]
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
save.image("test/testCallNumber/callNumberVM1IM1.RData")

# VM call number = 2, IM call number = 2
callInfo_df <- usdCallInfo_df[21:24,]
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
save.image("test/testCallNumber/callNumberVM2IM2.RData")

# VM call number = 3, IM call number = 3
callInfo_df <- usdCallInfo_df[21:26,]
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
save.image("test/testCallNumber/callNumberVM3IM3.RData")

# VM call number = 6, IM call number = 6
callInfo_df <- usdCallInfo_df[21:32,]
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
save.image("test/testCallNumber/callNumberVM6IM6.RData")

# VM call number = 10, IM call number = 10
callInfo_df <- usdCallInfo_df[21:40,]
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
save.image("test/testCallNumber/callNumberVM10IM10.RData")

# Medium VM call number = 2, Medium IM call number = 1
callInfo_df <- usdCallInfo_df[c(60:62),]
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
save.image("test/testCallNumber/callNumberMediumVM2IM1.RData")


#### Input Prepare END #############

