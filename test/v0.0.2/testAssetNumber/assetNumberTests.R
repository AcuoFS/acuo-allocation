
library("testthat")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
source("src/generalFunctions.R")

filePath <- "testAssetNumber/assetNumberPerformance.xlsx"

loadFolder <- "testAssetNumber"

data1 <- "callNumber1AssetNumber1.RData"
data2 <- "callNumber1AssetNumber3.RData"
data3 <- "callNumber1AssetNumber6.RData"
data4 <- "callNumber1AssetNumber10.RData"
data5 <- "callNumber1AssetNumber30.RData"
data6 <- "callNumber3AssetNumber3.RData"
data7 <- "callNumber3AssetNumber6.RData"
data8 <- "callNumber3AssetNumber20.RData"
data9 <- "callNumber3AssetNumber30.RData"
data10 <- "callNumberMedium3AssetNumber10.RData"
data11 <- "callNumberMedium3AssetNumber30.RData"
data12 <- "callNumberMedium6AssetNumber30.RData"
data13 <- "callNumberLarge3AssetNumber30.RData"

testCallNumber1AssetNumber1 <- function(loadFile=data1){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'SGD')
  checkEquals(result$callOutput$mcusd211$Quantity,7196.2)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(1.086957,2))
}

testCallNumber1AssetNumber3 <- function(loadFile=data2){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'USD')
  checkEquals(result$callOutput$mcusd211$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,1)
}

testCallNumber1AssetNumber6 <- function(loadFile=data3){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'GBP')
  checkEquals(result$callOutput$mcusd211$Quantity,3926.64)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(1.086959,2))
}

testCallNumber1AssetNumber10 <- function(loadFile=data4){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  # pref=c(5,5); haircut_usd=0,haircut_gbp=0.08; dailyCost_usd = 20, dailyCost_GBP = 21.74
  # Algo choose GBP
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'GB0030883606')
  checkEquals(result$callOutput$mcusd211$Quantity,415230)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(0.6321841,2))
}

testCallNumber1AssetNumber30 <- function(loadFile=data5){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  # pref=c(5,5); haircut_usd=0,haircut_FR0000570905=0.14; dailyCost_usd = 20, dailyCost_FR0000570905 = 34.88
  # Algo choose FR0000570905
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'FR0000570905')
  checkEquals(result$callOutput$mcusd211$Quantity,18919)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(0.6395482,2))
}

testCallNumber3AssetNumber3 <- function(loadFile=data6){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd201$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd212$Asset),'USD')
  checkEquals(result$callOutput$mcusd211$Quantity,5000)
  checkEquals(result$callOutput$mcusd212$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,3)
}

testCallNumber3AssetNumber6 <- function(loadFile=data7){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd212$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd201$Asset),'GBP')
  checkEquals(result$callOutput$mcusd211$Quantity,3926.64)
  checkEquals(result$callOutput$mcusd212$Quantity,3926.64)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(3.260878,2))
}

testCallNumber3AssetNumber20 <- function(loadFile=data8){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'FR0000570905')
  checkEquals(as.character(result$callOutput$mcusd212$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd201$Asset),'GBP')
  checkEquals(result$callOutput$mcusd211$Quantity,18919)
  checkEquals(result$callOutput$mcusd212$Quantity,3926.64)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(2.813467,2))
}

testCallNumber3AssetNumber30 <- function(loadFile=data9){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'FR0000570905')
  checkEquals(as.character(result$callOutput$mcusd212$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd201$Asset),'GBP')
  checkEquals(result$callOutput$mcusd211$Quantity,18919)
  checkEquals(result$callOutput$mcusd212$Quantity,3926.64)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(2.813467,2))
}

testMediumCallNumber3AssetNumber10 <- function(loadFile=data10){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000570905')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'GBP')
  checkEquals(result$callOutput$mcusd511$Quantity,189187)
  checkEquals(result$callOutput$mcusd512$Quantity,39266.31)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(28.13451,2))
  checkEquals(result$resultAnalysis$movements,3)
}

testMediumCallNumber3AssetNumber30 <- function(loadFile=data11){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000570905')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'GBP')
  checkEquals(result$callOutput$mcusd511$Quantity,189187)
  checkEquals(result$callOutput$mcusd501$Quantity,44211.96)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(28.13451,2))
  checkEquals(result$resultAnalysis$movements,3)
}

testMediumCallNumber6AssetNumber30 <- function(loadFile=data12){
  load(paste(loadFolder,loadFile,sep='/'))  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(loadFile,length(callInfo_df$id),length(resource_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'FR0000570897')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001108603')
  checkEquals(result$callOutput$mcusd531$Quantity,4729652)
  checkEquals(result$callOutput$mcusd522$Quantity,44211.96)
  checkEquals(result$callOutput$mcusd532$Quantity,39266.31)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(51.79481,2))
  checkEquals(result$resultAnalysis$movements,6)
}

testLargeCallNumber3AssetNumber30 <- function(loadFile=data13){
  load(paste(loadFolder,loadFile,sep='/'))  
  expect_error(CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                              pref_vec,operLimit,operLimitMs_vec,fungible,
                              ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue),"The model constructed by margin calls mcusd811 mcusd812 is infeasible")
}
