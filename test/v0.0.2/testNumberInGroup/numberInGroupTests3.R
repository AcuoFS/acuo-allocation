
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "testNumberInGroup/numberInGroupPerformance.xlsx"
loadFolder <- "testAssetAmount"

data1 <- "callNumber20AssetNumberS10M32L5SL3.RData"


testCallNumber20AssetNumberS10M32L5SL3Group1 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  inputLimit_vec <- c(1,1,1,1)
  timeLimit <- 10
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind(data1,inputLimit_vec[4],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber20AssetNumberS10M32L5SL3Group2 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  inputLimit_vec <- c(4,4,4,2)
  timeLimit <- 10*2
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind(data1,inputLimit_vec[4],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber20AssetNumberS10M32L5SL3Group3 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  inputLimit_vec <- c(6,6,6,3)
  timeLimit <- 10*3
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber20AssetNumberS10M32L5SL3.RData",inputLimit_vec[4],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber20AssetNumberS10M32L5SL3Group4 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  inputLimit_vec <- c(8,8,8,4)
  timeLimit <- 10*4
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber20AssetNumberS10M32L5SL3.RData",inputLimit_vec[4],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber20AssetNumberS10M32L5SL3Group5 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  inputLimit_vec <- c(10,10,10,5)
  timeLimit <- 10*5
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber20AssetNumberS10M32L5SL3.RData",inputLimit_vec[4],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber20AssetNumberS10M32L5SL3Group6 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  inputLimit_vec <- c(12,12,12,6)
  timeLimit <- 10*6
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber20AssetNumberS10M32L5SL3.RData",inputLimit_vec[4],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber20AssetNumberS10M32L5SL3Group7 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  inputLimit_vec <- c(14,14,14,7)
  timeLimit <- 10*7
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber20AssetNumberS10M32L5SL3.RData",inputLimit_vec[4],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber20AssetNumberS10M32L5SL3Group8 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  inputLimit_vec <- c(16,16,16,8)
  timeLimit <- 10*8
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber20AssetNumberS10M32L5SL3.RData",inputLimit_vec[4],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber20AssetNumberS10M32L5SL3Group9 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  inputLimit_vec <- c(18,18,18,9)
  timeLimit <- 10*9
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber20AssetNumberS10M32L5SL3.RData",inputLimit_vec[4],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber20AssetNumberS10M32L5SL3Group10 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  inputLimit_vec <- c(20,20,20,10)
  timeLimit <- 10*10
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber20AssetNumberS10M32L5SL3.RData",inputLimit_vec[4],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}
