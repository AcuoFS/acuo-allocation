
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "testObjectives/objectivesPerformance.xlsx"
loadFolder <- "testAssetAmount"

pref1 <- c(10,0)
pref2 <- c(8,2)
pref3 <- c(5,5)
pref4 <- c(2,8)
pref5 <- c(0,10)

data1 <- "callNumber10AssetNumberS10M32L5SL3.RData"

testCallNumber10AssetNumberS10M32L5SL3Pref1 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  pref_vec <- pref1
  timeLimit <- 10
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind(data1,pref_vec[1],pref_vec[2],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber10AssetNumberS10M32L5SL3Pref2 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  pref_vec <- pref2
  timeLimit <- 10
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber10AssetNumberS10M32L5SL3.RData",pref_vec[1],pref_vec[2],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber10AssetNumberS10M32L5SL3Pref3 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  pref_vec <- pref3
  timeLimit <- 10
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind(data1,pref_vec[1],pref_vec[2],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber10AssetNumberS10M32L5SL3Pref4 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  pref_vec <- pref4
  timeLimit <- 10
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind(data1,pref_vec[1],pref_vec[2],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber10AssetNumberS10M32L5SL3Pref5 <- function(loadFile=data1){
  loadPath <- paste(loadFolder,loadFile,sep = '/')
  load(loadPath)
  pref_vec <- pref5
  timeLimit <- 10
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind(data1,pref_vec[1],pref_vec[2],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}