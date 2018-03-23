
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "test/testObjectives/objectivesPerformance.xlsx"

pref1 <- c(10,0)
pref2 <- c(8,2)
pref3 <- c(5,5)
pref4 <- c(2,8)
pref5 <- c(0,10)

testCallNumber10AssetNumberS10M35L5Pref1 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
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
  output <- cbind("callNumber10AssetNumberS10M35L5.RData",pref_vec[1],pref_vec[2],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber10AssetNumberS10M35L5Pref2 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
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
  output <- cbind("callNumber10AssetNumberS10M35L5.RData",pref_vec[1],pref_vec[2],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber10AssetNumberS10M35L5Pref3 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
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
  output <- cbind("callNumber10AssetNumberS10M35L5.RData",pref_vec[1],pref_vec[2],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber10AssetNumberS10M35L5Pref4 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
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
  output <- cbind("callNumber10AssetNumberS10M35L5.RData",pref_vec[1],pref_vec[2],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}

testCallNumber10AssetNumberS10M35L5Pref5 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
  pref_vec <- pref5s
  timeLimit <- 10
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber10AssetNumberS10M35L5.RData",pref_vec[1],pref_vec[2],timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
}
