# to test the integer ratio without movement constraints
# 1. we need to change the code in CoreAlgo to remove the movement constraints
# 2. change the asset minimum unit value to adjust the integer ratio

source("test/testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R")
source("src/allocationFunction.R")
source("test/testIntegerRatioWithoutMovementConstraint/coreAlgoIntegerRatio.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "test/testIntegerRatioWithoutMovementConstraint/integerRatioWithoutMovementConstraintPerformance.xlsx"

pref1 <- c(10,0)
pref2 <- c(8,2)
pref3 <- c(5,5)
pref4 <- c(2,8)
pref5 <- c(0,10)

SetWithoutMovementIntegerRatio <- function(resource_df,ratio){
  if(ratio==1){
    idx <- 1:length(resource_df$id)
    times <- resource_df$minUnitValue[idx]/1
  } else{
    idx <- 1:round((1-ratio)*length(resource_df$id))
    times <- resource_df$minUnitValue[idx]/0.1
  }
  resource_df$minUnitValue[idx] <- resource_df$minUnitValue[idx]/times
  resource_df$minUnit[idx] <- resource_df$minUnit[idx]/times
  resource_df$qtyMin[idx] <- resource_df$qtyMin[idx]*times
  return(resource_df)
}

testCallNumber10AssetNumberS20M20L10WithoutMovementInteger0 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  source("test/testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R")
  
  ratio <- 0
  resource_df <- SetWithoutMovementIntegerRatio(resource_df,ratio)
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result<- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                    pref_vec,operLimit,operLimitMs_vec,fungible,
                                    ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber10AssetNumberS20M20L10.RData",costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)

  save(result,runTime,file="test/testIntegerRatioWithoutMovementConstraint/callNumber10AssetNumberS20M20L10WithoutMovementInteger0Result.RData")
}

testCallNumber10AssetNumberS20M20L10WithoutMovementInteger0Pref1 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  source("test/testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R")
  
  ratio <- 0
  resource_df <- SetWithoutMovementIntegerRatio(resource_df,ratio)
  pref_vec <- pref1
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                    pref_vec,operLimit,operLimitMs_vec,fungible,
                                    ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber10AssetNumberS20M20L10.RData",costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)

  save(result,runTime,file="test/testIntegerRatioWithoutMovementConstraint/callNumber10AssetNumberS20M20L10WithoutMovementInteger0Pref1Result.RData")
}

testCallNumber10AssetNumberS20M20L10WithoutMovementInteger100 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  source("test/testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R")
  
  ratio <- 100/100
  resource_df <- SetWithoutMovementIntegerRatio(resource_df,ratio)
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                    pref_vec,operLimit,operLimitMs_vec,fungible,
                                    ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber10AssetNumberS20M20L10.RData",costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)

  save(result,runTime,file="test/testIntegerRatioWithoutMovementConstraint/callNumber10AssetNumberS20M20L10WithoutMovementInteger100Result.RData")
}

testCallNumber10AssetNumberS20M20L10WithoutMovementInteger100Pref1 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  source("test/testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R")
  
  ratio <- 100/100
  resource_df <- SetWithoutMovementIntegerRatio(resource_df,ratio)
  pref_vec <- pref1
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                    pref_vec,operLimit,operLimitMs_vec,fungible,
                                    ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber10AssetNumberS20M20L10.RData",costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)

  save(result,runTime,file="test/testIntegerRatioWithoutMovementConstraint/callNumber10AssetNumberS20M20L10WithoutMovementInteger100Pref1Result.RData")
}

testCallNumber10AssetNumberS20M35L5WithoutMovementInteger0 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
  source("test/testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R")
  
  ratio <- 0
  resource_df <- SetWithoutMovementIntegerRatio(resource_df,ratio)
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                    pref_vec,operLimit,operLimitMs_vec,fungible,
                                    ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber10AssetNumberS10M35L5.RData",costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)

  save(result,runTime,file="test/testIntegerRatioWithoutMovementConstraint/callNumber10AssetNumberS10M35L5WithoutMovementInteger0Result.RData")
}

testCallNumber10AssetNumberS20M35L5WithoutMovementInteger0Pref1 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
  source("test/testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R")
  
  ratio <- 0
  resource_df <- SetWithoutMovementIntegerRatio(resource_df,ratio)
  pref_vec <- pref1
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                    pref_vec,operLimit,operLimitMs_vec,fungible,
                                    ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber10AssetNumberS10M35L5.RData",costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)

  save(result,runTime,file="test/testIntegerRatioWithoutMovementConstraint/callNumber10AssetNumberS10M35L5WithoutMovementInteger0Pref1Result.RData")
}

testCallNumber10AssetNumberS20M35L5WithoutMovementInteger100 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
  source("test/testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R")
  
  ratio <- 100/100
  resource_df <- SetWithoutMovementIntegerRatio(resource_df,ratio)
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                    pref_vec,operLimit,operLimitMs_vec,fungible,
                                    ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber10AssetNumberS10M35L5.RData",costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)

  save(result,runTime,file="test/testIntegerRatioWithoutMovementConstraint/callNumber10AssetNumberS10M35L5WithoutMovementInteger100Result.RData")
}

testCallNumber10AssetNumberS20M35L5WithoutMovementInteger100Pref1 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
  source("test/testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R")
  
  ratio <- 100/100
  resource_df <- SetWithoutMovementIntegerRatio(resource_df,ratio)
  pref_vec <- pref1
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                    pref_vec,operLimit,operLimitMs_vec,fungible,
                                    ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind("callNumber10AssetNumberS10M35L5.RData",costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)

  save(result,runTime,file="test/testIntegerRatioWithoutMovementConstraint/callNumber10AssetNumberS10M35L5WithoutMovementInteger100Pref1Result.RData")
}

