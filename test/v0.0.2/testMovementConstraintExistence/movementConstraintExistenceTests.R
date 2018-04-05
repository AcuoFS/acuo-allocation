# to test the movement constraints
# 1. we need to change the code in CoreAlgo to remove/keep the movement constraints
# 2. change the asset minimum unit value to adjust the integer ratio

source("src/allocationFunction.R")
source("src/callLpSolve.R")
source("testIntegerRatioWithoutMovementConstraint/coreAlgoIntegerRatio.R")
source("testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R")
library("testthat")
library("XLConnect")

filePath <- "testMovementConstraintExistence/movementConstraintExistencePerformance.xlsx"
loadFolder <- "testAssetAmount"
saveFolder <- "testMovementConstraintExistence"

generalFunctionSrcPath <- "src/generalFunctions.R"
coreAlgoSrcPath <- "src/coreAlgo.R"
generalFunctionThisPath <- "testIntegerRatioWithoutMovementConstraint/generalFunctionsIntegerRatio.R"
coreAlgoThisPath <- "testIntegerRatioWithoutMovementConstraint/coreAlgoIntegerRatio.R"

pref1 <- c(10,0)
pref2 <- c(8,2)
pref3 <- c(5,5)
pref4 <- c(2,8)
pref5 <- c(0,10)

data1 <- "callNumber10AssetNumberS20M20L10"
data2 <- "callNumber10AssetNumberS10M35L5"

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


testCallNumber10AssetNumberS20M20L10WithMovementInteger100 <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  source(coreAlgoSrcPath)
  source(generalFunctionSrcPath)
  withConstraint = T
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
  output <- cbind(paste(loadFile,"RData",sep='.'),withConstraint,costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  saveFile <- paste(loadFile,"WithMovementInteger",round(ratio*100),"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
}

testCallNumber10AssetNumberS20M20L10WithoutMovementInteger100 <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  source(generalFunctionThisPath)
  source(coreAlgoThisPath)
  withConstraint <- F
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
  output <- cbind(paste(loadFile,"RData",sep='.'),withConstraint,costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  saveFile <- paste(loadFile,"WithMovementInteger",round(ratio*100),"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
}

testCallNumber10AssetNumberS10M35L5WithMovementInteger100 <- function(loadFile=data2){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  source(coreAlgoSrcPath)
  source(generalFunctionSrcPath)
  withConstraint = T
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
  output <- cbind(paste(loadFile,"RData",sep='.'),withConstraint,costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  saveFile <- paste(loadFile,"WithMovementInteger",round(ratio*100),"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
}

testCallNumber10AssetNumberS10M35L5WithoutMovementInteger100 <- function(loadFile=data2){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  source(generalFunctionThisPath)
  source(coreAlgoThisPath)
  withConstraint <- F
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
  output <- cbind(paste(loadFile,"RData",sep='.'),withConstraint,costObj=pref_vec[1],liquidityObj=pref_vec[2],ratio,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  saveFile <- paste(loadFile,"WithMovementInteger",round(ratio*100),"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
}
