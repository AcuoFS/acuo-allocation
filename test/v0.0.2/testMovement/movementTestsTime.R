
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "testMovement/movementPerformance.xlsx"

loadFolder <- "testAssetAmount"
saveFolder <- "testMovement"

data1 <- "callNumber10AssetNumberS20M20L10"

testCallNumber6AssetNumberS20M20L10Movement1F <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  operLimitMs <- 1
  fungible <- FALSE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  expect_error(result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                        pref_vec,operLimit,operLimitMs_vec,fungible,
                                        ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue),"ALERR2003: Asset inventory is insufficient") 
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(paste(loadFile,"RData",sep='.'),operLimitMs,fungible,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  # save.image("test/testMovement/callNumber10AssetNumberS20M20L10Movement1FResult.RData")
}

testCallNumber10AssetNumberS20M20L10Movement2F <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  operLimitMs <- 2
  fungible <- FALSE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))
  resultList <- list()
  runTime <- rep(0,5)
  for(i in 1:5){
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    timeLimit <- 10*i^2
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime[i] <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind(paste(loadFile,"RData",sep='.'),operLimitMs,fungible,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  saveFile <- paste(loadFile,"Movement",operLimitMs,ifelse(fungible==T,"T","F"),"ResultList.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(resultList,runTime,file=savePath)
}

testCallNumber6AssetNumberS20M20L10Movement4F <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  operLimitMs <- 4
  fungible <- FALSE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))
  resultList <- list()
  runTime <- rep(0,5)
  for(i in 1:5){
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    timeLimit <- 10*i^2
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime[i] <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind(paste(loadFile,"RData",sep='.'),operLimitMs,fungible,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  saveFile <- paste(loadFile,"Movement",operLimitMs,ifelse(fungible==T,"T","F"),"ResultList.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(resultList,runTime,file=savePath)
}

testCallNumber6AssetNumberS20M20L10Movement1T <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  operLimitMs <- 1
  fungible <- TRUE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  expect_error(result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                        pref_vec,operLimit,operLimitMs_vec,fungible,
                                        ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue),"ALERR2003: Asset inventory is insufficient") 
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind(paste(loadFile,"RData",sep='.'),operLimitMs,fungible,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  # save.image("test/testMovement/callNumber10AssetNumberS20M20L10Movement1TResult.RData")
}

testCallNumber10AssetNumberS20M20L10Movement2T <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  operLimitMs <- 2
  fungible <- TRUE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))
  resultList <- list()
  runTime <- rep(0,5)
  for(i in 1:5){
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    timeLimit <- 10*i^2
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    temp <- proc.time() - ptm
    runTime[i] <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind(paste(loadFile,"RData",sep='.'),operLimitMs,fungible,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  saveFile <- paste(loadFile,"Movement",operLimitMs,ifelse(fungible==T,"T","F"),"ResultList.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(resultList,runTime,file=savePath)
}

testCallNumber10AssetNumberS20M20L10Movement4T <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  operLimitMs <- 4
  fungible <- TRUE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))
  resultList <- list()
  runTime <- rep(0,5)
  for(i in 1:5){
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    timeLimit <- 10*i^2
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime[i] <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind(paste(loadFile,"RData",sep='.'),operLimitMs,fungible,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  saveFile <- paste(loadFile,"Movement",operLimitMs,ifelse(fungible==T,"T","F"),"ResultList.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(resultList,runTime,file=savePath)
}

