
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "test/testMovement/movementPerformance.xlsx"

testCallNumber6AssetNumberS20M20L10Movement1F <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
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
  output <- cbind("callNumber10AssetNumberS20M20L10.RData",operLimitMs,fungible,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  save.image("test/testAssetAmount/callNumber10AssetNumberS20M20L10Movement1FResult.RData")
}

testCallNumber10AssetNumberS20M20L10Movement2F <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  operLimitMs <- 2
  fungible <- FALSE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumber10AssetNumberS20M20L10.RData",operLimitMs,fungible,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('EUR','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),c('GBP','FR0000578601'))
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'DE0001135069')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'DE0001135069')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),c('SGD','CA13506ZEU45'))
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'SGD')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), round(c(351902.2,400000.0),1))
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), round(c(312537.6,400000.0),1))
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 4900603)
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(11018.7,1))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,1),round(0.8117307,1))
  checkEquals(result$resultAnalysis$movements,10)
  save.image("test/testAssetAmount/callNumber10AssetNumberS20M20L10Movement2FResult.RData")
}

testCallNumber6AssetNumberS20M20L10Movement4F <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  operLimitMs <- 4
  fungible <- FALSE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumber10AssetNumberS20M20L10.RData",operLimitMs,fungible,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('SGD','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),c('SGD','AUD'))
  checkEquals(as.character(result$callOutput$mcusd821$Asset),c('EUR','FR0000578601','DE0001135069'))
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),c('SGD','JPY'))
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),c('EUR','CA13506ZEU45'))
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'EUR')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), round(c(572776.5,400000.0),1))
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), round(c(261684.8,400000.0,10000000.0),1))
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 71961.96)
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(11024.5,1))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,1),round(0.8117472,1))
  checkEquals(result$resultAnalysis$movements,11)
  save.image("test/testAssetAmount/callNumber10AssetNumberS20M20L10Movement4FResult.RData")
}


testCallNumber6AssetNumberS20M20L10Movement1T <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
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
  output <- cbind("callNumber10AssetNumberS20M20L10.RData",operLimitMs,fungible,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  save.image("test/testAssetAmount/callNumber10AssetNumberS20M20L10Movement1TResult.RData")
}

testCallNumber10AssetNumberS20M20L10Movement2T <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  operLimitMs <- 2
  fungible <- TRUE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))
  resultList <- list()
  for(i in 1:5){
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    timeLimit <- 10*i^2
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                     pref_vec,operLimit,operLimitMs_vec,fungible,
                                     ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList$resultAnalysis
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",operLimitMs,fungible,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('EUR','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),c('GBP','FR0000578601'))
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'DE0001135069')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'DE0001135069')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),c('SGD','CA13506ZEU45'))
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'SGD')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), round(c(351902.2,400000.0),1))
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), round(c(312537.6,400000.0),1))
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 4900603)
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(11018.7,1))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,1),round(0.8117307,1))
  checkEquals(result$resultAnalysis$movements,10)
  save.image("test/testAssetAmount/callNumber10AssetNumberS20M20L10Movement2TResult.RData")
}

testCallNumber10AssetNumberS20M20L10Movement4T <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  operLimitMs <- 4
  fungible <- TRUE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))
  for(i in 1:5){
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    timeLimit <- 10*i^2
    ptm <- proc.time()
    result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimit,operLimitMs_vec,fungible,
                             ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    output <- cbind("callNumber10AssetNumberS20M20L10Movement4T.RData",operLimitMs,fungible,timeLimit,runTime)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }

  
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('SGD','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),c('SGD','AUD'))
  checkEquals(as.character(result$callOutput$mcusd821$Asset),c('EUR','FR0000578601','DE0001135069'))
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),c('SGD','JPY'))
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),c('EUR','CA13506ZEU45'))
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'EUR')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), round(c(572776.5,400000.0),1))
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), round(c(261684.8,400000.0,10000000.0),1))
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 71961.96)
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(11024.5,1))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,1),round(0.8117472,1))
  checkEquals(result$resultAnalysis$movements,11)
  save.image("test/testAssetAmount/callNumber10AssetNumberS20M20L10Movement4FResult.RData")
}

