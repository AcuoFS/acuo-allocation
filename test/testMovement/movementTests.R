
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
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",operLimitMs,fungible,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testMovement/callNumber10AssetNumberS20M20L10Movement2FResultList.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd812$Asset),c('SGD','EUR'))
  checkEquals(as.character(result$callOutput$mcusd821$Asset),c('GBP','FR0000578601'))
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'DE0001135069')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),c('FR0000578593','JPY','USD'))
  checkEquals(as.character(result$callOutput$mcusd531$Asset),c('SGD','CA13506ZEU45'))
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'SGD')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), 719619.6)
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), round(c(432578.0,122925.6),1))
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 71961.96)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(10582.06,1))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,1),round(0.8104986,1))
  checkEquals(result$resultAnalysis$movements,11)
}

testCallNumber6AssetNumberS20M20L10Movement4F <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
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
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",operLimitMs,fungible,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testMovement/callNumber10AssetNumberS20M20L10Movement4FResultList.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),c('AUD','FR0000578601','DE0001135069','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'AUD')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),c('SGD','CA13506ZEU45'))
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'SGD')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), 719619.6)
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), round(c(268045.6,400000.0,10000000.0,400000.0),1))
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 44211.96)
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(11024.5,1))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,1),round(0.8117472,1))
  checkEquals(result$resultAnalysis$movements,10)
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
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",operLimitMs,fungible,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testMovement/callNumber10AssetNumberS20M20L10Movement2TResultList.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('GBP','FR0000578601','DE0001135069','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),c('SGD','JPY'))
  checkEquals(as.character(result$callOutput$mcusd531$Asset),c('SGD','CA13506ZEU45'))
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'SGD')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), round(c(152286.7,400000.0,10000000.0,400000.0),1))
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), 442119.6)
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 71961.96)
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(11024.5,1))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,1),round(0.8117472,1))
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumber10AssetNumberS20M20L10Movement4T <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
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
    output <- cbind("callNumber10AssetNumberS20M20L10Movement4T.RData",operLimitMs,fungible,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testMovement/callNumber10AssetNumberS20M20L10Movement4TResultList.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('GBP','FR0000578601','DE0001135069','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),c('SGD','JPY'))
  checkEquals(as.character(result$callOutput$mcusd531$Asset),c('SGD','CA13506ZEU45'))
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'SGD')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), round(c(152286.7,400000.0,10000000.0,400000.0),1))
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), 442119.6)
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 71961.96)
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(11024.5,1))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,1),round(0.8117472,1))
  checkEquals(result$resultAnalysis$movements,10)
}

