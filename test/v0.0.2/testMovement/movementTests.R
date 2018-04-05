
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
  
  saveFile <- paste(loadFile,"Movement",operLimitMs,ifelse(fungible==T,"T","F"),"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
}

testCallNumber10AssetNumberS20M20L10Movement2F <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
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
  analysis <- result$resultAnalysis
  output <- cbind(paste(loadFile,"RData",sep='.'),operLimitMs,fungible,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  saveFile <- paste(loadFile,"Movement",operLimitMs,ifelse(fungible==T,"T","F"),"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)

  checkEquals(as.character(result$callOutput$mcusd811$Asset),'GB0030883267')
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'AUD')
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'FR0000578593')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'GBP')
  checkEquals(result$callOutput$mcusd811$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd821$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd512$Quantity, 39266.31)
  checkEquals(result$callOutput$mcusd521$Quantity, 196025)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(396.3164,1))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,1),round(0.8145923,1))
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumber6AssetNumberS20M20L10Movement4F <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
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
  analysis <- result$resultAnalysis
  output <- cbind(paste(loadFile,"RData",sep='.'),operLimitMs,fungible,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  saveFile <- paste(loadFile,"Movement",operLimitMs,ifelse(fungible==T,"T","F"),"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
  
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('GB0030883267','FR0000578601','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'AUD')
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'JP1051271G37')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),c('JP1103121AC2','JP1051271G37'))
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'CA135087WL43')
  checkEquals(round(result$callOutput$mcusd812$Quantity,1), 691141.3)
  checkEquals(result$callOutput$mcusd512$Quantity, 50000)
  checkEquals(result$callOutput$mcusd521$Quantity, c(76,49))
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(396.2995,2))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,2),round(0.8148616,2))
  checkEquals(result$resultAnalysis$movements,15)
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
  
  saveFile <- paste(loadFile,"Movement",operLimitMs,ifelse(fungible==T,"T","F"),"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
}

testCallNumber10AssetNumberS20M20L10Movement2T <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  operLimitMs <- 2
  fungible <- TRUE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))

  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                   pref_vec,operLimit,operLimitMs_vec,fungible,
                                   ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind(paste(loadFile,"RData",sep='.'),operLimitMs,fungible,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  saveFile <- paste(loadFile,"Movement",operLimitMs,ifelse(fungible==T,"T","F"),"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
  
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('GB0030883267','FR0000578601','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'AUD')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),'GB0030883267')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001135069')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), round(c(24576914,400000,400000),1))
  checkEquals(result$callOutput$mcusd821$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd512$Quantity, 50000)
  checkEquals(result$callOutput$mcusd521$Quantity, 50000)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(402.5675,1))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,1),round(0.8130713,1))
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumber10AssetNumberS20M20L10Movement4T <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  operLimitMs <- 4
  fungible <- TRUE
  operLimitMs_vec <- rep(operLimitMs,length(unique(callInfo_df$marginStatement)))

  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind(paste(loadFile,"RData",sep='.'),operLimitMs,fungible,timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  saveFile <- paste(loadFile,"Movement",operLimitMs,ifelse(fungible==T,"T","F"),"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
  
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('GB0030883267','FR0000578601','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'AUD')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),'GB0030883267')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001135069')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), round(c(24576914,400000,400000),1))
  checkEquals(result$callOutput$mcusd821$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd512$Quantity, 50000)
  checkEquals(result$callOutput$mcusd521$Quantity, 50000)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(402.5675,2))
  checkEquals(round(result$resultAnalysis$reservedLiquidityRatio,2),round(0.8130713,2))
  checkEquals(result$resultAnalysis$movements,10)
}

