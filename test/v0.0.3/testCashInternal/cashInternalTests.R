
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "testCashInternal/cashInternalPerformance.xlsx"
loadFolder <- "testCashInternal/RData"
saveFolder <- "testCashInternal/Result"
data1 <- "callNumber10AssetNumberS20M20L10"
data2 <- "callNumber10AssetNumberS10M35L5"
cashInternalCost <- c(0.4,0.35,0.3,0.25,0.2,0.15,0.1)/10000

testCashInternalData1 <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)

  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  resultList <- list()
  
  # lower the internal cost of cash
  for(i in 1:length(cashInternalCost)){
    availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="cash")]<- cashInternalCost[i]
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimit,operLimitMs_vec,fungible,
                             ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind(paste(loadFile,"RData",sep='.'),timeLimit,cashInternalCost[i],runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2+i-1,startCol=1,header=F)
  }
  
  saveFile <- paste(loadFile,"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
  
  result <- resultList[[1]]
  
  checkEquals(as.character(result$callOutput$mcusd811$Asset),'GB0030883267')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),'GB0030883267')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'FR0000578593')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001135069')
  checkEquals(result$callOutput$mcusd811$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd821$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd512$Quantity, 39266.31)
  checkEquals(result$callOutput$mcusd521$Quantity, 196025)
  #checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(237.6729,2))
  checkEquals(result$resultAnalysis$movements,10)
}

testCashInternalData2 <- function(loadFile=data2){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  resultList <- list()
  
  # lower the internal cost of cash
  for(i in 1:length(cashInternalCost)){
    availAsset_df$internalCost[which(availAsset_df$cashOrNoncash=="cash")]<- cashInternalCost[i]
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind(paste(loadFile,"RData",sep='.'),timeLimit,cashInternalCost[i],runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2+i-1,startCol=1,header=F)
  }
  
  saveFile <- paste(loadFile,"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
  
  result <- resultList[[1]]
  
  checkEquals(as.character(result$callOutput$mcusd811$Asset),'GB0030883606')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),'GB0030883606')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'FR0000578593')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001135069')
  checkEquals(result$callOutput$mcusd811$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd821$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd512$Quantity, 44211.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 196025)
  #checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(237.6729,2))
  checkEquals(result$resultAnalysis$movements,10)
}

