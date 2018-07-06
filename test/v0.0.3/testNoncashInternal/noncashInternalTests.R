
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "testNoncashInternal/noncashInternalPerformance.xlsx"
loadFolder <- "testNoncashInternal/RData"
saveFolder <- "testNoncashInternal/Result"
data1 <- "callNumber10AssetNumberS20M20L10"
data2 <- "callNumber10AssetNumberS10M35L5"

testNoncashInternalData1 <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)

  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                    pref_vec,operLimit,operLimitMs_vec,fungible,
                                    ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind(paste(loadFile,"RData",sep='.'),timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)

  saveFile <- paste(loadFile,"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
  
  checkEquals(as.character(result$callOutput$mcusd811$Asset),'GB0030883150')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),'GB0030883150')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'SG31A1000000')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'SG7Y76964295')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'GB0030883150')
  checkEquals(result$callOutput$mcusd811$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd821$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 77)
  #checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(223.2087,2))
  checkEquals(result$resultAnalysis$movements,10)
}

testNoncashInternalData2 <- function(loadFile=data2){
  # result doesnt match
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  
  temp <- proc.time() - ptm
  runTime <- temp[3]
  analysis <- result$resultAnalysis
  output <- cbind(paste(loadFile,"RData",sep='.'),timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  saveFile <- paste(loadFile,"Result.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save.image(savePath)
  
  checkEquals(as.character(result$callOutput$mcusd811$Asset),'GB0030883606')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),'GB0030883606')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'GB0030883150')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'GB0030883150')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001135069')
  checkEquals(result$callOutput$mcusd811$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd821$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 4152299)
  #checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(247.4752,2))
  checkEquals(result$resultAnalysis$movements,10)
}

