
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "testCashExternal/cashExternalPerformance.xlsx"
loadFolder <- "testCashExternal/RData"
saveFolder <- "testCashExternal/Result"
data1 <- "callNumber10AssetNumberS20M20L10"
data2 <- "callNumber10AssetNumberS10M35L5"
externalCostRange <- c(0.07,0.35)/10000
interval <- 0.02/10000

testAllExternalData1 <- function(loadFile=data1){
  loadPath <- paste(paste(loadFolder,loadFile,sep = '/'),"RData",sep=".")
  load(loadPath)
  
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  resultList <- list()
  
  low <- floor((min(availAsset_df$externalCost) - externalCostRange[1])/interval)
  up <- floor((externalCostRange[2]-max(availAsset_df$externalCost))/interval)
  if(low<=0){
    changes <- c(0,1:up)
  } else{
    changes <- c(-(low:1),0,1:up)
  }
  
  oriAvailAsset_df <- availAsset_df
  for(i in 1:length(changes)){
    change <- changes[i]
    availAsset_df$externalCost <- availAsset_df$externalCost + interval*change
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    
    avgExternal_df <- aggregate(externalCost~cashOrNoncash,data=availAsset_df,mean)
    avgCashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='cash')]
    avgNoncashExternal <- avgExternal_df$externalCost[which(avgExternal_df$cashOrNoncash=='noncash')]
    output <- cbind(paste(loadFile,"RData",sep='.'),timeLimit,avgCashExternal,avgNoncashExternal,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2+i-1,startCol=1,header=F)
    availAsset_df <- oriAvailAsset_df
  }
  saveFile <- paste(loadFile,'All',"ResultList.RData",sep='')
  savePath <- paste(saveFolder,saveFile,sep = '/')
  save(resultList,file=savePath)
  
}

testCashExternalData1 <- function(loadFile=data1){
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(238.2707,2))
  checkEquals(result$resultAnalysis$movements,10)
}

testCashExternalData2 <- function(loadFile=data2){
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
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'FR0000578593')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001135069')
  checkEquals(result$callOutput$mcusd811$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd821$Quantity, 41522989)
  checkEquals(result$callOutput$mcusd512$Quantity, 44211.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 196025)
  #checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(238.2707,2))
  checkEquals(result$resultAnalysis$movements,10)
}

