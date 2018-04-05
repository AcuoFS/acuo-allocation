
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
source("src/generalFunctions.R")
library("testthat")
library("XLConnect")

filePath <- "testCallNumber/callNumberPerformance.xlsx"

data1 <- "callNumber0.RData"
data2 <- "callNumberVM1.RData"
data3 <- "callNumberVM2.RData"
data4 <- "callNumberVM3.RData"
data5 <- "callNumberVM6.RData"
data6 <- "callNumberVM10.RData"
data7 <- "callNumberVM15.RData"
data8 <- "callNumberVM20.RData"
data9 <- "callNumberMediumVM3.RData"
data10 <- "callNumberVM1IM1.RData"
data11 <- "callNumberVM2IM2.RData"
data12 <- "callNumberVM3IM3.RData"
data13 <- "callNumberVM6IM6.RData"
data14 <- "callNumberVM6IM6.RData"
data15 <- "callNumberVM10IM10.RData"
data16 <- "callNumberMediumVM2IM1.RData"

testCallNumber0 <- function(loadFile=data1){
  load(paste("testCallNumber",loadFile,sep='/'))
  expect_error( CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                               pref_vec,operLimit,operLimitMs_vec,fungible,
                               ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue), "no rows to aggregate")
}

testCallNumberVM1 <- function(loadFile=data2){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM1.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,1)
}

testCallNumberVM2 <- function(loadFile=data3){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM2.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
 
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd21$Asset),'USD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,2)
}

testCallNumberVM3 <- function(loadFile=data4){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM3.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd21$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd31$Asset),'USD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,3)
}

testCallNumberVM6 <- function(loadFile=data5){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM6.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd21$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd31$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd61$Asset),'SGD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$callOutput$mcusd61$Quantity,7196.2)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(6.086957,2))
}

testCallNumberVM10 <- function(loadFile=data6){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM10.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd21$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd31$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd51$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd71$Asset),'SGD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$callOutput$mcusd91$Quantity,7196.2)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(10.26087,2))
}

testCallNumberVM15 <- function(loadFile=data7){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM15.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd21$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd31$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd51$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd61$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd141$Asset),'SGD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$callOutput$mcusd91$Quantity,7196.2)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(15.43479,2))
}

testCallNumberVM20 <- function(loadFile=data8){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM20.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd21$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd31$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd51$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd81$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd181$Asset),'SGD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$callOutput$mcusd141$Quantity,7196.2)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(20.6087,2))
}

testCallNumberMediumVM3 <- function(loadFile=data9){
  # result doesnt match
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberMediumVM3.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd311$Asset),c('JPY','USD'))
  checkEquals(as.character(result$callOutput$mcusd321$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd331$Asset),c('SGD','USD'))
  checkEquals(result$callOutput$mcusd311$Quantity,c(100000.0, 49153.6))
  checkEquals(result$callOutput$mcusd331$Quantity,c(70743.80, 846.39))
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(30.86957,2))
  checkEquals(result$resultAnalysis$movements,5)
}

testCallNumberVM1IM1 <- function(loadFile=data10){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM1IM1.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd212$Asset),'USD')
  checkEquals(result$callOutput$mcusd211$Quantity,5000)
  checkEquals(result$callOutput$mcusd212$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,2)
  checkEquals(result$resultAnalysis$movements,1)
}

testCallNumberVM2IM2 <- function(loadFile=data11){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM2IM2.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd222$Asset),'USD')
  checkEquals(result$callOutput$mcusd211$Quantity,5000)
  checkEquals(result$callOutput$mcusd221$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,4)
  checkEquals(result$resultAnalysis$movements,2)
}

testCallNumberVM3IM3 <- function(loadFile=data12){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM3IM3.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd222$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd232$Asset),'SGD')
  checkEquals(result$callOutput$mcusd211$Quantity,5000)
  checkEquals(result$callOutput$mcusd221$Quantity,5000)
  checkEquals(result$callOutput$mcusd232$Quantity,7196.2)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(6.086957,2))
  checkEquals(result$resultAnalysis$movements,4)
}

testCallNumberVM6IM6 <- function(loadFile=data13){
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM6IM6.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd222$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd242$Asset),'SGD')
  checkEquals(result$callOutput$mcusd211$Quantity,5000)
  checkEquals(result$callOutput$mcusd221$Quantity,5000)
  checkEquals(result$callOutput$mcusd232$Quantity,7196.2)
  checkEquals(result$callOutput$mcusd262$Quantity,7196.2)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(12.34783,2))
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumberVM6IM6 <- function(loadFile=data14){ ## Why suggest USD for VM but SGD for IM ???
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM6IM6.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd222$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd242$Asset),'SGD')
  checkEquals(result$callOutput$mcusd211$Quantity,5000)
  checkEquals(result$callOutput$mcusd221$Quantity,5000)
  checkEquals(result$callOutput$mcusd232$Quantity,7196.2)
  checkEquals(result$callOutput$mcusd262$Quantity,7196.2)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(12.34783,2))
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumberVM10IM10 <- function(loadFile=data15){ ## Why suggest USD for VM but SGD for IM ???
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberVM10IM10.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd222$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd242$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd301$Asset),'SGD')
  checkEquals(result$callOutput$mcusd211$Quantity,5000)
  checkEquals(result$callOutput$mcusd221$Quantity,5000)
  checkEquals(result$callOutput$mcusd232$Quantity,7196.2)
  checkEquals(result$callOutput$mcusd262$Quantity,7196.2)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(20.6087,2))
  checkEquals(result$resultAnalysis$movements,17)
}

testCallNumberMediumVM2IM1 <- function(loadFile=data16){ ## Why suggest USD for VM but SGD for IM ???
  # result doesnt match
  load(paste("testCallNumber",loadFile,sep='/'))
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumberMediumVM2IM1.RData",length(callInfo_df$id),timeLimit,runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd501$Asset),c('SGD','USD'))
  checkEquals(result$callOutput$mcusd512$Quantity,c(100000.0,49153.6))
  checkEquals(result$callOutput$mcusd501$Quantity,c(70743.80,846.39))
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(30.86957,2))
  checkEquals(result$resultAnalysis$movements,4)
}

