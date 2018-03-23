
library("testthat")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
source("src/generalFunctions.R")

filePath <- "test/testCallNumber/callNumberPerformance.xlsx"

testCallNumber0 <- function(){
  load("test/testCallNumber/callNumber0.RData")
  expect_error( CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                               pref_vec,operLimit,operLimitMs_vec,fungible,
                               ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue), "no rows to aggregate")
}


testCallNumberVM1 <- function(){
  load("test/testCallNumber/callNumberVM1.RData")
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
  checkEquals(result$resultAnalysis$dailyCost,20)
}

testCallNumberVM2 <- function(){
  load("test/testCallNumber/callNumberVM2.RData")
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
  checkEquals(result$resultAnalysis$dailyCost,40)
}

testCallNumberVM3 <- function(){
  load("test/testCallNumber/callNumberVM3.RData")
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
  checkEquals(result$resultAnalysis$dailyCost,60)
}

testCallNumberVM6 <- function(){
  load("test/testCallNumber/callNumberVM6.RData")
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(121.7391,2))
}

testCallNumberVM10 <- function(){
  load("test/testCallNumber/callNumberVM10.RData")
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(205.2174,2))
}

testCallNumberVM15 <- function(){
  load("test/testCallNumber/callNumberVM15.RData")
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(308.6957,2))
}

testCallNumberVM20 <- function(){
  load("test/testCallNumber/callNumberVM20.RData")
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(412.174,2))
}

testCallNumberMediumVM3 <- function(){
  load("test/testCallNumber/callNumberMediumVM3.RData")
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(617.3913,2))
  checkEquals(result$resultAnalysis$movements,5)
}

testCallNumberVM1IM1 <- function(){
  load("test/testCallNumber/callNumberVM1IM1.RData")
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
  checkEquals(result$resultAnalysis$dailyCost,40)
  checkEquals(result$resultAnalysis$movements,1)
}

testCallNumberVM2IM2 <- function(){
  load("test/testCallNumber/callNumberVM2IM2.RData")
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
  checkEquals(result$resultAnalysis$dailyCost,80)
  checkEquals(result$resultAnalysis$movements,2)
}

testCallNumberVM3IM3 <- function(){
  load("test/testCallNumber/callNumberVM3IM3.RData")
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(121.7391,2))
  checkEquals(result$resultAnalysis$movements,4)
}

testCallNumberVM6IM6 <- function(){
  load("test/testCallNumber/callNumberVM6IM6.RData")
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(246.9566,2))
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumberVM6IM6 <- function(){ ## Why suggest USD for VM but SGD for IM ???
  load("test/testCallNumber/callNumberVM6IM6.RData")
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(246.9566,2))
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumberVM10IM10 <- function(){ ## Why suggest USD for VM but SGD for IM ???
  load("test/testCallNumber/callNumberVM10IM10.RData")
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(412.174,2))
  checkEquals(result$resultAnalysis$movements,17)
}

testCallNumberMediumVM2IM1 <- function(){ ## Why suggest USD for VM but SGD for IM ???
  load("test/testCallNumber/callNumberMediumVM2IM1.RData")
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
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(617.3913,2))
  checkEquals(result$resultAnalysis$movements,4)
}

