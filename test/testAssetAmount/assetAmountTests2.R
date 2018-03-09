
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "test/testAssetAmount/assetAmountPerformance.xlsx"

testCallNumber10AssetNumberS20M20L10 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  resultList <- list()
  for(i in 1:5){
    timeLimit <- 10*i^2
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimit,operLimitMs_vec,fungible,
                             ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,file="callNumber10AssetNumberS20M20L10ResultList.RData")
  result <- resultList[[1]]
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
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumber20AssetNumberS20M20L10 <- function(){
  load("test/testAssetAmount/callNumber20AssetNumberS20M20L10.RData")
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  expect_error(result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                        pref_vec,operLimit,operLimitMs_vec,fungible,
                                        ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue),"ALERR2005: The model constructed by margin calls mcusd851 mcusd852 is infeasible")
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumber20AssetNumberS20M20L10.RData",runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  #save.image("test/testAssetAmount/callNumber10AssetNumberS20M20L10Result.RData")
}


testCallNumber10AssetNumberS20M35L5 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
  resultList <- list()
  for(i in 1:5){
    timeLimit <- 10*i^2
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimit,operLimitMs_vec,fungible,
                             ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,file="callNumber10AssetNumberS10M35L5ResultLimit.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('EUR','FR0000578601'))
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
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumber20AssetNumberS10M35L5 <- function(){
  load("test/testAssetAmount/callNumber20AssetNumberS10M35L5.RData")
  worksheet <- readWorksheetFromFile(filePath,sheet="Results")
  ptm <- proc.time()
  expect_error(result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                        pref_vec,operLimit,operLimitMs_vec,fungible,
                                        ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue),"ALERR2005: The model constructed by margin calls mcusd841 mcusd842 is infeasible")
  temp <- proc.time() - ptm
  runTime <- temp[3]
  output <- cbind("callNumber20AssetNumberS10M35L5.RData",runTime)
  writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  save.image("test/testAssetAmount/callNumber20AssetNumberS10M35L5Result.RData")
}



testCallNumber10AssetNumberS10M32L5SL3 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M32L5SL3.RData")
  resultList <- list()
  for(i in 1:5){
    timeLimit <- 10*i^2
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind("callNumber10AssetNumberS10M32L5SL3.RData",timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,file="callNumber10AssetNumberS10M32L5SL3ResultList.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('EUR','FR0000578601'))
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
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumber20AssetNumberS10M32L5SL3 <- function(){
  load("test/testAssetAmount/callNumber20AssetNumberS10M32L5SL3.RData")
  resultList <- list()
  for(i in 1:5){
    timeLimit <- 10*i^2
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind("callNumber20AssetNumberS10M32L5SL3.RData",timeLimit,runTime,analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,file = "test/testAssetAmount/callNumber20AssetNumberS10M32L5SL3ResultList.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('EUR','FR0000578601'))
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
  checkEquals(result$resultAnalysis$movements,10)
}
