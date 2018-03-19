
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
  runTime <- rep(0,5)
  for(i in 1:5){
    timeLimit <- 10*i^2
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimit,operLimitMs_vec,fungible,
                             ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime[i] <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testAssetAmount/callNumber10AssetNumberS20M20L10ResultList.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd812$Asset),c('SGD','EUR'))
  checkEquals(as.character(result$callOutput$mcusd821$Asset),c('GBP','FR0000578601'))
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'DE0001135069')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),c('SGD','JPY','USD'))
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'FR0000578593')
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'SUD')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), 719619.6)
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), round(c(312537.6,400000.0),1))
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 71961.96)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(10582.06,1))
  checkEquals(result$resultAnalysis$movements,11)
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
  runTime <- rep(0,5)
  for(i in 1:5){
    timeLimit <- 10*i^2
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimit,operLimitMs_vec,fungible,
                             ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime[i] <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind("callNumber10AssetNumberS10M35L5.RData",timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testAssetAmount/callNumber10AssetNumberS10M35L5ResultList.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('EUR','DE0001135069'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),'GB0030883606')
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'FR0000578593')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'SGD')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), round(c(351902.2,10000000.0),1))
  checkEquals(round(result$callOutput$mcusd821$Quantity), round(41522989))
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.97)
  checkEquals(result$callOutput$mcusd521$Quantity, 196025)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(11826.57,1))
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
  #output <- cbind("callNumber20AssetNumberS10M35L5.RData",runTime)
  #writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  save.image("test/testAssetAmount/callNumber20AssetNumberS10M35L5Result.RData")
}

testCallNumber10AssetNumberS10M32L5SL3 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M32L5SL3.RData")
  resultList <- list()
  runTime <- rep(0,5)
  for(i in 1:5){
    timeLimit <- 10*i^2
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime [i]<- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind("callNumber10AssetNumberS10M32L5SL3.RData",timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testAssetAmount/callNumber10AssetNumberS10M32L5SL3ResultList.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('SGD','FR0000578601'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd822$Asset),c('SGD','JPY'))
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'DE0001135069')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'DE0001135069')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'FR0000578593')
  checkEquals(as.character(result$callOutput$mcusd532$Asset),'SGD')
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), round(c(572776.5,400000.0),1))
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), 719619.6)
  checkEquals(result$callOutput$mcusd512$Quantity, 71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 4900603)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(10726.12,1))
  checkEquals(result$resultAnalysis$movements,10)
}

testCallNumber20AssetNumberS10M32L5SL3 <- function(){
  load("test/testAssetAmount/callNumber20AssetNumberS10M32L5SL3.RData")
  resultList <- list()
  runTime <- rep(0,5)
  for(i in 1:5){
    timeLimit <- 10*i^2
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    ptm <- proc.time()
    resultList[[i]] <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimit,operLimitMs_vec,fungible,
                                      ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    
    temp <- proc.time() - ptm
    runTime[i] <- temp[3]
    analysis <- resultList[[i]]$resultAnalysis
    output <- cbind("callNumber20AssetNumberS10M32L5SL3.RData",timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file = "test/testAssetAmount/callNumber20AssetNumberS10M32L5SL3ResultList.RData")
  result <- resultList[[1]]
  checkEquals(as.character(result$callOutput$mcusd811$Asset),c('SGD','CA13506ZEU45'))
  checkEquals(as.character(result$callOutput$mcusd812$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd821$Asset),c('SGD','DE0001135069'))
  checkEquals(as.character(result$callOutput$mcusd822$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd831$Asset),c('GBP','FR0000578601'))
  checkEquals(as.character(result$callOutput$mcusd832$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd841$Asset),c('EUR','FR0000578593'))
  checkEquals(as.character(result$callOutput$mcusd842$Asset),'EUR')
  checkEquals(as.character(result$callOutput$mcusd851$Asset),'SGD')
  checkEquals(as.character(result$callOutput$mcusd852$Asset),c('SGD','JPY'))
  checkEquals(as.character(result$callOutput$mcusd861$Asset),c('SGD','CA135087WL43'))
  checkEquals(round(result$callOutput$mcusd811$Quantity,1), c(624609, 100))
  checkEquals(round(result$callOutput$mcusd821$Quantity,1), round(c(572776.5,10000000.0),1))
  checkEquals(round(result$callOutput$mcusd862$Quantity,1), 719619.6)
  checkEquals(result$callOutput$mcusd521$Quantity, 71961.96)
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,1),round(33191.57,1))
  checkEquals(result$resultAnalysis$movements,17)
}
