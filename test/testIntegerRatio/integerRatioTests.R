
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

filePath <- "test/testIntegerRatio/integerRatioPerformance.xlsx"

SetIntegerRatio <- function(resource_df,ratio){
  if(ratio==1){
    idx <- 1:length(resource_df$id)
    times <- resource_df$minUnitValue[idx]/1
  } else{
    idx <- 1:round((1-ratio)*length(resource_df$id))
    times <- resource_df$minUnitValue[idx]/0.1
  }
  resource_df$minUnitValue[idx] <- resource_df$minUnitValue[idx]/times
  resource_df$minUnit[idx] <- resource_df$minUnit[idx]/times
  resource_df$qtyMin[idx] <- resource_df$qtyMin[idx]*times
  return(resource_df)
}

testCallNumber10AssetNumberS20M20L10Integer0 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  
  ratio <- 0
  resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS20M20L10Integer0ResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS20M20L10Integer50 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  
  ratio <- 50/100
  resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS20M20L10Integer50ResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS20M20L10Integer100 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  
  ratio <- 100/100
  resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS20M20L10Integer100ResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS20M20L10IntegerOri <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS20M20L10.RData")
  
  ratio <- length(which(resource_df$minUnitValue>=1))/length(resource_df$id)
  #resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS20M20L10.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS20M20L10IntegerOriResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS20M35L5Integer0 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
  
  ratio <- 0
  resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS10M35L5.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS10M35L5Integer0ResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS20M35L5Integer50 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
  
  ratio <- 50/100
  resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS10M35L5.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS10M35L5Integer50ResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS20M35L5Integer100 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
  
  ratio <- 100/100
  resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS10M35L5.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS10M35L5Integer100ResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS20M35L5IntegerOri <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M35L5.RData")
  
  ratio <- length(which(resource_df$minUnitValue>=1))/length(resource_df$id)
  #resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS10M35L5.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS10M35L5IntegerOriResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS10M32L5SL3Integer0 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M32L5SL3.RData")
  
  ratio <- 0
  resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS10M32L5SL3.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS10M32L5SL3Integer0ResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS10M32L5SL3Integer50 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M32L5SL3.RData")
  
  ratio <- 50/100
  resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS10M32L5SL3.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS10M32L5SL3Integer50ResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS10M32L5SL3Integer100 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M32L5SL3.RData")
  
  ratio <- 100/100
  resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS10M32L5SL3.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS10M32L5SL3Integer100ResultList.RData")
  result <- resultList[[1]]
}

testCallNumber10AssetNumberS10M32L5SL3IntegerOri <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberS10M32L5SL3.RData")
  
  ratio <- length(which(resource_df$minUnitValue>=1))/length(resource_df$id)
  #resource_df <- SetIntegerRatio(resource_df,ratio)
  
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
    output <- cbind("callNumber10AssetNumberS10M32L5SL3.RData",ratio,timeLimit,runTime[i],analysis$dailyCost,analysis$reservedLiquidityRatio,analysis$movements)
    worksheet <- readWorksheetFromFile(filePath,sheet="Results")
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=length(worksheet[,1])+2,startCol=1,header=F)
  }
  save(resultList,runTime,file="test/testIntegerRatio/callNumber10AssetNumberS10M32L5SL3IntegerOriResultList.RData")
  result <- resultList[[1]]
}
