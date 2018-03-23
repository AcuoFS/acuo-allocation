
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")

testCallNumber1AssetNumberM50 <- function(){
  load("test/testAssetAmount/callNumber1AssetNumberM50.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(result$callOutput$mcusd511$Quantity,196025)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(361.4474,2))
  save.image("test/testAssetAmount/callNumber1AssetNumberM50Result.RData")
}

testCallNumber3AssetNumberM50 <- function(){
  load("test/testAssetAmount/callNumber3AssetNumberM50.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd501$Asset),'EUR')
  checkEquals(result$callOutput$mcusd511$Quantity,196025)
  checkEquals(result$callOutput$mcusd512$Quantity,39266.31)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(796.2301,2))
}

testCallNumber6AssetNumberM50 <- function(){
  load("test/testAssetAmount/callNumber6AssetNumberM50.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'FR0000578593')
  checkEquals(as.character(result$callOutput$mcusd522$Asset),'EUR')
  checkEquals(result$callOutput$mcusd511$Quantity,196025)
  checkEquals(result$callOutput$mcusd532$Quantity,39266.31)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,3),round(1736.515,3))
  save.image("test/testAssetAmount/callNumber6AssetNumberM50Result.RData")
}

testCallNumber10AssetNumberM50 <- function(){
  load("test/testAssetAmount/callNumber10AssetNumberM50.RData")
  expect_error(result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                                        pref_vec,operLimit,operLimitMs_vec,fungible,
                                        ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue),"ALERR2005: The model constructed by margin calls mcusd811 mcusd812 is infeasible") 
}


testCallNumber3AssetNumberS20M20L10 <- function(){
  load("test/testAssetAmount/callNumber3AssetNumberS20M20L10.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(result$callOutput$mcusd511$Quantity,196025)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(796.2301,2))
  save.image("test/testAssetAmount/callNumber3AssetNumberS20M20L10Result.RData")
}

testCallNumber6AssetNumberS20M20L10 <- function(){
  load("test/testAssetAmount/callNumber6AssetNumberS20M20L10.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001135069')
  checkEquals(result$callOutput$mcusd512$Quantity,39266.31)
  checkEquals(result$callOutput$mcusd521$Quantity, 196025)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,3),round(1736.515,3))
  save.image("test/testAssetAmount/callNumber6AssetNumberS20M20L10Result.RData")
}

testCallNumber3AssetNumberS10M35L5 <- function(){
  load("test/testAssetAmount/callNumber3AssetNumberS10M35L5.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'EUR')
  checkEquals(result$callOutput$mcusd501$Quantity,44211.96)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(796.2301,2))
  save.image("test/testAssetAmount/callNumber3AssetNumberS10M35L5Result.RData")
}


testCallNumber6AssetNumberS10M35L5 <- function(){
  load("test/testAssetAmount/callNumber6AssetNumberS10M35L5.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001135069')
  checkEquals(result$callOutput$mcusd512$Quantity,44211.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 196025)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,3),round(1736.515,3))
  save.image("test/testAssetAmount/callNumber6AssetNumberS10M35L5Result.RData")
}


testCallNumber3AssetNumberS10M32L5SL3 <- function(){
  load("test/testAssetAmount/callNumber3AssetNumberS10M32L5SL3.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(result$callOutput$mcusd512$Quantity,71961.96)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(796.2301,2))
  save.image("test/testAssetAmount/callNumber3AssetNumberS10M32L5SL3Result.RData")
}


testCallNumber6AssetNumberS10M32L5SL3 <- function(){
  load("test/testAssetAmount/callNumber6AssetNumberS10M32L5SL3.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000578601')
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001135069')
  checkEquals(result$callOutput$mcusd512$Quantity,71961.96)
  checkEquals(result$callOutput$mcusd521$Quantity, 196025)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,3),round(1736.515,3))
  save.image("test/testAssetAmount/callNumber6AssetNumberS10M32L5SL3Result.RData")
}
