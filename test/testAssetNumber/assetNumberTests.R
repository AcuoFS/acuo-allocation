
library("testthat")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
source("src/generalFunctions.R")

testCallNumber1AssetNumber1 <- function(){
  load("test/testAssetNumber/callNumber1AssetNumber1.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'SGD')
  checkEquals(result$callOutput$mcusd211$Quantity,7196.2)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(21.73914,2))
}

testCallNumber1AssetNumber3 <- function(){
  load("test/testAssetNumber/callNumber1AssetNumber3.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'USD')
  checkEquals(result$callOutput$mcusd211$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,20)
}

testCallNumber1AssetNumber6 <- function(){
  load("test/testAssetNumber/callNumber1AssetNumber6.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'GBP')
  checkEquals(result$callOutput$mcusd211$Quantity,3926.64)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(21.73918,2))
}

testCallNumber1AssetNumber10 <- function(){
  load("test/testAssetNumber/callNumber1AssetNumber10.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  # pref=c(5,5); haircut_usd=0,haircut_gbp=0.08; dailyCost_usd = 20, dailyCost_GBP = 21.74
  # Algo choose GBP
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'GBP')
  checkEquals(result$callOutput$mcusd211$Quantity,3926.64)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(21.73918,2))
}

testCallNumber1AssetNumber30 <- function(){
  load("test/testAssetNumber/callNumber1AssetNumber30.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  # pref=c(5,5); haircut_usd=0,haircut_FR0000570905=0.14; dailyCost_usd = 20, dailyCost_FR0000570905 = 34.88
  # Algo choose FR0000570905
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'FR0000570905')
  checkEquals(result$callOutput$mcusd211$Quantity,18919)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(34.88445,2))
}

testCallNumber3AssetNumber3 <- function(){
  load("test/testAssetNumber/callNumber3AssetNumber3.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd201$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd212$Asset),'USD')
  checkEquals(result$callOutput$mcusd211$Quantity,5000)
  checkEquals(result$callOutput$mcusd212$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,60)
}

testCallNumber3AssetNumber6 <- function(){
  load("test/testAssetNumber/callNumber3AssetNumber6.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd212$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd201$Asset),'GBP')
  checkEquals(result$callOutput$mcusd211$Quantity,3926.64)
  checkEquals(result$callOutput$mcusd212$Quantity,3926.64)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(65.21755,2))
}

testCallNumber3AssetNumber20 <- function(){
  load("test/testAssetNumber/callNumber3AssetNumber20.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd212$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd201$Asset),'GBP')
  checkEquals(result$callOutput$mcusd211$Quantity,3926.64)
  checkEquals(result$callOutput$mcusd212$Quantity,3926.64)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(65.21755,2))
}

testCallNumber3AssetNumber30 <- function(){
  load("test/testAssetNumber/callNumber3AssetNumber30.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd211$Asset),'FR0000570905')
  checkEquals(as.character(result$callOutput$mcusd212$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd201$Asset),'GBP')
  checkEquals(result$callOutput$mcusd211$Quantity,18919)
  checkEquals(result$callOutput$mcusd212$Quantity,3926.64)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(78.36282,2))
}

testMediumCallNumber3AssetNumber10 <- function(){
  load("test/testAssetNumber/callNumberMedium3AssetNumber10.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'EUR')
  checkEquals(result$callOutput$mcusd511$Quantity,39266.31)
  checkEquals(result$callOutput$mcusd512$Quantity,44211.96)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,652.174)
  checkEquals(result$resultAnalysis$movements,3)
}

testMediumCallNumber3AssetNumber30 <- function(){
  load("test/testAssetNumber/callNumberMedium3AssetNumber30.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),'FR0000570905')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'GBP')
  checkEquals(result$callOutput$mcusd511$Quantity,189187)
  checkEquals(result$callOutput$mcusd501$Quantity,44211.96)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(783.6216,2))
  checkEquals(result$resultAnalysis$movements,3)
}

testMediumCallNumber6AssetNumber30 <- function(){
  load("test/testAssetNumber/callNumberMedium6AssetNumber30.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd531$Asset),'DE0001108603')
  checkEquals(as.character(result$callOutput$mcusd512$Asset),'GBP')
  checkEquals(as.character(result$callOutput$mcusd521$Asset),'FR0000570897')
  checkEquals(result$callOutput$mcusd531$Quantity,4729652)
  checkEquals(result$callOutput$mcusd522$Quantity,44211.96)
  checkEquals(result$callOutput$mcusd532$Quantity,39266.31)
  checkEquals(result$solverStatus,-1)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(1698.689,2))
  checkEquals(result$resultAnalysis$movements,6)
}
