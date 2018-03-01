
library("testthat")

testCallNumber0 <- function(){
  load("test/testCallNumber/callNumber0.RData")
  expect_error( CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                               pref_vec,operLimit,operLimitMs_vec,fungible,
                               ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue), "no rows to aggregate")
}


testCallNumberVM1 <- function(){
  load("test/testCallNumber/callNumberVM1.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,20)
}

testCallNumberVM2 <- function(){
  load("test/testCallNumber/callNumberVM2.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd21$Asset),'USD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,40)
}

testCallNumberVM3 <- function(){
  load("test/testCallNumber/callNumberVM3.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd21$Asset),'USD')
  checkEquals(as.character(result$callOutput$mcusd31$Asset),'USD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,60)
}

testCallNumberVM6 <- function(){
  load("test/testCallNumber/callNumberVM6.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
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
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
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
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
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
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
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
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd311$Asset),c('SGD','USD'))
  checkEquals(as.character(result$callOutput$mcusd321$Asset),c('SGD','USD'))
  checkEquals(as.character(result$callOutput$mcusd331$Asset),c('JPY','USD'))
  checkEquals(result$callOutput$mcusd311$Quantity,c(69419.69, 1766.40))
  checkEquals(result$callOutput$mcusd331$Quantity,c(100000.0, 49153.6))
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(617.3913,2))
  checkEquals(result$resultAnalysis$movements,6)
}

testCallNumberVM1IM1 <- function(){
  load("test/testCallNumber/callNumberVM1IM1.RData")
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
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
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
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
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
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
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
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
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
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
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
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
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd511$Asset),c('SGD','USD'))
  checkEquals(as.character(result$callOutput$mcusd501$Asset),c('JPY','USD'))
  checkEquals(result$callOutput$mcusd512$Quantity,c(1324.1,49080.0))
  checkEquals(result$callOutput$mcusd501$Quantity,c(100000.0,49153.6))
  checkEquals(result$solverStatus,0)
  checkEquals(round(result$resultAnalysis$dailyCost,2),round(617.3913,2))
  checkEquals(result$resultAnalysis$movements,4)
}

