
source("src/generalFunctions.R")
source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")
library("testthat")
library("XLConnect")

# load basic parameters
load("E:/ACUO/projects/acuo-allocation/test/basicParams.RData")

testCallAmount0 <- function(){
  callInfo_df$callAmount <- 0
  expect_error( CallAllocation(algoVersion,scenario=1,
                               callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                               ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue), "ALERR3004: There's no asset allocated to margin call mcusd11")
}

testCallAmount1 <- function(){
  callInfo_df$callAmount <- 1
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                               pref_vec,operLimit,operLimitMs_vec,fungible,
                               ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(result$callOutput$mcusd11$Quantity,1)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,0.004)
}

testCallAmount5000 <- function(){
  callInfo_df$callAmount <- 5000
  result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
  checkEquals(as.character(result$callOutput$mcusd11$Asset),'USD')
  checkEquals(result$callOutput$mcusd11$Quantity,5000)
  checkEquals(result$solverStatus,-1)
  checkEquals(result$resultAnalysis$dailyCost,20)
}

testCallAmount170000 <- function(){
  callInfo_df$callAmount <- 170000
  expect_error( CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                               pref_vec,operLimit,operLimitMs_vec,fungible,
                               ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue), "ALERR2005: The model constructed by margin calls mcusd11 is infeasible")
}

testCallAmountRunTime <- function(){
  filePath <- "test/testCallAmount/callAmountPerformance.xlsx"
  callAmount_vec <- 5000*(1:33)
  runTime_vec <- rep(0,length(callAmount_vec))
  writeWorksheetToFile(filePath,data=cbind("CallAmount","RunTime"),sheet='Results',startRow=1,startCol=1,header=F)
  for(i in 1:length(callAmount_vec)){
    callInfo_df$callAmount <- callAmount_vec[i]
    ptm <- proc.time()
    result <- CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimit,operLimitMs_vec,fungible,
                             ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue)
    temp <- proc.time() - ptm
    runTime_vec[i] <- temp[3]

    output <- cbind(callAmount_vec[i],runTime_vec[i])
    writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=i+1,startCol=1,header=F)
  }
  
  # output <- data.frame(CallAmount=callAmount_vec,RunTime=runTime_vec)
  # writeWorksheetToFile(filePath,data=output,sheet='Results',startRow=1,startCol=1,header=F)
}
