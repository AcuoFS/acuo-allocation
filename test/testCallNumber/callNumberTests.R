
library("testthat")

testCallNumber0 <- function(){
  load("E:/ACUO/projects/acuo-allocation/test/testCallNumber/callNumber0.RData")
  expect_error( CallAllocation(algoVersion,scenario=1,callInfo_df,availAsset_df,resource_df,
                               pref_vec,operLimit,operLimitMs_vec,fungible,
                               ifNewAlloc=T,list(),inputLimit_vec,timeLimit,callOrderMethod,minMoveValue), "no rows to aggregate")
  #rm(list=ls())
}
