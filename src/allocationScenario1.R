AllocationScenario1 <- function(callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
                                algoVersion,ifNewAlloc,allocated_list,minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod){
  # Scenario1: algo suggestion
  #
  #### Allocate Calls by Groups ##################
  result <- AllocateByGroups(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,ifNewAlloc,allocated_list,
                             minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod)
  
  #### Return Allocation and Analysis Result #######
  return(result)
}