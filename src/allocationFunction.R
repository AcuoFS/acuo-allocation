
AllocationAlgo <- function(callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimitMs,fungible,
                           algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod,
                           ifNewAlloc,allocated_list){
  
  #### Handle Extreme Scenarios ##################
  ## 1. movement limit for one or several margin statements is 1
  if(operLimitMs==1){
    availAsset_df <- HandleStatementMovementLimitIsOne(availAsset_df,callInfo_df,resource_df)
  }
  
  #### Allocate Calls by Groups ##################
  result <- AllocateByGroups(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod,
                             ifNewAlloc,allocated_list)
  
  #### Return Allocation Result ##################
  return(result)
}
