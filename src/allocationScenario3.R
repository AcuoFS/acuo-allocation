AllocationScenario3 <- function(callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
                                algoVersion,ifNewAlloc,allocated_list,minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod){
  # Scenario3: allocate least liquid assets
  # To similate this scenario, we need to set the pref_vec to (0,10), which
  #           means only liquidity objective is taken into account
  #
  
  #### Adjust the Preference #####
  pref_vec <- c(0,10)
  
  #### Allocate Calls by Groups ##################
  result <- AllocateByGroups(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,ifNewAlloc,allocated_list,
                             minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod)
  
  #### Return Allocation and Analysis Result #######
  return(result)
}