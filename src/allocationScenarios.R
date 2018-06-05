AllocationScenario1 <- function(callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
                                algoVersion,controls,ifNewAlloc,allocated_list,minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod){
  # Scenario1: algo suggestion
  #
  #### Allocate Calls by Groups ##################
  result <- AllocateByGroups(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,controls,ifNewAlloc,allocated_list,
                             minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod)
  
  #### Return Allocation and Analysis Result #######
  return(result)
}

AllocationScenario2 <- function(callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
                                algoVersion,controls,ifNewAlloc,allocated_list,minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod){
  # Scenario2: allocate using settlement currency of the call
  # To similate this scenario, we need to remove the assets other than call settlement currency 
  # from both availAsset_df and resource_df
  #
  #
  #### Adjust availAsset_df and resource_df ######## 
  ## Keep Only Assets Same as Call Currency
  idxAssetKeep_vec <- vector() # to store assets idx with the same currency of the call
  
  for(i in 1:length(callInfo_df$id)){
    thisCallId <- callInfo_df$id[i]
    thisCallCcy <- callInfo_df$currency[i]
    idxAssetCallCcy_vec <- which(availAsset_df$callId==thisCallId & SplitResource(availAsset_df$resource,'asset')==thisCallCcy)
    if(length(idxAssetCallCcy_vec)>0){
      idxAssetKeep_vec <- append(idxAssetKeep_vec,idxAssetCallCcy_vec)
    } else{
      stop('ALERR2001: Settlement currency is not available(not in inventory/not eligible)!')
    }
  }
  ## Usable Resources
  availAsset_df <- availAsset_df[idxAssetKeep_vec,]
  resource_df <- resource_df[match(unique(availAsset_df$resource),resource_df$id),]
  
  #### Allocate Calls by Groups ##################
  result <- AllocateByGroups(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,controls,ifNewAlloc,allocated_list,
                             minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod)
  
  #### Return Allocation and Analysis Result #######
  return(result)
}

AllocationScenario3 <- function(callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
                                algoVersion,controls,ifNewAlloc,allocated_list,minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod){
  # Scenario3: allocate least liquid assets
  # To similate this scenario, we need to set the pref_vec to (0,10), which
  #           means only liquidity objective is taken into account
  #
  
  #### Adjust the Preference #####
  pref_vec <- c(0,10)
  
  #### Allocate Calls by Groups ##################
  result <- AllocateByGroups(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,controls,ifNewAlloc,allocated_list,
                             minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod)
  
  #### Return Allocation and Analysis Result #######
  return(result)
}