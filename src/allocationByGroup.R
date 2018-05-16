AllocateByGroups <- function(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,ifNewAlloc,allocated_list,
                             minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod){  
  # Improve Algo performance by allocating a few calls each time instead of the entire list
  # and then update the quantity used for assets after each iteration, until all calls are allocated
  #
  # Args(Direct Use): 
  #   maxCallNum,maxMsNum: number of calls and number of statements in a group
  #
  # Args(Indirect Use -- pass to other functions)
  #   
  #
  # Returns:  
  #   The allocation result 
  
  #### Initiate the Returned Variables #########
  callOutput_list <- list()
  msOutput_list <- list()
  
  #### Group the Margin Calls ###################
  groupCallId_list <- GroupCallIdByMs(maxCallNum,maxMsNum,callInfo_df,callOrderMethod)
  
  #### Iterate the Groups, Run Algo ####
  for(i in 1:length(groupCallId_list)){
    callIdGroup_vec <- groupCallId_list[[i]]
    
    callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
    availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
    
    updatedInfo <- UpdateResourceInfoAndAvailAsset(resource_df,availAssetGroup_df,length(callIdGroup_vec))
    resourceGroup_df <- updatedInfo$resource_df
    availAssetGroup_df <- updatedInfo$availAsset_df
    
    if(ifNewAlloc){
      allocatedGroup_list <- list()
    } else{
      idxTemp_vec <- match(callIdGroup_vec,names(allocated_list))
      allocatedGroup_list <- allocated_list[idxTemp_vec]
    }
    cat("group:", i,"; avail length:",length(availAssetGroup_df$callId),'\n')
    #### Call AllocateAndCompareResults ####
    groupResult <- AllocateAndCompareResults(callInfoGroup_df,availAssetGroup_df,resourceGroup_df,
                                             pref_vec,operLimitMs,fungible,
                                             algoVersion,ifNewAlloc,allocatedGroup_list,minMoveValue,timeLimit)
    
    
    #### Quantity Update ##########
    # update the resource_df quantity, rounding
    quantityUsed_vec <- UsedQtyFromResultList(groupResult$callOutput_list,resource_df$id,callInfo_df$id)
    resource_df$qtyMin <- round(resource_df$qtyMin - quantityUsed_vec/resource_df$minUnit,4)
    
    for(k in 1:length(callIdGroup_vec)){
      callId <- callIdGroup_vec[k]
      msId <- callInfo_df$marginStatement[which(callInfo_df$id==callId)]
      callOutput_list[[callId]] <- groupResult$callOutput_list[[callId]]
      msOutput_list[[msId]] <- groupResult$msOutput_list[[msId]]
    }
  }
  
  #### Return Allocation Result #################
  return(list(callOutput_list=callOutput_list,msOutput_list=msOutput_list))
}

AllocateAndCompareResults <- function(callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimitMs,fungible,
                                      algoVersion,ifNewAlloc,allocated_list,minMoveValue,timeLimit){
  #### PreAllocation Allocation #################
  preAllocateResult <- PreAllocation(callInfo_df,availAsset_df,resource_df,
                                     pref_vec,operLimitMs,fungible,
                                     algoVersion,ifNewAlloc,allocated_list,minMoveValue,timeLimit)
  
  initAllocation_list <- preAllocateResult$callOutput_list # currently, store all the cumulated margin calls
  
  #### CoreAlgo Allocation ######################
  if(algoVersion==1){
    resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)#,initAllocation_list)
  } else if(algoVersion==2){
    coreAlgoResult <- CoreAlgoV2(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             ifNewAlloc,initAllocation_list,allocated_list,
                             minMoveValue,timeLimit)
  }
  
  #### Result Selection #########################
  # select the better result between PreAllocation and CoreAlgo
  betterResult <- ResultSelect(preAllocateResult, coreAlgoResult,availAsset_df,resource_df,callInfo_df,pref_vec)
  
  #### Return Allocation Result #################
  return(betterResult)
}

PreAllocation <- function(callInfo_df,availAsset_df,resource_df,
                          pref_vec,operLimitMs,fungible,
                          algoVersion,ifNewAlloc,allocated_list,
                          minMoveValue,timeLimit){
  ## callInfo_df: in group
  msId_vec <- unique(callInfo_df$marginStatement)
  callNum <- length(callId_vec)
  
  resource_vec <- unique(availAsset_df$assetCustacId)
  
  msOutput_list <- list()
  callOutput_list <- list()
  
  for(i in 1:length(msId_vec)){
    msId <- msId_vec[i]
    callIdx_vec <- which(callInfo_df$marginStatement==msId)
    callInThisMs_vec <- callInfo_df$id[callIdx_vec]
    
    callInfoGroup_df <- callInfo_df[match(callInThisMs_vec,callInfo_df$id),]
    availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callInThisMs_vec),]
    
    cat("pre group:", i,"; avail length:",length(availAssetGroup_df$callId),'\n')
    updatedInfo <- UpdateResourceInfoAndAvailAsset(resource_df,availAssetGroup_df,length(callInThisMs_vec))
    resourceGroup_df <- updatedInfo$resource_df
    availAssetGroup_df <- updatedInfo$availAsset_df
    
    idxTemp_vec <- match(callInThisMs_vec,names(allocated_list))
    allocatedGroup_list <- allocated_list[idxTemp_vec]
    # core Algo, assume all data comes in a list
    if(algoVersion==1){
      resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)
    } else if(algoVersion==2){
      resultGroup_list <- CoreAlgoV2(callInfoGroup_df,availAssetGroup_df, resourceGroup_df, 
                                     pref_vec,operLimitMs,fungible,
                                     ifNewAlloc,list(),allocatedGroup_list,
                                     minMoveValue,timeLimit)
    }
    
    msOutputGroup_list <- resultGroup_list$msOutput_list
    callOutputGroup_list <- resultGroup_list$callOutput_list
    
    for(k in 1:length(callInThisMs_vec)){
      callId <- callInThisMs_vec[k]
      msId <- callInfo_df$marginStatement[which(callInfo_df$id==callId)]
      callOutput_list[[callId]] <- callOutputGroup_list[[callId]]
      msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
    }
    ## update the quantity in  resource_df
    quantityUsed_vec <- UsedQtyFromResultList(callOutputGroup_list,resource_df$id,callId_vec)
    resource_df$qtyMin <- resource_df$qtyMin - quantityUsed_vec/resource_df$minUnit
  }
  resultPre_list <- list(callOutput_list=callOutput_list,msOutput_list=msOutput_list)
  return(resultPre_list)
}
