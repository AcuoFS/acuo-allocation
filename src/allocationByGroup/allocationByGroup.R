AllocateByGroups <- function(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod,
                             ifNewAlloc,allocated_list){  
  # Improve Algo performance by allocating a few calls each time instead of the entire list
  # and then updating the quantity used for assets after each iteration, until all calls are allocated
  #
  # Args(Direct Use): 
  #   inputLimit_vec: number of calls and number of statements in a group
  #
  # Args(Indirect Use -- pass to other functions)
  #   
  #
  # Returns:  
  #   The allocation result 
  
  #### Initiate the Returned Variables #########
  callOutput_list <- list()
  msOutput_list <- list()
  checkCall_mat <- matrix(c(callInfo_df$callAmount,rep(0,length(callInfo_df$id))),nrow=length(callInfo_df$id), 
                          dimnames = list(callInfo_df$id,c('callAmount','fulfilledAmount')))
  
  #### Group the Margin Calls ###################
  groupCallId_list <- GroupCallIdByMs(callLimit=inputLimit_vec[3],msLimit=inputLimit_vec[4],callInfo_df,callOrderMethod)
  
  #### Iterate the Groups, Run Algo ####
  for(i in 1:length(groupCallId_list)){
    
    callIdGroup_vec <- groupCallId_list[[i]]
    msIdGroup_vec <- unique(callInfo_df$marginStatement[which(callInfo_df$id %in% callIdGroup_vec)])
    
    # *estimate the movement limits for the groups
    ratio <- length(msIdGroup_vec)/length(msId_vec) # the proportion of the msGroup in the msList
    operLimitGroup <- operLimit*ratio
    
    idxTemp_vec <- match(msIdGroup_vec,msId_vec)
    operLimitGroupMs_vec <- operLimitMs_vec[idxTemp_vec]
    
    callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
    availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
    
    updatedInfo <- UpdateResourceInfoAndAvailAsset(resource_df,availAssetGroup_df,length(callIdGroup_vec))
    resourceGroup_df <- updatedInfo$resource_df
    availAssetGroup_df <- updatedInfo$availAsset_df
    
    availInfoGroup_list <- AssetByCallInfo(callIdGroup_vec,resourceGroup_df$id,availAssetGroup_df,resourceGroup_df)
    
    if(ifNewAlloc){
      allocatedGroup_list <- list()
    } else{
      idxTemp_vec <- match(callIdGroup_vec,names(allocated_list))
      allocatedGroup_list <- allocated_list[idxTemp_vec]
    }
    
    #### Call AllocateAndCompareResults ####
    groupResult <- AllocateAndCompareResults(callInfoGroup_df,availAssetGroup_df,resourceGroup_df,
                                             pref_vec,operLimitMs,fungible,
                                             algoVersion,minMoveValue,timeLimit,
                                             availInfoGroup_list,ifNewAlloc,allocatedGroup_list)
    
    
    #### Quantity Update ##########
    # update the resource_df quantity, rounding
    quantityUsed_vec <- UsedQtyFromResultList(groupResult$callOutput_list,resource_df$id,callInfo_df$id)
    resource_df$qtyMin <- round(resource_df$qtyMin - quantityUsed_vec/resource_df$minUnit,4)
    
    for(k in 1:length(callIdGroup_vec)){
      callId <- callIdGroup_vec[k]
      msId <- callInfo_df$marginStatement[which(callInfo_df$id==callId)]
      callOutput_list[[callId]] <- groupResult$callOutput_list[[callId]]
      msOutput_list[[msId]] <- groupResult$msOutput_list[[msId]]
      checkCallGroup_mat <- groupResult$checkCall_mat
      checkCall_mat[which(rownames(checkCall_mat)==callId),2] <- checkCallGroup_mat[which(rownames(checkCallGroup_mat)==callId),2]
    }
  }
  
  #### Return Allocation Result #################
  return(list(callOutput_list=callOutput_list,msOutput_list=msOutput_list,checkCall_mat=checkCall_mat,
              solverStatus=groupResult$solverStatus,solverObjValue=groupResult$solverObjValue))
}

AllocateAndCompareResults <- function(callInfo_df,availAsset_df,resource_df,
                                      pref_vec,operLimitMs,fungible,
                                      algoVersion,minMoveValue,timeLimit,
                                      availInfo_list,ifNewAlloc,allocated_list){
  #### PreAllocation Allocation #################
  preAllocateResult <- PreAllocation(callInfo_df,availAsset_df,resource_df,
                                     pref_vec,operLimitMs,fungible,
                                     algoVersion,minMoveValue,timeLimit,
                                     ifNewAlloc,allocated_list)
  
  initAllocation_list <- preAllocateResult$callOutput_list # currently, store all the cumulated margin calls
  
  #### CoreAlgo Allocation ######################
  if(algoVersion==1){
    resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)#,initAllocation_list)
  } else if(algoVersion==2){
    operLimitMs_vec <- rep(operLimitMs,msNum)
    coreAlgoResult <- CoreAlgoV2(callInfo_df, resource_df, availInfo_list,
                             pref_vec,operLimit,operLimitMs_vec,fungible,
                             minMoveValue,timeLimit,ifNewAlloc,initAllocation_list,allocated_list)
  }
  
  #### Result Selection #########################
  # select the better result between PreAllocation and CoreAlgo
  betterResult <- ResultSelect(preAllocateResult, coreAlgoResult,availAsset_df,availAsset_df,resource_df,resource_df,callInfo_df,pref_vec)
  
  #### Return Allocation Result #################
  return(betterResult)
}

PreAllocation <- function(callInfo_df,availAsset_df,resource_df,
                          pref_vec,operLimitMs,fungible,
                          algoVersion,minMoveValue,timeLimit,
                          ifNewAlloc,allocated_list){
  ## callInfo_df: in group
  msId_vec <- unique(callInfo_df$marginStatement)
  callNum <- length(callId_vec)
  
  resource_vec <- unique(availAsset_df$assetCustacId)
  
  callOutput_list <- list()
  checkCall_mat <- matrix(c(callInfo_df$callAmount,rep(0,callNum)),nrow=callNum, dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
  
  for(i in 1:length(msId_vec)){
    msId <- msId_vec[i]
    callIdx_vec <- which(callInfo_df$marginStatement==msId)
    callInThisMs_vec <- callInfo_df$id[callIdx_vec]
    
    callInfoGroup_df <- callInfo_df[match(callInThisMs_vec,callInfo_df$id),]
    availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callInThisMs_vec),]
    
    updatedInfo <- UpdateResourceInfoAndAvailAsset(resource_df,availAssetGroup_df,length(callInThisMs_vec))
    resourceGroup_df <- updatedInfo$resource_df
    availAssetGroup_df <- updatedInfo$availAsset_df
    
    availInfoGroup_list <- AssetByCallInfo(callInThisMs_vec,resourceGroup_df$id,availAssetGroup_df,resourceGroup_df)
    
    idxTemp_vec <- match(callInThisMs_vec,names(allocated_list))
    allocatedGroup_list <- allocated_list[idxTemp_vec]
    # core Algo, assume all data comes in a list
    if(algoVersion==1){
      resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)
    } else if(algoVersion==2){
      resultGroup_list <- CoreAlgoV2(callInfoGroup_df, resourceGroup_df, availInfoGroup_list,
                                     pref_vec,operLimitMs,operLimitMs,fungible,
                                     minMoveValue,timeLimit,ifNewAlloc,list(),allocatedGroup_list)
    }
    
    #msOutputGroup_list <- resultGroup_list$msOutput_list
    callOutputGroup_list <- resultGroup_list$callOutput_list
    solverStatus <- resultGroup_list$solverStatus
    solverObjValue <- resultGroup_list$solverObjValue
    checkCallGroup_mat <- resultGroup_list$checkCall_mat
    
    for(k in 1:length(callInThisMs_vec)){
      callId <- callInThisMs_vec[k]
      msId <- callInfo_df$marginStatement[which(callInfo_df$id==callId)]
      callOutput_list[[callId]] <- callOutputGroup_list[[callId]]
      #msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
      checkCall_mat[which(rownames(checkCall_mat)==callId),2] <- checkCallGroup_mat[which(rownames(checkCallGroup_mat)==callId),2]
    }
    ## update the quantity in  resource_df
    quantityUsed_vec <- UsedQtyFromResultList(callOutputGroup_list,resource_df$id,callId_vec)
    resource_df$qtyMin <- resource_df$qtyMin - quantityUsed_vec/resource_df$minUnit
  }
  resultPre_list <- list(checkCall_mat=checkCall_mat,callOutput_list=callOutput_list)
  return(resultPre_list)
}
