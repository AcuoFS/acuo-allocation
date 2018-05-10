
CallAllocation <- function(scenario,
                           callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimitMs,fungible,
                           algoVersion,ifNewAlloc,allocated_list,
                           minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod){
  # Deal with different scenarios by controling the input to the algo &
  # Analysize allocation result performance
  #
  # Args(Direct Use): 
  #
  # Args(Indirect Use -- pass to other functions)
  #   
  #
  # Returns:  
  #   The allocation result and analysis
  
  #### Scenario Code #########
  # scenario = 1, Algo suggestion
  # scenario = 2, post settlement cash only
  # scenario = 3, post least liquid assets
  
  #### Input Prepare #########

  #### Run Algo Under a Specific Scenario
  if(scenario==1){
    #### Allocation #################################
    result <- AllocationAlgo(callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
                             algoVersion,ifNewAlloc,allocated_list,
                             minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod)
    
    #### Analyze Allocation Result Performance ######
    resultAnalysis <- ResultAnalysis(availAsset_df,availAsset_df,resource_df,resource_df,callInfo_df,
                                     result$callOutput_list)
  } else if(scenario==2){
    
    # store the idx of the assets with the same currency of the call
    idxAssetKeep_vec <- vector()
    
    for(i in 1:length(callInfo_df$id)){
      thisCallId <- callInfo_df$id[i]
      thisCallCcy <- callInfo_df$currency[i]
      
      # find idx(in availAsset_df) of the assets with the same currency of the call in availAsset_df
      idxAssetCallCcy_vec <- which(availAsset_df$callId==thisCallId & SplitResource(availAsset_df$assetCustacId,'asset')==thisCallCcy)

      if(length(idxAssetCallCcy_vec)==0){
        stop('ALERR2001: Settlement currency is not available(not in inventory/not eligible)!')
      } else{
        idxAssetKeep_vec <- append(idxAssetKeep_vec,idxAssetCallCcy_vec)
      }
    }
    availAssetCash_df <- availAsset_df[idxAssetKeep_vec,]
    resourceCash_df <- resource_df[match(unique(availAssetCash_df$assetCustacId),resource_df$id),]

    result <- AllocationAlgo(callInfo_df,availAssetCash_df,resourceCash_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,ifNewAlloc,allocated_list,
                             minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod)
    
    #### Analyze Allocation Result Performance ######
    resultAnalysis <- ResultAnalysis(availAssetCash_df,availAssetOri_df=availAsset_df,resourceCash_df,resourceOri_df=resource_df,callInfo_df,
                                     result$callOutput_list)
  } else if(scenario==3){
    pref_vec <- c(0,10)
    result <- AllocationAlgo(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,ifNewAlloc,allocated_list,
                             minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod)
    
    #### Analyze Allocation Result Performance ######
    resultAnalysis <- ResultAnalysis(availAsset_df,availAsset_df,resource_df,resource_df,callInfo_df,
                                     result$callOutput_list)
  } else{
    stop('ALERR1006: Please input a valid scenario!')
  }
  
  return(list(#msOutput=msOutput_list,
    callOutput=result$callOutput_list,checkCall_mat=result$checkCall_mat,
    solverStatus=result$solverStatus,solverObjValue=result$solverObjValue,resultAnalysis=resultAnalysis))
}

