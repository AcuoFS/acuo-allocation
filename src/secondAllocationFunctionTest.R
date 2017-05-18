
#### Main Function Start ############
CallSecondAllocation <- function(algoVersion,callId_vec, resource_vec,callInfo_df,availAsset_df,assetInfo_df,
                                 dsAssetId,dsCallId_vec, currentSelection_list,
                                 pref_vec,operLimit,operLimitMs,fungible){
  callId_vec <- unlist(callId_vec)
  dsCallId_vec <- unlist(dsCallId_vec)
  callIdTotal_vec <- callId_vec
  callInfoTotal_df <- callInfo_df
  resourceTotal_vec <- resource_vec
  availAssetTotal_df <- availAsset_df
  resourceTotal_df <- resource_df
  
  if(algoVersion==1){
    if(length(dsCallId_vec)==1){
      dsCallId <- dsCallId_vec
      
      result <- SecondAllocationAlgoV1(callId_vec, resource_vec,callInfo_df,availAsset_df,assetInfo_df,
                                       dsAssetId,dsCallId, currentSelection_list,
                                       pref_vec)
    } else if(length(dsCallId_vec)>1){
      stop('Cannot handle deselection from multiple margin calls currently under operation as an objective settings!')
    } else{
      stop('Please specify which margin calls the asset is removed from!')
    }
  } else if(algoVersion==2){
    if(length(dsCallId_vec)>=1){
      result <- SecondAllocationAlgoAllMsV2(callIdTotal_vec,callInfoTotal_df,resourceTotal_vec,availAssetTotal_df,resourceTotal_df,
                                            dsAssetId,dsCallId_vec,currentSelection_list,
                                            pref_vec,operLimit,operLimitMs,fungible)
    } else{
      stop('Please specify which margin calls the asset is removed from!')
    }
  }
  return(result)
}
#### Main Function END ##############
