
#### Main Function Start ############
CallSecondAllocation <- function(algoVersion,callId_vec, resource_vec,callInfo_df,availAsset_df,assetInfo_df,resource_df,
                                 dsAssetId,dsCallId_vec, currentSelection_list,
                                 pref_vec,operLimit,operLimitMs_vec,fungible){
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
      stop('ALERR3002: Algorithm version 1 can not deal with deselecting an asset from multiple margin calls!')
    } 
  } else if(algoVersion==2){
      result <- SecondAllocationV2(callIdTotal_vec,callInfoTotal_df,resourceTotal_vec,availAssetTotal_df,resourceTotal_df,
                                            dsAssetId,dsCallId_vec,currentSelection_list,
                                            pref_vec,operLimit,operLimitMs_vec,fungible)
  }
  return(result)
}
#### Main Function END ##############
