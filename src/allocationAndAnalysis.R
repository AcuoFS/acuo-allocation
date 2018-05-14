##############################################################
## Title: Function to run allocation under a specific scenario given the scenario code
## Purpose: To measure the Algo performance from costs and liquidity point of view, we defined 
#           two scenarios for comparison:
#             1. to allocate using settlement currency of the call;
#             2. to allocate the least liquid asset;
## Strategy: To simulate the first scenario, we have to remove the assets other than 
#           call settlement currency from both availAsset_df and resource_df
#           To similate the second scenario, we need to set the pref_vec to (0,10), which
#           means only liquidity objective is taken into account
## Author: Silin Yuan
## Date: 2018/05/10
##############################################################

CallAllocation <- function(scenario,callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
                           algoVersion,ifNewAlloc,allocated_list,minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod){
  # Run Algo under different scenarios by controling the input to the algo &
  # Analyze allocation result performance
  #
  # Args(Direct Use): 
  #   scenario:
  #     scenario = 1, Algo suggestion
  #     scenario = 2, allocate settlement cash only
  #     scenario = 3, allocate least liquid assets
  #   availAsset_df
  #   resource_df
  #   callInfo_df
  #
  # Variables:
  #   usableAvailAsset_df: info of the resources can be used to allocate for calls
  #   usableResource_df: info of all resources can be used to allocate
  #   
  #
  # Returns:  
  #   A list contains allocation result and analysis
  
  #### Adjust the Input to Algo ####################

  if(scenario==1){
    ## Usable Resources
    usableAvailAsset_df <- availAsset_df
    usableResource_df <- resource_df
  } else if(scenario==2){ 
    ## Keep Only Assets Same as Call Currency
    idxAssetKeep_vec <- vector() # to store assets idx with the same currency of the call
    
    for(i in 1:length(callInfo_df$id)){
      thisCallId <- callInfo_df$id[i]
      thisCallCcy <- callInfo_df$currency[i]
      idxAssetCallCcy_vec <- which(availAsset_df$callId==thisCallId & SplitResource(availAsset_df$assetCustacId,'asset')==thisCallCcy)
      if(length(idxAssetCallCcy_vec)>0){
        idxAssetKeep_vec <- append(idxAssetKeep_vec,idxAssetCallCcy_vec)
      } else{
        stop('ALERR2001: Settlement currency is not available(not in inventory/not eligible)!')
      }
    }
    ## Usable Resources
    usableAvailAsset_df <- availAsset_df[idxAssetKeep_vec,]
    usableResource_df <- resource_df[match(unique(usableAvailAsset_df$assetCustacId),resource_df$id),]
  } else if(scenario==3){
    ## Set the Preference
    pref_vec <- c(0,10)
    ## Usable Resources
    usableAvailAsset_df <- availAsset_df
    usableResource_df <- resource_df
  } else{
    stop('ALERR1006: Please input a valid scenario!')
  }
  #### Allocation ################################
  result <- AllocationAlgo(callInfo_df,usableAvailAsset_df,usableResource_df,
                           pref_vec,operLimitMs,fungible,
                           algoVersion,ifNewAlloc,allocated_list,
                           minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod)
  
  #### Analyze Allocation Result Performance ######
  resultAnalysis <- DeriveResultAnalytics(availAsset_df,resource_df,callInfo_df,
                                   result$callOutput_list)
  
  #### Return Allocation and Analysis Result #######
  return(list(#msOutput=msOutput_list,
    callOutput=result$callOutput_list,checkCall_mat=result$checkCall_mat,
    solverStatus=result$solverStatus,solverObjValue=result$solverObjValue,resultAnalysis=resultAnalysis))
}

