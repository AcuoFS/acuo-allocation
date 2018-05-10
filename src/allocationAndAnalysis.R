
CallAllocation <- function(algoVersion,scenario,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimitMs,fungible,ifNewAlloc,allocated_list,inputLimit_vec,timeLimit,callOrderMethod,minMoveValue){
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
  callId_vec <- as.character(callInfo_df$id)
  resource_vec <- as.character(resource_df$id)
  if(missing(inputLimit_vec)){
    inputLimit_vec <- c(7,7,7,4)
  }
  if(missing(timeLimit)){
    timeLimit <- 13
  }
  if(missing(callOrderMethod)){
    callOrderMethod <- 3
  }
  if(missing(minMoveValue)){
    minMoveValue <- 1000
  }
  
  #### Run Algo Under a Specific Scenario
  if(scenario==1){
    #### Allocation #################################
    result <- AllocationAlgo(callInfo_df,availAsset_df,resource_df,pref_vec,operLimitMs,fungible,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod,
                             ifNewAlloc,allocated_list)
    
    #### Analyze Allocation Result Performance ######
    # dailyCost, monthlyCost, reservedLiquidityRatio, movement
    resultAnalysis <- ResultAnalysis(availAsset_df,availAsset_df,resource_df,resource_df,callInfo_df,
                                     result$callOutput_list)
  } else if(scenario==2){
    
    availAssetCash_df <- availAsset_df
    resourceCash_vec <- resource_vec
    resourceCash_df <- resource_df
    settleCcy_vec <- callInfo_df$currency
    
    idxKeep_vec <- rep(0,length(availAssetCash_df$callId))
    count <- 0
    for(i in 1:length(callId_vec)){
      assetTemp_vec <- SplitResource(availAsset_df$assetCustacId,'asset')
      idxTemp_vec <- which(availAssetCash_df$callId==callId_vec[i] & assetTemp_vec==callInfo_df$currency[i])
      if(length(idxTemp_vec)==0){
        stop('ALERR2001: Settlement currency is not available(not in inventory/not eligible)!')
      }
      numTemp <- length(idxTemp_vec)
      count <- count+numTemp
      idxKeep_vec[(count-numTemp+1):count] <- idxTemp_vec
    }
    idxKeep_vec <- idxKeep_vec[1:count]
    availAssetCash_df <- availAssetCash_df[idxKeep_vec,]
    resourceCash_vec <- unique(availAssetCash_df$assetCustacId)
    resourceCash_df <- resource_df[match(resourceCash_vec,resource_df$id),]
    
    result <- AllocationAlgo(callInfo_df,availAssetCash_df,resourceCash_df,
                             pref_vec,operLimit,operLimitMs_vec,fungible,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod,
                             ifNewAlloc,allocated_list)
    
    #### Analyze Allocation Result Performance ######
    # dailyCost, monthlyCost, reservedLiquidityRatio, movement
    resultAnalysis <- ResultAnalysis(availAssetCash_df,availAsset_df,resourceCash_df,resource_df,callInfo_df,
                                     result$callOutput_list)
  } else if(scenario==3){
    pref_vec <- c(0,10,0)
    result <- AllocationAlgo(callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod,
                             ifNewAlloc,allocated_list)
    
    #### Analyze Allocation Result Performance ######
    # dailyCost, monthlyCost, reservedLiquidityRatio, movement
    resultAnalysis <- ResultAnalysis(availAsset_df,availAsset_df,resource_df,resource_df,callInfo_df,
                                     result$callOutput_list)
  } else{
    stop('ALERR1006: Please input a valid scenario!')
  }
  
  return(list(#msOutput=msOutput_list,
    callOutput=result$callOutput_list,checkCall_mat=result$checkCall_mat,
    solverStatus=result$solverStatus,solverObjValue=result$solverObjValue,resultAnalysis=resultAnalysis))
}

