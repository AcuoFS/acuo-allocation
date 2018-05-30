AllocateByGroups <- function(callInfo_df,availAsset_df,resource_df,
                             pref_vec,operLimitMs,fungible,
                             algoVersion,ifNewAlloc,allocated_list,
                             minMoveValue,timeLimit,maxCallNum,maxMsNum,callOrderMethod){  
  # Improve Algo performance by allocating a few calls each time instead of the entire list
  # and then update the quantity used for assets after each iteration, until all calls are allocated.
  # Before running the allocation, first check the asset inventory sufficiency. 
  #
  # Args(Direct Use): 
  #   maxCallNum,maxMsNum: number of calls and number of statements in a group
  #
  # Returns:  
  #   A list contains two lists of allocation result structured by call and MS. 
  
  #### Check Asset Pool Sufficiency #####
  AssetsAreSufficient <- EstimateAssetSufficiency(availAsset_df,callInfo_df,resource_df)
  if(!AssetsAreSufficient){
    stop('ALERR2003: Asset inventory is insufficient')
  }
  
  #### Group the Margin Calls ###################
  groupCallId_list <- GroupCallIdByMs(maxCallNum,maxMsNum,callInfo_df,callOrderMethod)
  #### Initiate the Returned Variables #########
  callOutput_list <- list()
  msOutput_list <- list()
  #### Iterate the Groups, Run Algo ####
  for(i in 1:length(groupCallId_list)){
    callIdGroup_vec <- groupCallId_list[[i]]
    
    #### Derive the Input for One Statement ######
    callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
    availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
    resourceGroup_df <- resource_df[which(resource_df$id %in% availAssetGroup_df$assetCustacId),]
    
    ## Exclude Resources with Few Units Left
    updatedInfo <- ExcludeInsufficientResourceFromAllocation(resource_df,availAssetGroup_df,length(callIdGroup_vec))
    resourceGroup_df <- updatedInfo$resource_df
    availAssetGroup_df <- updatedInfo$availAsset_df
    
    if(ifNewAlloc){
      allocatedGroup_list <- list()
    } else{
      allocatedGroup_list <- allocated_list[match(callIdGroup_vec,names(allocated_list))]
    }
    
    #### Call AllocateAndCompareResults ####
    groupResult <- AllocateAndCompareResults(callInfoGroup_df,availAssetGroup_df,resourceGroup_df,
                                             pref_vec,operLimitMs,fungible,
                                             algoVersion,ifNewAlloc,allocatedGroup_list,minMoveValue,timeLimit)
    #### Store the Result #######
    for(k in 1:length(callIdGroup_vec)){
      callId <- callIdGroup_vec[k]
      msId <- callInfo_df$marginStatement[which(callInfo_df$id==callId)]
      callOutput_list[[callId]] <- groupResult$callOutput_list[[callId]]
      msOutput_list[[msId]] <- groupResult$msOutput_list[[msId]]
    }
    
    #### Update the Quantity in resource_df ##########
    # update the resource_df quantity, rounding
    quantityUsed_vec <- UsedQtyFromResultList(groupResult$callOutput_list,resource_df$id,callInfo_df$id)
    resource_df$qtyMin <- round(resource_df$qtyMin - quantityUsed_vec/resource_df$minUnit,4)
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
  # Generate an allocation as the initial guess to CoreAlgo
  # The approach is to derive allocation per statement by calling CoreAlgo and then aggregate the result. 
  # 
  msOutput_list <- list()
  callOutput_list <- list()
  
  msId_vec <- unique(callInfo_df$marginStatement)
  for(i in 1:length(msId_vec)){
    callInThisMs_vec <- callInfo_df$id[which(callInfo_df$marginStatement==msId_vec[i])]
    
    #### Derive the Input for One Statement ######
    callInfoGroup_df <- callInfo_df[match(callInThisMs_vec,callInfo_df$id),]
    availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callInThisMs_vec),]
    resourceGroup_df <- resource_df[which(resource_df$id %in% availAssetGroup_df$assetCustacId),]
    
    ## Exclude Resources with Few Units Left
    updatedInfo <- ExcludeInsufficientResourceFromAllocation(resourceGroup_df,availAssetGroup_df,length(callInThisMs_vec))
    resourceGroup_df <- updatedInfo$resource_df
    availAssetGroup_df <- updatedInfo$availAsset_df
    
    allocatedGroup_list <- allocated_list[match(callInThisMs_vec,names(allocated_list))]
    
    #### Call CoreAlgo ###########
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
    
    #### Store the Result #######
    for(k in 1:length(callInThisMs_vec)){
      callId <- callInThisMs_vec[k]
      msId <- callInfo_df$marginStatement[which(callInfo_df$id==callId)]
      callOutput_list[[callId]] <- callOutputGroup_list[[callId]]
      msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
    }
    
    #### Update the Quantity in resource_df ######
    quantityUsed_vec <- UsedQtyFromResultList(callOutputGroup_list,resource_df$id,callId_vec)
    resource_df$qtyMin <- resource_df$qtyMin - quantityUsed_vec/resource_df$minUnit
  }
  resultPre_list <- list(callOutput_list=callOutput_list,msOutput_list=msOutput_list)
  return(resultPre_list)
}

ResultSelect <- function(result1, result2,availAsset_df,resource_df,callInfo_df,pref_vec){
  # select the allocation result which has better analytic indicators
  # 
  callId_vec <- callInfo_df$id
  resource_vec <- resource_df$id
  callOutput1 <- result1$callOutput
  callOutput2 <- result2$callOutput
  
  # update the resource_df quantity, rounding
  quantityUsed1_vec <- UsedQtyFromResultList(callOutput1,resource_vec,callId_vec)
  quantityUsed2_vec <- UsedQtyFromResultList(callOutput2,resource_vec,callId_vec)
  qtyMin1 <- round(resource_df$qtyMin - quantityUsed1_vec/resource_df$minUnit,4)
  qtyMin2 <- round(resource_df$qtyMin - quantityUsed2_vec/resource_df$minUnit,4)
  resource1_df <- resource_df
  resource1_df$qtyMin <- qtyMin1
  resource2_df <- resource_df
  resource2_df$qtyMin <- qtyMin2
  resultAnalysis1 <- DeriveResultAnalytics(availAsset_df,resource1_df,callInfo_df,callOutput1)
  resultAnalysis2 <- DeriveResultAnalytics(availAsset_df,resource2_df,callInfo_df,callOutput2)
  
  # compare and select
  cost1 <- resultAnalysis1$dailyCost
  cost2 <- resultAnalysis2$dailyCost
  liquidity1 <- resultAnalysis1$reservedLiquidityRatio
  liquidity2 <- resultAnalysis2$reservedLiquidityRatio
  movement1 <- resultAnalysis1$movements
  movement2 <- resultAnalysis2$movements
  
  if(cost1 <= cost2 && liquidity1 >= liquidity2){
    finalResult <- result1
  }else if(pref_vec[1]>=pref_vec[2] && cost1 <= cost2){
    finalResult <- result1
  }else if(pref_vec[1]>=pref_vec[2] && liquidity1 >= liquidity2){
    finalResult <- result1
  }else{
    finalResult <- result2
  }
  return(finalResult)
}

OrderCallId <- function(callOrderMethod,callInfo_df){
  ## method 0: Keep original
  ## method 1: By margin call amount, decreasing
  ## method 2: By margin type, VM then IM; sub order by call amount
  ## method 3: By total call amount in margin statement, decreasing
  
  #### Assign Default Values
  callOrderMethod <- 3
  
  #### Order Calls
  if(callOrderMethod==0){ # keep original
    callInfo_df <- callInfo_df
  }else if(callOrderMethod==1){ # by call amount, decreasing
    callInfo_df <- callInfo_df[order(callInfo_df$callAmount,decreasing=T),]
  }else if(callOrderMethod==2){ # by margin type(VM first) and call amount, decreasing
    callInfoVM <- callInfo_df[which(toupper(callInfo_df$marginType)=='VARIATION'),]
    callInfoVM <- callInfoVM[order(callInfoVM$callAmount,decreasing=T),]
    
    callInfoIM <- callInfo_df[which(toupper(callInfo_df$marginType)=='INITIAL'),]
    callInfoIM <- callInfoIM[order(callInfoIM$callAmount,decreasing=T),]
    callInfo_df <- rbind(callInfoVM,callInfoIM)
  }else if(callOrderMethod==3){ # by margin statement, call amount in margin statement, decreasing
    msAggrCall_df <- aggregate(callAmount~marginStatement,data=callInfo_df,sum)
    msAggrCall_df <- msAggrCall_df[order(msAggrCall_df$callAmount,decreasing=T),]
    tempMs_vec <- msAggrCall_df$marginStatement
    newCallInfo_df <- callInfo_df
    idxCurrent <- 0
    for(i in 1:length(tempMs_vec)){
      idxTemp_vec <- which(tempMs_vec[i]==callInfo_df$marginStatement)
      tempCallInfo_df <- callInfo_df[idxTemp_vec,]
      tempCallInfo_df <- tempCallInfo_df[order(tempCallInfo_df$callAmount,decreasing=F),]
      idxNewTemp_vec <- idxCurrent+1:length(idxTemp_vec)
      newCallInfo_df[idxNewTemp_vec,] <- tempCallInfo_df
      
      idxCurrent <- idxCurrent+length(idxTemp_vec)
    }
    callInfo_df<- newCallInfo_df
  }else if(callOrderMethod==4){ # by margin statement, call amount in margin statement, increasing
    msAggrCall_df <- aggregate(callAmount~marginStatement,data=callInfo_df,sum)
    msAggrCall_df <- msAggrCall_df[order(msAggrCall_df$callAmount,decreasing=F),]
    tempMs_vec <- msAggrCall_df$marginStatement
    newCallInfo_df <- callInfo_df
    idxCurrent <- 0
    for(i in 1:length(tempMs_vec)){
      idxTemp_vec <- which(tempMs_vec[i]==callInfo_df$marginStatement)
      tempCallInfo_df <- callInfo_df[idxTemp_vec,]
      tempCallInfo_df <- tempCallInfo_df[order(tempCallInfo_df$callAmount,decreasing=F),]
      idxNewTemp_vec <- idxCurrent+1:length(idxTemp_vec)
      newCallInfo_df[idxNewTemp_vec,] <- tempCallInfo_df
      
      idxCurrent <- idxCurrent+length(idxTemp_vec)
    }
    callInfo_df<- newCallInfo_df
  }
  return(callInfo_df)
}

GroupCallIdByMs <- function(callLimit,msLimit,callInfo_df,callOrderMethod){
  #### Assign Default Values
  if(missing(callLimit)){
    callLimit <- 7
  }
  if(missing(msLimit)){
    msLimit <- 4
  }
  #### Order callId_vec
  callInfo_df <- OrderCallId(callOrderMethod,callInfo_df)
  callId_vec <- callInfo_df$id
  
  groupCallId_list <- list()
  # if the call number is equal or less than callLimit
  # or the ms number is equal or less than msLimit
  # then only one group
  # else group by msLimit
  if(length(callInfo_df[,1])<=callLimit){
    groupCallId_list[[1]] <- callId_vec
  } else if(length(unique(callInfo_df$marginStatement))<=msLimit){
    groupCallId_list[[1]] <- callId_vec
  } else{
    groupMsId_list <- list()
    callMs_vec <- callInfo_df$marginStatement
    ms_vec <- unique(callMs_vec)
    msGroupNum <- ceiling(length(ms_vec)/msLimit)
    
    for(i in 1:(msGroupNum-1)){
      tempCurrent <- msLimit*(i-1)
      tempMs_vec <- ms_vec[(tempCurrent+1):(tempCurrent+msLimit)]
      tempCall_vec <- callInfo_df$id[which((callInfo_df$marginStatement) %in% tempMs_vec)]
      groupMsId_list[[i]]<- tempMs_vec
      groupCallId_list[[i]]<- tempCall_vec
    }
    tempCurrent <- msLimit*(msGroupNum-1)
    tempMs_vec <- na.omit(ms_vec[(tempCurrent+1):(tempCurrent+msLimit)])
    tempCall_vec <- callInfo_df$id[which((callInfo_df$marginStatement) %in% tempMs_vec)]
    groupMsId_list[[msGroupNum]]<- tempMs_vec
    groupCallId_list[[msGroupNum]]<- tempCall_vec
  }
  return(groupCallId_list)
}

ExcludeInsufficientResourceFromAllocation <- function(resource_df,availAsset_df,callNum){
  # For those resources with 0 or few units left, we exclude them from both resource_df and availAsset_df
  #
  rmResourceIdx <- which(resource_df$qtyMin/resource_df$minUnit < callNum)
  if(length(rmResourceIdx)>0){
    rmResource_vec <- resource_df$id[rmResourceIdx]
    resource_df <- resource_df[-rmResourceIdx,]
  }
  rmIdxAvail <- which(is.na(match(availAsset_df$assetCustacId,resource_df$id)))
  if(length(rmIdxAvail)>0){
    availAsset_df <- availAsset_df[-rmIdxAvail,]
  }
  return(list(resource_df=resource_df,availAsset_df=availAsset_df))
}

UsedQtyFromResultList <- function(result_list,resource_vec,callId_vec){
  #### minUnitQuantity of resources used for allocation
  quantityUsed_vec <- rep(0,length(resource_vec))
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  for(i in 1:callNum){
    callId <- callId_vec[i]
    alloc_df <- result_list[[callId]]
    resourceTemp_vec <- PasteResource(alloc_df$Asset,alloc_df$CustodianAccount)
    idxInRes_vec <- na.omit(match(resourceTemp_vec,resource_vec))
    if(length(idxInRes_vec)!=0){
      idxInAlloc <- match(resource_vec[idxInRes_vec],resourceTemp_vec)
      quantityUsed_vec[idxInRes_vec] <- quantityUsed_vec[idxInRes_vec]+alloc_df$Quantity[idxInAlloc]
    }
  }
  return(quantityUsed_vec)
}
