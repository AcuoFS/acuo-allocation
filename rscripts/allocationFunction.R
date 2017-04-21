
#### Main Function-Interface of Java Start #######
CallAllocation <- function(algoVersion,scenario,callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit){
  #### Scenario Code Start #########
  # scenario = 1, Algo suggestion
  # scenario = 2, post settlement cash only
  # scenario = 3, post least liquid assets
  #### Scenario Code END ###########
  
  inputLimit_vec <- c(7,7,6,4); 
  timeLimit=13; 
  callOrderMethod=3
  minMoveValue<- 1000;
  # build scenario into the function
  #### Scenario: Algo suggestion: #####
  if(scenario==1){
    result <- AllocationAlgo(callId_vec,resource_vec,resource_vec,
                             callInfo_df,availAsset_df,availAsset_df,assetInfo_df,assetInfo_df,pref_vec,operLimit,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod)
  } else if(scenario==2){
    
    availAssetCash_df <- availAsset_df
    resourceCash_vec <- resource_vec
    assetInfoCash_df <- assetInfo_df
    settleCcy_vec <- callInfo_df$currency
    
    idxKeep_vec <- rep(0,length(availAssetCash_df$callId))
    count <- 0
    for(i in 1:length(callId_vec)){
      idxTemp_vec <- which(availAssetCash_df$callId==callId_vec[i] & availAsset_df$assetId==callInfo_df$currency[i])
      if(length(idxTemp_vec)==0){
        stop('Settlement currency is not available(not in inventory/not eligible)!')
      }
      numTemp <- length(idxTemp_vec)
      count <- count+numTemp
      idxKeep_vec[(count-numTemp+1):count] <- idxTemp_vec
    }
    idxKeep_vec <- idxKeep_vec[1:count]
    availAssetCash_df <- availAssetCash_df[idxKeep_vec,]
    resourceCash_vec <- unique(availAssetCash_df$assetCustacId)
    assetIdCash_vec <- unique(SplitResource(resourceCash_vec,'asset'))
    assetInfoCash_df <- assetInfoByAssetId(assetIdCash_vec)
    assetInfoCash_df <- assetInfoCash_df[match(assetIdCash_vec,assetInfoCash_df$id),]
    
    result <- AllocationAlgo(callId_vec,resourceCash_vec,resource_vec,
                             callInfo_df,availAssetCash_df,availAsset_df,assetInfoCash_df,assetInfo_df,pref_vec,operLimit,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod)
  } else if(scenario==3){
    pref_vec <- c(0,10,0)
    result <- AllocationAlgo(callId_vec,resource_vec,resource_vec,
                             callInfo_df,availAsset_df,availAsset_df,assetInfo_df,assetInfo_df,pref_vec,operLimit,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod)
  } else{
    stop('Please input a valid scenario!')
  }
  return(result)
}
#### Main Function-Interface of Java End #########


#### OTHER FUNCTIONS(CALLED IN THE MAIN FUNCTION)##########################
AllocationAlgo <- function(callId_vec,resource_vec,resourceOri_vec,callInfo_df,availAsset_df,availAssetOri_df,assetInfo_df,assetInfoOri_df,pref_vec,operLimit,
                           algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod){
  
  #### Order callId_vec Start ######################################
  ## method 0: Keep original
  ## method 1: By margin call amount, decreasing
  ## method 2: By margin type, VM then IM; sub order by call amount
  ## method 3: By total call amount in margin statement, decreasing
  
  if(missing(callOrderMethod)){
    callOrderMethod <-2
  }
  
  callInfo_df <- OrderCallId(callOrderMethod,callInfo_df)
  callId_vec <- callInfo_df$id
  msId_vec <- unique(callInfo_df$marginStatement)
  #### Order callId_vec END ######################################
  
  #### Group the callId_vec Start ################################
  # method 1: group by marginType
  # maximum limitVm VM or limitIm IM a time
  # group by margin statements
  imLimit <- inputLimit_vec[1]
  vmLimit <- inputLimit_vec[2]  
  callLimit <- inputLimit_vec[3]
  msLimit <- inputLimit_vec[4]
  
  groupCallId_list <- GroupCallIdByMs(callLimit,msLimit,callInfo_df,callId_vec)
  #### Group the callId_vec Start #################################
  
  
  callNum <- length(callId_vec)
  
  # allocate one group a time
  # after each allocation, update the tempQuantity_vec of each asset
  callOutput_list <- list()
  msOutput_list <- list()
  checkCall_mat <- matrix(c(callInfo_df$callAmount,rep(0,callNum)),nrow=callNum, dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
  
  ############ ITERATE THE GROUP, RUN THE ALGO Start #########################
  
  for(i in 1:length(groupCallId_list)){
    
    callIdGroup_vec <- groupCallId_list[[i]]
    msIdGroup_vec <- unique(callInfo_df$marginStatement[which(callInfo_df$id %in% callIdGroup_vec)])
    #cat(' group:',i,'\n','callId_vec:',callIdGroup_vec,'\n')
    
    callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
    availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
    
    resourceGroup_vec <- unique(availAssetGroup_df$assetCustacId)
    assetIdGroup_vec <-unique(SplitResource(resourceGroup_vec,'asset'))
    assetInfoGroup_df <- assetInfo_df[match(assetIdGroup_vec,assetInfo_df$id),]
    
    # input data to the core Algo
    coreInput_list <- AllocationInputData(callIdGroup_vec,resourceGroup_vec,callInfoGroup_df,availAssetGroup_df,assetInfoGroup_df)
    
    #### Pre-allocate Start ######################
    availAssetPre_df <- availAssetGroup_df
    callInfoPre_df <- callInfoGroup_df
    assetInfoPre_df <- assetInfoGroup_df
    callOutputPre_list <- callOutput_list # currently, store all the cumulated margin calls
    for(p in 1:length(callIdGroup_vec)){
      # consider to change to margin statement 
      callId <- callIdGroup_vec[p] 
      operLimitMs <- operLimit/length(msIdGroup_vec) + 1 # this limit is supposed to set on ms not mc 
      res <- PreAllocation(algoVersion,callId,callInfoPre_df,availAssetPre_df,assetInfoPre_df,pref_vec,operLimitMs,minMoveValue,timeLimit,callOutput_list,checkCall_mat)
      availAssetPreGroup_df <- res$availAssetGroup_df # availAssetPreGroup_df: the available asset for current margin call(done in PreAllocate)
      #print('availAssetPreGroup_df:'); print(availAssetPreGroup_df)
      
      #### find all occurance of the resource in the availAsset_df to update the quantity
      #### why not just separate the quantity from availAsset_df
      #### the reason is to reserve the quantity for the margin call. 
      #### no use currently, don't know whether will be used in the future
      
      for(k in 1:length(availAssetPreGroup_df$assetCustacId)){
        updateResource <- availAssetPreGroup_df$assetCustacId[k]
        updateQuantity <- availAssetPreGroup_df$quantity[k]
        updateIdx_vec <- which(availAssetPre_df$assetCustacId==updateResource)
        availAssetPre_df$quantity[updateIdx_vec] <- updateQuantity
      }
      #print('availAssetPre_df:'); print(availAssetPre_df)
      
      callOutputPreGroup_list <- res$callOutput_list
      resultPre_list <- res$resultGroup_list
      checkCallPre_mat <- res$checkCall_mat
      callOutputPre_list[[callId]] <- callOutputPreGroup_list[[callId]]
    }
    
    # parameteres need to pass to the CoreAlgoV2
    # callOutputPre_list
    # availAssetPre_df # don't need, solver will auto deduct the quantity while solving
    # combine into a single list: preAllocation_list
    
    initAllocation_list <- callOutputPre_list # currently, store all the cumulated margin calls
    #### Pre-allocate End ########################
    
    #### Run CoreAlgo Start ######################
    if(algoVersion==1){
      resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)#,initAllocation_list)
    } else if(algoVersion==2){
      resultGroup_list <- CoreAlgoV2(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,operLimit,minMoveValue,initAllocation_list)
    }
    #### Run CoreAlgo END ########################
    
    msOutputGroup_list <- resultGroup_list$msOutput_list
    callOutputGroup_list <- resultGroup_list$callOutput_list
    
    status <- resultGroup_list$status
    lpsolveRun <- resultGroup_list$lpsolveRun
    solverObjValue <- resultGroup_list$solverObjValue
    checkCallGroup_mat <- resultGroup_list$checkCall_mat
    
    # update the availAsset 
    # consider to move the update of available asset from CoreAlgo to outside
    # based on the result list
    availAssetGroup_df <- resultGroup_list$availAsset_df
    availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),] <- availAssetGroup_df
    
    for(k in 1:length(callIdGroup_vec)){
      callId <- callIdGroup_vec[k]
      j <- which(msIdGroup_vec==callInfo_df$marginStatement[which(callInfo_df$id==callId)])
      
      msId <- msId_vec[j]
      callOutput_list[[callId]] <- callOutputGroup_list[[callId]]
      #msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
      checkCall_mat[which(rownames(checkCall_mat)==callId),2] <- checkCallGroup_mat[which(rownames(checkCallGroup_mat)==callId),2]
    }
  }
  
  
  #### Result Analysis Output Start #####################
  coreInput_list <- AllocationInputData(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df)
  eli_vec <- coreInput_list$eli_vec; idxEli_vec <- which(eli_vec==1)
  varInfo_list <- VarInfo(eli_vec,callInfo_df,resource_vec,callId_vec)
  varName_vec <- varInfo_list$varName_vec; varNum <- varInfo_list$varNum
  varAmount_vec <- callList2AmountVec(callOutput_list,callId_vec,varName_vec[1:varNum])
  #### Costs
  dailyCost <- CostFun(varAmount_vec,coreInput_list$cost_vec[idxEli_vec])
  monthlyCost <- dailyCost*30
  dailyCost <- round(dailyCost,2)
  monthlyCost <- round(monthlyCost,2)
  
  #### Movements
  varAmount_mat <- VarVec2mat(varAmount_vec[1:varNum],varName_vec[1:varNum],callId_vec,resource_vec)
  movements <- OperationFun(varAmount_mat,callInfo_df,'matrix')
  
  #### Liquidity
  coreInputOri_list <- AllocationInputData(callId_vec,resourceOri_vec,callInfo_df,availAssetOri_df,assetInfoOri_df)
  quantityTotal_mat <- coreInputOri_list$minUnitQuantity_mat;
  resourceTotal_vec <- coreInputOri_list$resource_vec
  
  if(callNum==1){
    quantityTotal_vec <- quantityTotal_mat
  } else{
    quantityTotal_vec <- apply(quantityTotal_mat,2,max)
  }
  
  coreInput_list <- AllocationInputData(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df)
  quantityRes_mat <- coreInput_list$minUnitQuantity_mat
  quantityRes_vec <- quantityTotal_vec
  
  idxTemp_vec <-match(resource_vec,resourceTotal_vec)
  if(callNum==1){
    quantityRes_vec[idxTemp_vec] <- quantityRes_mat
  } else{
    quantityRes_vec[idxTemp_vec] <- apply(quantityRes_mat,2,max)
  }
  #cat('quantityTotal_vec',quantityTotal_vec,'\n')
  #cat('quantityRes_vec',quantityRes_vec,'\n')
  #print('coreInputOri_list$haircut_mat: '); print(coreInputOri_list$haircut_mat)
  
  liquidity_vec <- apply((1-coreInputOri_list$haircut_mat)^2,2,min)
  minUnitValue_vec <- apply(coreInputOri_list$minUnitValue_mat,2,max)
  
  #cat('liquidity_vec',liquidity_vec,'\n')
  #cat('minUnitValue_vec',minUnitValue_vec,'\n')
  reservedLiquidityRatio <- LiquidFun(quantityRes_vec,quantityTotal_vec,liquidity_vec,minUnitValue_vec)
  
  resultAnalysis <- list(dailyCost=dailyCost,monthlyCost=monthlyCost,movements=movements,reservedLiquidityRatio=reservedLiquidityRatio)
  #### Result Analysis Output END #########################
  
  
  
  ############ ITERATE THE GROUP, RUN THE ALGO END #########################
  
  return(list(#msOutput=msOutput_list,
    callOutput=callOutput_list,checkCall_mat=checkCall_mat,availAsset_df=availAsset_df,
    status=status,lpsolveRun=lpsolveRun,solverObjValue=solverObjValue,resultAnalysis=resultAnalysis))
}

PreAllocation <- function(algoVersion,callIdGroup_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit, minMoveValue,timeLimit,callOutput_list,checkCall_mat){
  
  msIdGroup_vec <- unique(callInfo_df$marginStatement[which(callInfo_df$id %in% callIdGroup_vec)])
  #cat(' group:',i,'\n','callId_vec:',callIdGroup_vec,'\n')
  callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
  #print('in pre function');print(availAsset_df)
  availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
  #print(availAssetGroup_df)
  resourceGroup_vec <- unique(availAssetGroup_df$assetCustacId)
  assetIdGroup_vec <- matrix(unlist(strsplit(resourceGroup_vec,'-')),nrow=2)[1,]
  assetInfoGroup_df <- assetInfo_df[match(assetIdGroup_vec,assetInfo_df$id),]
  
  # input data to the core Algo
  coreInput_list <- AllocationInputData(callIdGroup_vec,resourceGroup_vec,callInfoGroup_df,availAssetGroup_df,assetInfoGroup_df)
  
  # core Algo, assume all data comes in a list
  if(algoVersion==1){
    resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)
  } else if(algoVersion==2){
    resultGroup_list <- CoreAlgoV2(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,operLimit,minMoveValue)
  }
  #msOutputGroup_list <- resultGroup_list$msOutput_list
  callOutputGroup_list <- resultGroup_list$callOutput_list
  status <- resultGroup_list$status
  lpsolveRun <- resultGroup_list$lpsolveRun
  solverObjValue <- resultGroup_list$solverObjValue
  checkCallGroup_mat <- resultGroup_list$checkCall_mat
  
  # update the availAsset 
  availAssetGroup_df <- resultGroup_list$availAsset_df
  availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),] <- availAssetGroup_df
  
  for(k in 1:length(callIdGroup_vec)){
    callId <- callIdGroup_vec[k]
    j <- which(msIdGroup_vec==callInfo_df$marginStatement[which(callInfo_df$id==callId)])
    msId <- msIdGroup_vec[j]
    callOutput_list[[callId]] <- callOutputGroup_list[[callId]]
    #msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
    checkCall_mat[which(rownames(checkCall_mat)==callId),2] <- checkCallGroup_mat[which(rownames(checkCallGroup_mat)==callId),2]
  }
  
  solveOutput_list <- list(resultGroup_list=resultGroup_list,availAssetGroup_df=availAssetGroup_df,checkCall_mat=checkCall_mat,callOutput_list=callOutput_list)
  return(solveOutput_list)
}

OrderCallId <- function(callOrderMethod,callInfo_df){
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
  }
  return(callInfo_df)
}

SplitCallId <- function(vmLimit,imLimit,callLimit,msLimit,callInfo_df,callId_vec){
  
  groupCallId_list <- list()
  # if the total call numbers is equal or less than limitTotal, only one group
  if(length(callInfo_df[,1])<=limitTotal){
    groupCallId_list[[1]] <- callId_vec
  } else{
    # index of VM and IM in the call list
    
    idxVm_vec <- which(toupper(callInfo_df$marginType)=='VARIATION')
    idxIm_vec <- which(toupper(callInfo_df$marginType)=='INITIAL')
    # number of VM and IM groups 
    groupVmNum <- ceiling(length(idxVm_vec)/limitVm) 
    groupImNum <- ceiling(length(idxIm_vec)/limitIm)
    
    # make the group list, VM and IM in the same list
    index <- 0
    if(groupVmNum==1){
      index <- index+1
      groupCallId_list[[index]] <- callId_vec[idxVm_vec]
    } else if(groupVmNum > 1){
      for(i in 1:(groupVmNum-1)){
        index <- index+1
        groupCallId_list[[index]] <- callId_vec[idxVm_vec[(i-1)*limitVm+(1:limitVm)]]
      } 
      index <- index+1
      groupCallId_list[[index]] <- callId_vec[tail(idxVm_vec,length(idxVm_vec)-(groupVmNum-1)*limitVm)]
    }
    
    if(groupImNum==1){
      index <- index+1
      groupCallId_list[[index]] <- callId_vec[idxIm_vec]
    } else if(groupImNum > 1){
      for(i in 1:(groupImNum-1)){
        index <- index+1
        groupCallId_list[[index]] <- callId_vec[idxIm_vec[(i-1)*limitIm+(1:limitIm)]]
      } 
      index <- index+1
      groupCallId_list[[index]] <- callId_vec[tail(idxIm_vec,length(idxIm_vec)-(groupImNum-1)*limitIm)]
    }
  }
  return(groupCallId_list)
}

GroupCallIdByMs <- function(callLimit,msLimit,callInfo_df,callId_vec){
  
  groupCallId_list <- list()
  # if the total call numbers is equal or less than limitTotal, only one group
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
