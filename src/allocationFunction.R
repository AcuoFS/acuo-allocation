
#### Main Function-Interface of Java Start #######
CallAllocation <- function(algoVersion,scenario,callId_vec,resource_vec,callInfo_df,availAsset_df,resource_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,ifNewAlloc,allocated_list){
  #### Scenario Code Start #########
  # scenario = 1, Algo suggestion
  # scenario = 2, post settlement cash only
  # scenario = 3, post least liquid assets
  #### Scenario Code END ###########
  
  inputLimit_vec <- c(7,7,7,4); 
  timeLimit=13; 
  callOrderMethod=3
  minMoveValue<- 1000;
  # build scenario into the function
  #### Scenario: Algo suggestion: #####
  if(scenario==1){
    result <- AllocationAlgo(callId_vec,resource_vec,resource_vec,
                             callInfo_df,availAsset_df,availAsset_df,resource_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod,
                             ifNewAlloc,allocated_list)
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
        stop('Settlement currency is not available(not in inventory/not eligible)!')
      }
      numTemp <- length(idxTemp_vec)
      count <- count+numTemp
      idxKeep_vec[(count-numTemp+1):count] <- idxTemp_vec
    }
    idxKeep_vec <- idxKeep_vec[1:count]
    availAssetCash_df <- availAssetCash_df[idxKeep_vec,]
    resourceCash_vec <- unique(availAssetCash_df$assetCustacId)
    resourceCash_df <- resource_df[match(resourceCash_vec,resource_df$id),]
    
    result <- AllocationAlgo(callId_vec,resourceCash_vec,resource_vec,
                             callInfo_df,availAssetCash_df,availAsset_df,resourceCash_df,resource_df,
                             pref_vec,operLimit,operLimitMs_vec,fungible,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod,
                             ifNewAlloc,allocated_list)
  } else if(scenario==3){
    pref_vec <- c(0,10,0)
    result <- AllocationAlgo(callId_vec,resource_vec,resource_vec,
                             callInfo_df,availAsset_df,availAsset_df,resource_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod,
                             ifNewAlloc,allocated_list)
  } else{
    stop('Please input a valid scenario!')
  }
  return(result)
}
#### Main Function-Interface of Java End #########


#### CONNECT TO THE CORE MODULE OF OPTIMIZATION ####################
AllocationAlgo <- function(callId_vec,resource_vec,resourceOri_vec,callInfo_df,availAsset_df,availAssetOri_df,
                           resource_df,resourceOri_df,
                           pref_vec,operLimit,operLimitMs_vec,fungible,
                           algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod,
                           ifNewAlloc,allocated_list){
  
  #### Input Prepare & Output Initialization Start ###########
  #### Order callId_vec
  msOri_vec <- unique(callInfo_df$marginStatement)
  
  callInfo_df <- OrderCallId(callOrderMethod,callInfo_df)
  
  msTemp_vec <- unique(callInfo_df$marginStatement)
  idxTemp_vec <- match(msOri_vec,msTemp_vec)
  
  operLimitMs_vec <- operLimitMs_vec[idxTemp_vec]
  callId_vec <- callInfo_df$id
  
  #### Group the callId_vec
  groupCallId_list <- GroupCallIdByMs(callLimit=inputLimit_vec[3],msLimit=inputLimit_vec[4],callInfo_df,callId_vec)
  
  #### Initialize the Ouput
  callNum <- length(callId_vec)
  msId_vec <- unique(callInfo_df$marginStatement)
  msNum <- length(msId_vec)
  
  callOutput_list <- list()
  msOutput_list <- list()
  checkCall_mat <- matrix(c(callInfo_df$callAmount,rep(0,callNum)),nrow=callNum, dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
  #### Input Prepare & Output Initialization END #############
  
  #### Case When Movement Limit Is 1 Per Margin Statement Start ####
  for(i in 1:msNum){
    msId <- msId_vec[i]
    operLimitMs <- operLimitMs_vec[i]
    if(operLimitMs==1){
      callInThisMs_vec <- callInfo_df$id[which(callInfo_df$marginStatement==msId)]
      # assume one margin statement contains either 1 or 2 margin calls
      if(length(callInThisMs_vec)==2){
        # find the lines of available resources for these two calls
        idx1_vec <- which(availAsset_df$callId==callInThisMs_vec[1]) 
        idx2_vec <- which(availAsset_df$callId==callInThisMs_vec[2]) 
        
        # find the common resources
        resource1_vec <- availAsset_df$assetCustacId[idx1_vec]
        resource2_vec <- availAsset_df$assetCustacId[idx2_vec]
        sameResource_vec <- intersect(resource1_vec,resource2_vec)
        if(length(sameResource_vec)==0){ # not possible normally
          errormsg <- paste('There is no common asset can be allocated for both',paste(callInThisMs_vec),'in',msId)
          stop(errormsg)
        }
        
        # remove the other resources in availAsset_df
        rmIdx1_vec <- idx1_vec[-match(sameResource_vec,resource1_vec)] # lines not in the intersection
        rmIdx2_vec <- idx2_vec[-match(sameResource_vec,resource2_vec)]
        if(length(c(rmIdx1_vec,rmIdx2_vec))>=1){
          availAsset_df <- availAsset_df[-c(rmIdx1_vec,rmIdx2_vec),]
        }
        
        # if the optimal ccy for a margin statement is sufficient,
        # then the asset allocated to VM and IM will be the same
        # if the optimal ccy is not sufficient, 
        # then we need to allocate the second optimal ccy which is sufficient
        
        #### approach 1
        # pre-calculate the sufficient quantity for each ccy,
        # remove the insuffient ccy in the availAsset_df
        # and done
        
        callAmount1 <- callInfo_df$callAmount[which(callInfo_df$id==callInThisMs_vec[1])]
        callAmount2 <- callInfo_df$callAmount[which(callInfo_df$id==callInThisMs_vec[2])]
        
        idxAvail1_vec <- which(availAsset_df$callId==callInThisMs_vec[1]) 
        idxAvail2_vec <- which(availAsset_df$callId==callInThisMs_vec[2]) 
        
        haircut1_vec <- availAsset_df$haircut[idxAvail1_vec]+availAsset_df$FXHaircut[idxAvail1_vec]
        haircut2_vec <- availAsset_df$haircut[idxAvail2_vec]+availAsset_df$FXHaircut[idxAvail2_vec]
        
        resourceAvail_vec <- availAsset_df$assetCustacId[idxAvail1_vec]
        # resourceAvail_vec <- availAsset_df$assetCustacId[idxAvail2_vec] # should be the same
        
        idxRes_vec <- match(resourceAvail_vec,resource_df$id)
        minUnitValue_vec <- resource_df$minUnitValue[idxRes_vec]
        quantity_vec <- resource_df$qtyMin[idxRes_vec]
        
        integralSuffQty1_vec <- ceiling(callAmount1/(1-haircut1_vec)/minUnitValue_vec)
        integralSuffQty2_vec <- ceiling(callAmount2/(1-haircut2_vec)/minUnitValue_vec)
        integralSuffQty_vec <- integralSuffQty1_vec+integralSuffQty2_vec
        
        suffIdx_vec <- which(quantity_vec >= integralSuffQty_vec)
        if(length(suffIdx_vec)==0){ # not possible normally
          errormsg <- paste('There is no asset which is sufficient for both',callInThisMs_vec,'in',msId)
          stop(errormsg)
        }
        
        ## remove the insufficient assets
        rmIdx1_vec <- idxAvail1_vec[-suffIdx_vec] # lines not sufficient
        rmIdx2_vec <- idxAvail2_vec[-suffIdx_vec] 
        
        if(length(c(rmIdx1_vec,rmIdx2_vec))>=1){
          availAsset_df <- availAsset_df[-c(rmIdx1_vec,rmIdx2_vec),]
        }
      }
    }
  }
  # update the available resource based on the new availAsset_df
  resource_vec <- unique(availAsset_df$assetCustacId)
  resource_df <- resource_df[match(resource_vec,resource_df$id),]
  
  #### Case When Movement Limit Is 1 Per Margin Statement END ######
  
  ############ ITERATE THE GROUP, RUN THE ALGO Start #########################
  
  for(i in 1:length(groupCallId_list)){
    
    callIdGroup_vec <- groupCallId_list[[i]]
    msIdGroup_vec <- unique(callInfo_df$marginStatement[which(callInfo_df$id %in% callIdGroup_vec)])
    ratio <- length(msIdGroup_vec)/length(msId_vec) # the proportion of the msGroup in the msList
    operLimitGroup <- operLimit*ratio
    
    idxTemp_vec <- match(msIdGroup_vec,msId_vec)
    operLimitGroupMs_vec <- operLimitMs_vec[idxTemp_vec]
    
    #cat(' group:',i,'\n','callId_vec:',callIdGroup_vec,'\n')
    
    callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
    availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
    
    resourceGroup_vec <- unique(availAssetGroup_df$assetCustacId)
    resourceGroup_df <- resource_df[match(resourceGroup_vec,resource_df$id),]
    
    availInfoGroup_list <- AssetByCallInfo(callIdGroup_vec,resourceGroup_vec,availAssetGroup_df)
    
    if(ifNewAlloc){
      allocatedGroup_list <- list()
    } else{
      idxTemp_vec <- match(callIdGroup_vec,names(allocated_list))
      allocatedGroup_list <- allocated_list[idxTemp_vec]
    }
    #### Pre-allocate Start ######################
    
    resultPre <- PreAllocation(algoVersion,callIdGroup_vec,callInfoGroup_df,availAssetGroup_df,resourceGroup_df,
                               pref_vec,operLimitGroupMs_vec,operLimitGroupMs_vec,fungible,minMoveValue,timeLimit,
                               ifNewAlloc,allocatedGroup_list)
    
    callOutputGroupPre_list <- resultPre$callOutput_list
    checkCallGroupPre_mat <- resultPre$checkCall_mat
    
    initAllocation_list <- callOutputGroupPre_list # currently, store all the cumulated margin calls
    #### Pre-allocate End ########################
    
    #### Run CoreAlgo Start ######################
    if(algoVersion==1){
      resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)#,initAllocation_list)
    } else if(algoVersion==2){
      resultGroup_list <- CoreAlgoV2(callInfoGroup_df, resourceGroup_df, availInfoGroup_list,
                                     timeLimit,pref_vec,operLimitGroup,operLimitGroupMs_vec,fungible,minMoveValue,
                                     ifNewAlloc,initAllocation_list,allocatedGroup_list)
    }
    #### Run CoreAlgo END ########################
    
    msOutputGroup_list <- resultGroup_list$msOutput_list
    callOutputGroup_list <- resultGroup_list$callOutput_list
    
    solverStatus <- resultGroup_list$solverStatus
    lpsolveRun <- resultGroup_list$lpsolveRun
    solverObjValue <- resultGroup_list$solverObjValue
    checkCallGroup_mat <- resultGroup_list$checkCall_mat
    
    # update the resource_df 
    quantityUsed_vec <- UsedQtyFromResultList(callOutputGroup_list,resource_vec,callId_vec)
    resource_df$qtyMin <- resource_df$qtyMin - quantityUsed_vec/resource_df$minUnit
    
    for(k in 1:length(callIdGroup_vec)){
      callId <- callIdGroup_vec[k]
      msId <- callInfo_df$marginStatement[which(callInfo_df$id==callId)]
      callOutput_list[[callId]] <- callOutputGroup_list[[callId]]
      msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
      checkCall_mat[which(rownames(checkCall_mat)==callId),2] <- checkCallGroup_mat[which(rownames(checkCallGroup_mat)==callId),2]
    }
  }
  
  #### Result Analysis Output Start #####################
  
  availInfo_list <- AssetByCallInfo(callId_vec,resource_vec,availAsset_df)
  
  eli_mat <- availInfo_list$eli_mat; 
  eli_vec <-  as.vector(t(eli_mat)) 
  idxEli_vec <- which(eli_vec==1)     
  
  cost_mat <- availInfo_list$cost_mat
  cost_vec <- as.vector(t(cost_mat))[idxEli_vec]
  
  varInfo_list <- VarInfo(eli_vec,callInfo_df,resource_vec,callId_vec)
  varName_vec <- varInfo_list$varName_vec; 
  varNum <- varInfo_list$varNum
  varAmount_vec <- ResultList2AmountVec(callOutput_list,callId_vec,varName_vec[1:varNum])
  
  #### Costs
  dailyCost <- CostFun(varAmount_vec,cost_vec)
  monthlyCost <- dailyCost*30
  dailyCost <- round(dailyCost,2)
  monthlyCost <- round(monthlyCost,2)
  
  #### Movements
  varAmount_mat <- VarVec2mat(varAmount_vec[1:varNum],varName_vec[1:varNum],callId_vec,resource_vec)
  movements <- OperationFun(varAmount_mat,callInfo_df,'matrix')
  
  #### Liquidity
  availInfoOri_list <- AssetByCallInfo(callId_vec,resourceOri_vec,availAssetOri_df)
  liquidity_vec <- apply((1-availInfoOri_list$haircut_mat)^2,2,min)
  
  qtyLeft <- resourceOri_df$qtyMin
  idx_vec <- match(resource_df$id,resourceOri_df$id)
  qtyLeft[idx_vec] <- resource_df$qtyMin
  
  reservedLiquidityRatio <- LiquidFun(qtyLeft,resourceOri_df$qtyMin,liquidity_vec,resourceOri_df$minUnitValue/resourceOri_df$FXRate)
  
  resultAnalysis <- list(dailyCost=dailyCost,monthlyCost=monthlyCost,movements=movements,reservedLiquidityRatio=reservedLiquidityRatio)
  #### Result Analysis Output END #########################
  
  ############ ITERATE THE GROUP, RUN THE ALGO END #########################
  
  return(list(#msOutput=msOutput_list,
    callOutput=callOutput_list,checkCall_mat=checkCall_mat,
    solverStatus=solverStatus,lpsolveRun=lpsolveRun,solverObjValue=solverObjValue,resultAnalysis=resultAnalysis))
}

PreAllocation <- function(algoVersion,callId_vec,callInfo_df,availAsset_df,resource_df,
                          pref_vec,operLimit,operLimitMs_vec,fungible,minMoveValue,timeLimit,
                          ifNewAlloc,allocated_list){
  ## callInfo_df: in group
  msId_vec <- unique(callInfo_df$marginStatement)
  callNum <- length(callId_vec)
  
  resource_vec <- unique(availAsset_df$assetCustacId)
  
  callOutput_list <- list()
  checkCall_mat <- matrix(c(callInfo_df$callAmount,rep(0,callNum)),nrow=callNum, dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
  
  for(i in 1:length(msId_vec)){
    msId <- msId_vec[i]
    operLimitMs <- operLimitMs_vec[i]
    callIdx_vec <- which(callInfo_df$marginStatement==msId)
    callInThisMs_vec <- callInfo_df$id[callIdx_vec]
    
    callInfoGroup_df <- callInfo_df[match(callInThisMs_vec,callInfo_df$id),]
    availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callInThisMs_vec),]
    resourceGroup_vec <- unique(availAssetGroup_df$assetCustacId)
    resourceGroup_df <- resource_df[match(resourceGroup_vec,resource_df$id),]
    availInfoGroup_list <- AssetByCallInfo(callInThisMs_vec,resourceGroup_vec,availAssetGroup_df)
    
    idxTemp_vec <- match(callInThisMs_vec,names(allocated_list))
    allocatedGroup_list <- allocated_list[idxTemp_vec]
    # core Algo, assume all data comes in a list
    if(algoVersion==1){
      resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)
    } else if(algoVersion==2){
      resultGroup_list <- CoreAlgoV2(callInfoGroup_df, resourceGroup_df, availInfoGroup_list,
                                     timeLimit,pref_vec,operLimit,operLimitMs,fungible,minMoveValue,
                                     ifNewAlloc,list(),allocatedGroup_list)
    }
    
    #msOutputGroup_list <- resultGroup_list$msOutput_list
    callOutputGroup_list <- resultGroup_list$callOutput_list
    solverStatus <- resultGroup_list$solverStatus
    lpsolveRun <- resultGroup_list$lpsolveRun
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
    quantityUsed_vec <- UsedQtyFromResultList(callOutputGroup_list,resource_vec,callId_vec)
    resource_df$qtyMin <- resource_df$qtyMin - quantityUsed_vec/resource_df$minUnit
    
  }
  resultPre_list <- list(checkCall_mat=checkCall_mat,callOutput_list=callOutput_list)
  return(resultPre_list)
}