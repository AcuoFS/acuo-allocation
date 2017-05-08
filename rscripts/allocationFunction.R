
#### Main Function-Interface of Java Start #######
CallAllocation <- function(algoVersion,scenario,callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit,operLimitMs,fungible){
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
                             callInfo_df,availAsset_df,availAsset_df,assetInfo_df,assetInfo_df,pref_vec,operLimit,operLimitMs,fungible,
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
                             callInfo_df,availAssetCash_df,availAsset_df,assetInfoCash_df,assetInfo_df,pref_vec,operLimit,operLimitMs,fungible,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod)
  } else if(scenario==3){
    pref_vec <- c(0,10,0)
    result <- AllocationAlgo(callId_vec,resource_vec,resource_vec,
                             callInfo_df,availAsset_df,availAsset_df,assetInfo_df,assetInfo_df,pref_vec,operLimit,operLimitMs,fungible,
                             algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod)
  } else{
    stop('Please input a valid scenario!')
  }
  return(result)
}
#### Main Function-Interface of Java End #########


#### CONNECT TO THE CORE MODULE OF OPTIMIZATION ####################
AllocationAlgo <- function(callId_vec,resource_vec,resourceOri_vec,callInfo_df,availAsset_df,availAssetOri_df,assetInfo_df,assetInfoOri_df,pref_vec,operLimit,operLimitMs,fungible,
                           algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod){
  
  #### Input Prepare & Output Initialization Start ###########
  #### Order callId_vec
  callInfo_df <- OrderCallId(callOrderMethod,callInfo_df)
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
  if(operLimitMs==1){
    for(i in 1:msNum){
      msId <- msId_vec[i]
      callIdOneMs_vec <- callInfo_df$id[which(callInfo_df$marginStatement==msId)]
      # assume one margin statement contains either 1 or 2 margin calls
      if(length(callIdOneMs_vec)==2){
        # find the lines of available resources for these two calls
        idx1_vec <- which(availAsset_df$callId==callIdOneMs_vec[1]) 
        idx2_vec <- which(availAsset_df$callId==callIdOneMs_vec[2]) 
        
        # find the common resources
        resource1_vec <- availAsset_df$assetCustacId[idx1_vec]
        resource2_vec <- availAsset_df$assetCustacId[idx2_vec]
        sameResource_vec <- intersect(resource1_vec,resource2_vec)
        if(length(sameResource_vec)==0){ # not possible normally
          errormsg <- paste('There is no common asset can be allocated for both',callIdOneMs_vec,'in',msId)
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
        newIdx1_vec <- which(availAsset_df$callId==callIdOneMs_vec[1]) 
        newIdx2_vec <- which(availAsset_df$callId==callIdOneMs_vec[2]) 
        
        callAmount1 <- callInfo_df$callAmount[which(callInfo_df$id==callIdOneMs_vec[1])]
        callAmount2 <- callInfo_df$callAmount[which(callInfo_df$id==callIdOneMs_vec[2])]
        
        haircut1_vec <- availAsset_df$haircut[newIdx1_vec]+availAsset_df$FXHaircut[newIdx1_vec]
        haircut2_vec <- availAsset_df$haircut[newIdx2_vec]+availAsset_df$FXHaircut[newIdx2_vec]
        
        minUnitValue1_vec <- availAsset_df$minUnitValue[newIdx1_vec]
        minUnitValue2_vec <- availAsset_df$minUnitValue[newIdx2_vec]
        
        quantity_vec <- availAsset_df$quantity[newIdx1_vec]/availAsset_df$minUnit[newIdx1_vec]
        #quantity_vec <- availAsset_df$quantity[newIdx2_vec] # the same 
        
        integralSuffQty1_vec <- ceiling(callAmount1/(1-haircut1_vec)/minUnitValue1_vec)
        integralSuffQty2_vec <- ceiling(callAmount2/(1-haircut2_vec)/minUnitValue2_vec)
        integralSuffQty_vec <- integralSuffQty1_vec+integralSuffQty2_vec
        
        suffIdx_vec <- which(quantity_vec >= integralSuffQty_vec)
        if(length(suffIdx_vec)==0){ # not possible normally
          errormsg <- paste('There is no asset which is sufficient for both',callIdOneMs_vec,'in',msId)
          stop(errormsg)
        }
        
        ## remove the insufficient assets
        rmIdx1_vec <- newIdx1_vec[-suffIdx_vec] # lines not sufficient
        rmIdx2_vec <- newIdx2_vec[-suffIdx_vec] 
        
        if(length(c(rmIdx1_vec,rmIdx2_vec))>=1){
          availAsset_df <- availAsset_df[-c(rmIdx1_vec,rmIdx2_vec),]
        }
      }
    }
    # update the available resource based on the new availAsset_df
    resource_vec <- unique(availAsset_df$assetCustacId)
  }
  #### Case When Movement Limit Is 1 Per Margin Statement END ######
  
  ############ ITERATE THE GROUP, RUN THE ALGO Start #########################
  
  for(i in 1:length(groupCallId_list)){
    
    callIdGroup_vec <- groupCallId_list[[i]]
    msIdGroup_vec <- unique(callInfo_df$marginStatement[which(callInfo_df$id %in% callIdGroup_vec)])
    ratio <- length(msIdGroup_vec)/length(msId_vec) # the proportion of the msGroup in the msList
    operLimitGroup <- operLimit*ratio
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
    for(ms in 1:length(msIdGroup_vec)){
      msId <- msIdGroup_vec[ms]
      callIdx_vec <- which(callInfoGroup_df$marginStatement==msId)
      callIdOneMs_vec <- callInfoGroup_df$id[callIdx_vec]
      res <- PreAllocation(algoVersion,callIdOneMs_vec,callInfoPre_df,availAssetPre_df,assetInfoPre_df,pref_vec,operLimitMs,operLimitMs,fungible,minMoveValue,timeLimit,callOutput_list,checkCall_mat)
      availAssetPreGroup_df <- res$availAssetGroup_df # availAssetPreGroup_df: the available asset for current margin call(done in PreAllocate)
      
      
      for(k in 1:length(availAssetPreGroup_df$assetCustacId)){
        updateResource <- availAssetPreGroup_df$assetCustacId[k]
        updateQuantity <- availAssetPreGroup_df$quantity[k]
        updateIdx_vec <- which(availAssetPre_df$assetCustacId==updateResource)
        availAssetPre_df$quantity[updateIdx_vec] <- updateQuantity
      }
      
      callOutputPreGroup_list <- res$callOutput_list
      checkCallPre_mat <- res$checkCall_mat
      for(c in 1:length(callIdOneMs_vec)){
        callId <- callIdOneMs_vec[c]
        callOutputPre_list[[callId]] <- callOutputPreGroup_list[[callId]]
      }
    }
    
    # parameteres need to pass to the CoreAlgoV2
    # callOutputPre_list
    # availAssetPre_df # don't need, solver will auto deduct the quantity while solving
    # combine into a single list: preAllocation_list
    #cat('callIdGroup_vec',callIdGroup_vec,'\n')
    #print(callOutputPre_list)
    initAllocation_list <- callOutputPre_list # currently, store all the cumulated margin calls
    #### Pre-allocate End ########################
    
    #### Run CoreAlgo Start ######################
    if(algoVersion==1){
      resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)#,initAllocation_list)
    } else if(algoVersion==2){
      resultGroup_list <- CoreAlgoV2(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,operLimitGroup,operLimitMs,fungible,minMoveValue,initAllocation_list)
    }
    #### Run CoreAlgo END ########################
    
    msOutputGroup_list <- resultGroup_list$msOutput_list
    callOutputGroup_list <- resultGroup_list$callOutput_list
    
    solverStatus <- resultGroup_list$solverStatus
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
      msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
      checkCall_mat[which(rownames(checkCall_mat)==callId),2] <- checkCallGroup_mat[which(rownames(checkCallGroup_mat)==callId),2]
    }
  }
  
  
  #### Result Analysis Output Start #####################
  coreInput_list <- AllocationInputData(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df)
  eli_vec <- coreInput_list$eli_vec; idxEli_vec <- which(eli_vec==1)
  varInfo_list <- VarInfo(eli_vec,callInfo_df,resource_vec,callId_vec)
  varName_vec <- varInfo_list$varName_vec; varNum <- varInfo_list$varNum
  varAmount_vec <- ResultList2AmountVec(callOutput_list,callId_vec,varName_vec[1:varNum])
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
  
  liquidity_vec <- apply((1-coreInputOri_list$haircut_mat)^2,2,min)
  minUnitValue_vec <- apply(coreInputOri_list$minUnitValue_mat,2,max)
  
  #cat('liquidity_vec',liquidity_vec,'\n')
  #cat('minUnitValue_vec',minUnitValue_vec,'\n')
  reservedLiquidityRatio <- LiquidFun(quantityRes_vec,quantityTotal_vec,liquidity_vec,minUnitValue_vec)
  
  resultAnalysis <- list(dailyCost=dailyCost,monthlyCost=monthlyCost,movements=movements,reservedLiquidityRatio=reservedLiquidityRatio)
  #### Result Analysis Output END #########################
  
  
  
  ############ ITERATE THE GROUP, RUN THE ALGO END #########################
  
  return(list(#msOutput=msOutput_list,
    callOutput=callOutput_list,checkCall_mat=checkCall_mat,#availAsset_df=availAsset_df,
    solverStatus=solverStatus,lpsolveRun=lpsolveRun,solverObjValue=solverObjValue,resultAnalysis=resultAnalysis))
}

PreAllocation <- function(algoVersion,callIdGroup_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit,operLimitMs,fungible,minMoveValue,timeLimit,callOutput_list,checkCall_mat){
  
  msIdGroup_vec <- unique(callInfo_df$marginStatement[which(callInfo_df$id %in% callIdGroup_vec)])
  callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
  availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
  resourceGroup_vec <- unique(availAssetGroup_df$assetCustacId)
  assetIdGroup_vec <- SplitResource(resourceGroup_vec,'asset')
  assetInfoGroup_df <- assetInfo_df[match(assetIdGroup_vec,assetInfo_df$id),]
  
  # input data to the core Algo
  coreInput_list <- AllocationInputData(callIdGroup_vec,resourceGroup_vec,callInfoGroup_df,availAssetGroup_df,assetInfoGroup_df)
  
  # core Algo, assume all data comes in a list
  if(algoVersion==1){
    resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)
  } else if(algoVersion==2){
    resultGroup_list <- CoreAlgoV2(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,operLimit,operLimitMs,fungible,minMoveValue)
  }
  #msOutputGroup_list <- resultGroup_list$msOutput_list
  callOutputGroup_list <- resultGroup_list$callOutput_list
  solverStatus <- resultGroup_list$solverStatus
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
  
  solveOutput_list <- list(availAssetGroup_df=availAssetGroup_df,checkCall_mat=checkCall_mat,solverStatus=solverStatus,callOutput_list=callOutput_list)
  return(solveOutput_list)
}
