
#### Main Function-Interface of Java Start #######
CallAllocation <- function(algoVersion,scenario,callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit){
  #### Scenario Code Start #########
  # scenario = 1, Algo suggestion
  # scenario = 2, post settlement cash only
  # scenario = 3, post least liquid assets
  #### Scenario Code END ###########
  
  inputLimit_vec <- c(7,7,7,5); 
  timeLimit=20; 
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
      numTemp <- length(idxTemp_vec)
      count <- count+numTemp
      idxKeep_vec[(count-numTemp+1):count] <- idxTemp_vec
    }
    idxKeep_vec <- idxKeep_vec[1:count]
    availAssetCash_df <- availAssetCash_df[idxKeep_vec,]
    resourceCash_vec <- unique(availAssetCash_df$assetCustacId)
    assetIdCash_vec <- matrix(unlist(strsplit(resourceCash_vec,'-')),nrow=2)[1,]
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
    assetIdGroup_vec <- matrix(unlist(strsplit(resourceGroup_vec,'-')),nrow=2)[1,]
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
      availAssetPre_df <- res$availAsset_df
      #availAssetPre_df[which(availAssetPre_df$callId %in% callId),] <- availAssetPreGroup_df
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
    
    #msOutputGroup_list <- resultGroup_list$msOutput_list
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
  availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
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
  
  solveOutput_list <- list(resultGroup_list=resultGroup_list,availAsset_df=availAsset_df,checkCall_mat=checkCall_mat,callOutput_list=callOutput_list)
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


AllocationInputData <- function(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df){
  
  ### new identifer ####
  assetId_vec <- SplitResource(resource_vec,'asset')
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  callInfo_df$currency[which(is.na(callInfo_df$currency))] <- 'ZZZ' 
  availAsset_df <- availAsset_df[order(availAsset_df$callId),] # order the availAsset_df by callId_vec
  custodianAccount <- matrix(unlist(strsplit(resource_vec,'-')),nrow=2)[2,]
  venue <- availAsset_df$venue[match(resource_vec,availAsset_df$assetCustacId)]
  
  ###############################################
  # eligibility matrix: 1-eligible, 0-ineligible
  # haircut matrix: haircut+FX haircut
  # tempQuantity_vec matrix
  # unitValue matrix: unitValue/FX rate
  # cost matrix: internal+external+opptunity-yield(interestRate)
  # call amount matrix: duplicate the column
  # minUnit matrix: minUnit[i,j]=x, asset j for margin call i has a minimum denomination x,
  
  #     which means we can only allocate the integral multiples tempQuantity_vec of A_j to MC_i.
  #     To start with, we use (i>=1) for non-cash securities; 0.01 for cash, apply to all margin calls.
  ############################################
  
  base_mat <- matrix(0,nrow=callNum,ncol=resourceNum, dimnames = list(callId_vec,resource_vec))
  eli_mat <- base_mat
  haircut_mat <- base_mat
  cost_mat <- base_mat
  quantity_mat <- base_mat
  minUnitQuantity_mat <- base_mat
  callAmount_mat <- base_mat
  
  unitValue_mat<- base_mat
  minUnit_mat <- base_mat  
  minUnitValue_mat <- base_mat
  
  # fill in matrixes with the data from availAsset_df
  
  callAmount_mat[]<- matrix(rep(callInfo_df$callAmount,resourceNum),nrow=callNum,byrow=F)
  idxTempCallId_vec <- match(availAsset_df$callId,callId_vec)
  
  #resource_vec <- availAsset_df$assetCustacId
  idxTempResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  quantity_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$quantity
  eli_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- 1
  haircut_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$haircut+availAsset_df$FXHaircut
  cost_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-(availAsset_df$interestRate+availAsset_df$yield)
  
  #### restructure!
  resourceInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]
  
  unitValue_mat[] <- matrix(rep(resourceInfo_df$unitValue/resourceInfo_df$FXRate,callNum),nrow=callNum,byrow=TRUE)
  minUnit_mat[]<- matrix(rep(resourceInfo_df$minUnit,callNum),nrow=callNum,byrow=TRUE)
  minUnitValue_mat[] <- matrix(rep(resourceInfo_df$minUnitValue/resourceInfo_df$FXRate,callNum),nrow=callNum,byrow=TRUE)
  minUnitQuantity_mat[]<- floor(quantity_mat/minUnit_mat) # round down to the nearest integer
  
  # convert the matrix format data to vector format
  # thinking of keeping only eligible parts
  eli_vec <- as.vector(t(eli_mat))
  haircut_vec <- as.vector(t(haircut_mat))
  cost_vec <- as.vector(t(cost_mat))
  quantity_vec <- as.vector(t(quantity_mat))
  minUnitQuantity_vec <- as.vector(t(minUnitQuantity_mat))
  unitValue_vec <- as.vector(t(unitValue_mat))
  minUnit_vec <- as.vector(t(minUnit_mat))
  minUnitValue_vec <- as.vector(t(minUnitValue_mat))
  callAmount_vec <- as.vector(t(callAmount_mat))
  
  output_list <- list(resource_vec=resource_vec,callId_vec=callId_vec,assetInfo_df=assetInfo_df,callInfo_df=callInfo_df,
                      custodianAccount=custodianAccount,venue=venue,
                      base_mat=base_mat,
                      eli_mat=eli_mat, eli_vec = eli_vec,
                      haircut_mat=haircut_mat, haircut_vec=haircut_vec,
                      cost_mat = cost_mat, cost_vec = cost_vec,
                      quantity_mat=quantity_mat, quantity_vec=quantity_vec,
                      minUnitQuantity_mat=minUnitQuantity_mat,minUnitQuantity_vec=minUnitQuantity_vec,
                      unitValue_mat=unitValue_mat,unitValue_vec=unitValue_vec,
                      minUnit_mat=minUnit_mat, minUnit_vec=minUnit_vec,
                      minUnitValue_mat=minUnitValue_mat,minUnitValue_vec= minUnitValue_vec,
                      callAmount_mat = callAmount_mat,callAmount_vec=callAmount_vec
  )
  return (output_list)
}

ResultMat2List <- function(result_mat,resource_vec,assetId_vec,availAsset_df,coreInput_list,callSelect_list,msSelect_list){
  venue <- coreInput_list$venue
  custodianAccount <- coreInput_list$custodianAccount
  assetInfo_df <- coreInput_list$assetInfo_df
  callInfo_df <- coreInput_list$callInfo_df
  unitValue_mat <- coreInput_list$unitValue_mat
  minUnit_mat <- coreInput_list$minUnit_mat
  haircut_mat <- coreInput_list$haircut_mat
  msId_vec <- unique(callInfo_df$marginStatement)
  callId_vec <- coreInput_list$callId_vec
  callNum <- length(callId_vec)
  
  for(i in 1:callNum){                          # store the result into select list
    # j, corresponding index of margin statement
    # j <- which(msId_vec==callInfo_df$marginStatement[which(callInfo_df$id==callId_vec[i])])
    j <- which(msId_vec==callInfo_df$marginStatement[i])
    
    idxSelectResource_vec <- which(result_mat[i,]!=0)
    
    selectResource_vec <- resource_vec[idxSelectResource_vec]
    selectAssetId_vec <- assetId_vec[idxSelectResource_vec]
    idxSelectAsset_vec <- rep(0,length(idxSelectResource_vec))
    for(m in 1:length(idxSelectResource_vec)){
      idxSelectAsset_vec[m] <- which(assetInfo_df$id==selectAssetId_vec[m])[1]
    }
    selectAssetCustodianAccount_vec <- custodianAccount[idxSelectResource_vec]
    selectAssetVenue_vec <- venue[idxSelectResource_vec]
    selectAssetName_vec <- assetInfo_df$name[idxSelectAsset_vec]
    selectAssetHaircut_vec <- haircut_mat[i,idxSelectResource_vec]
    selectAssetCurrency_vec <- assetInfo_df$currency[idxSelectAsset_vec]
    selectAssetMinUnitQuantity_vec <- result_mat[i,idxSelectResource_vec]
    selectAssetQuantity_vec <- result_mat[i,idxSelectResource_vec]*minUnit_mat[i,idxSelectResource_vec]
    selectMarginType_vec <- rep(callInfo_df$marginType[i],length(idxSelectResource_vec))
    selectMs_vec <- rep(callInfo_df$marginStatement[i],length(idxSelectResource_vec))
    selectCall_vec <- rep(callId_vec[i],length(idxSelectResource_vec))
    
    #### UPDATE THE ASSET QUANTITY ########
    for(k in 1:length(selectAssetId_vec)){
      tempResource <- selectResource_vec[k]
      tempAvailQuantity_vec <- availAsset_df$quantity[which(availAsset_df$assetCustacId==tempResource)]
      tempQuantity_vec <- availAsset_df$totalQuantity[which(availAsset_df$assetCustacId==tempResource)]
      availAsset_df$quantity[which(availAsset_df$assetCustacId==tempResource)]<- tempAvailQuantity_vec-selectAssetQuantity_vec[k]
      availAsset_df$totalQuantity[which(availAsset_df$assetCustacId==tempResource)]<- tempQuantity_vec-selectAssetQuantity_vec[k]
    }
    #### END ##############################
    
    selectAssetUnitValue_vec <- unitValue_mat[i,idxSelectResource_vec]
    selectAssetAmountUSD_vec <- round(selectAssetQuantity_vec*selectAssetUnitValue_vec,2)
    selectAssetNetAmountUSD_vec <- selectAssetAmountUSD_vec*(1-haircut_mat[i,idxSelectResource_vec])
    selectAssetFX_vec <- assetInfo_df$FXRate[idxSelectAsset_vec]
    selectAssetAmount_vec <- selectAssetAmountUSD_vec*selectAssetFX_vec
    selectAssetNetAmount_vec <- selectAssetNetAmountUSD_vec*selectAssetFX_vec
    #######
    # netAmount(in local currency) is surfacing in UI
    #######
    selectAsset_df <- data.frame(selectAssetId_vec,selectAssetName_vec,selectAssetNetAmount_vec,selectAssetNetAmountUSD_vec,selectAssetFX_vec,selectAssetHaircut_vec,selectAssetAmount_vec,selectAssetAmountUSD_vec,selectAssetCurrency_vec,
                                 selectAssetQuantity_vec,selectAssetCustodianAccount_vec,selectAssetVenue_vec,selectMarginType_vec,selectMs_vec,selectCall_vec)
    colnames(selectAsset_df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType','marginStatement','marginCall')
    
    callSelect_list[[callId_vec[i]]] <- selectAsset_df
    if(is.null(msSelect_list[[msId_vec[j]]])){
      msSelect_list[[msId_vec[j]]] <- selectAsset_df
    } else{
      tempAsset_df <- msSelect_list[[msId_vec[j]]]
      selectAsset_df <- rbind(selectAsset_df,tempAsset_df)
      msSelect_list[[msId_vec[j]]] <- selectAsset_df
    }
  }
  
  result_list <- list(callSelect_list=callSelect_list,  msSelect_list=msSelect_list,
                      availAsset_df=availAsset_df)
  return(result_list)
}

ResultList2Mat <- function(callOutput_list,callId_vec,resource_vec,minUnit_mat){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  result_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
  
  for(m in 1:callNum){
    callId <- callId_vec[m]
    callAlloc_df <- callOutput_list[[callId]]
    
    # the 'Quantity'= decision variable * minUnit
    # find the corresponding decision variable index from the varName
    resourceTemp_vec <- PasteResource(callAlloc_df$Asset,callAlloc_df$CustodianAccount)
    #varNameTemp_vec <- PasteVarName(callAlloc_df$marginStatement,callAlloc_df$marginCall,resourceTemp_vec)
    
    idxTemp_vec <- match(resourceTemp_vec,resource_vec)
    
    quantityTemp_vec <- callAlloc_df$Quantity
    minUnitQuantityTemp_vec <- quantityTemp_vec/minUnit_mat[m,idxTemp_vec]
    
    result_mat[m,idxTemp_vec] <- minUnitQuantityTemp_vec
  }
  return(result_mat)
}

CallList2Var <- function(callOutput_list,callId_vec,minUnit_vec,varName_vec,varNum3,varNum,idxEli_vec){
  #varnum <- length(varName_vec)
  var_vec <- rep(0,varNum3)
  callNum <- length(callId_vec)
  
  for(m in 1:callNum){
    callId <- callId_vec[m]
    callAlloc_df <- callOutput_list[[callId]]
    
    # the 'Quantity'= decision variable * minUnit
    # find the corresponding decision variable index from the varName
    resourceTemp_vec <- PasteResource(callAlloc_df$Asset,callAlloc_df$CustodianAccount)
    varNameTemp_vec <- PasteVarName(callAlloc_df$marginStatement,callAlloc_df$marginCall,resourceTemp_vec)
    
    minUnitEli_vec <- minUnit_vec[idxEli_vec]
    
    for(k in 1:length(resourceTemp_vec)){
      idxVarTemp <- which(varName_vec==varNameTemp_vec[k])
      quantityTemp <- callAlloc_df$Quantity[k]
      
      var_vec[idxVarTemp] <- quantityTemp/minUnitEli_vec[idxVarTemp]
      var_vec[idxVarTemp+varNum] <- 1
    }
    # if inside one margin statement, two margin calls are using the same asset, 
    # then assign 1 to (varNum3-varNum2)
  }
  return(var_vec)
}

VarInfo <- function(eli_vec,callInfo_df,resource_vec,callId_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  # matrix store the index number, by row
  idx_mat <- matrix(1:(callNum*resourceNum),nrow=callNum,byrow = TRUE,dimnames = list(callId_vec,resource_vec))
  # matrix store the variable name("msId_mcId_assetCustId"), by row
  fullNameOri_mat <-  matrix('',nrow=callNum,ncol=resourceNum,byrow = TRUE,dimnames = list(callId_vec,resource_vec))
  for(i in 1:callNum){
    msId <- callInfo_df$marginStatement[i]
    fullNameOri_mat[i,]<-paste(msId,callId_vec[i],resource_vec,sep='_')
  }
  fullNameDummy_mat <- fullNameOri_mat
  fullNameDummy_mat[] <- paste(fullNameOri_mat,'dummy',sep='-')
  
  fullNameAll_mat <- rbind(fullNameOri_mat,fullNameDummy_mat)
  
  splitName_mat <- matrix(unlist(strsplit(t(fullNameAll_mat),'_')),ncol=3,byrow=TRUE)
  splitName_mat <- cbind(splitName_mat,1:(2*length(idx_mat)))
  colnames(splitName_mat) <- c('msId','callId','assetCustId','index'); rownames(splitName_mat)<- 1:(2*length(idx_mat))
  
  ## filter the eligible/available asset variables
  idxEli_vec <- which(eli_vec==1)  
  ## variable numbers
  varNum <- length(idxEli_vec)    # variable numbers
  varNum2 <- varNum*2
  
  varName_vec <- c(t(fullNameOri_mat)[idxEli_vec],t(fullNameDummy_mat)[idxEli_vec])
  varSplitName_mat <- rbind(splitName_mat[idxEli_vec,],splitName_mat[length(idx_mat)+idxEli_vec,])
  
  # update the variable index
  varSplitName_mat[,4]<-1:varNum2
  varDummy_mat <- varSplitName_mat[(varNum+1):varNum2,]
  
  ### Margin Statement ###
  marginTypeNum <- 2
  msVarNum <- 0
  msVar_mat <- matrix(0,nrow=ceiling(varNum/2),ncol=3,dimnames=list(1:ceiling(varNum/2),c("index_1","index_2","index")))
  
  msId_vec <- unique(callInfo_df$marginStatement)
  for(i in 1:length(msId_vec)){
    tempCallId_vec <- which(callInfo_df$marginStatement==msId_vec[i])
    tempLength <- length(tempCallId_vec)
    if(tempLength==2){
      idxTemp <- which(varDummy_mat[,1]==msId_vec[i])
      temp_df <- as.data.frame(table(varDummy_mat[which(varDummy_mat[,1]==msId_vec[i]),3]))
      
      # When IM and VM in the same margin statement have the same available asset
      # the frequency = 2 (assume only one IM and one VM in one margin statement)
      
      idxTempRep_vec <- which(temp_df[,2]==2)
      if(length(idxTempRep_vec)>=1){ # at least one asset is eligible for both IM and VM
        
        tempResource_vec <- as.character(temp_df[idxTempRep_vec,1])
        
        temp_mat <- varDummy_mat[idxTemp,]
        temp2_mat <- temp_mat[which(!is.na(match(temp_mat[,3],tempResource_vec))),]
        
        tempRes_df <- aggregate(index~msId+assetCustId,data=temp2_mat,PasteFun1)
        tempPair_vec <- tempRes_df[,3]
        idxTempNew_vec <- msVarNum+1:length(tempPair_vec)
        msVarNum <- msVarNum + length(tempPair_vec)
        msVar_mat[idxTempNew_vec,c(1,2)] <- as.numeric(t(data.frame((strsplit(tempPair_vec,'_')))))
        msVar_mat[idxTempNew_vec,3] <- idxTempNew_vec+varNum2
      }
    }
  }
  # update msVar_mat
  if(msVarNum==0){
    msVar_mat <- msVar_mat[-c(1:length(msVar_mat[,1])),]
  } else{
    msVar_mat <- msVar_mat[1:msVarNum,]
    msVar_mat <- matrix(as.numeric(msVar_mat),ncol=3)
  }
  
  ## add new auxiliary variables, two margin calls coming from one margin statement
  varNum3 <- varNum2+msVarNum
  msVarName_mat <- varSplitName_mat[msVar_mat[,1],]
  if(is.vector(msVarName_mat)){
    msVarName_mat[2] <- msVarName_mat[1]
    msVarName_mat[4] <- msVar_mat[3]
    
    msVarName_vec <- PasteFun2(msVarName_mat[1:3]) 
  } else{
    msVarName_mat[,2] <- msVarName_mat[,1]
    msVarName_mat[,4] <- msVar_mat[,3]
    
    msVarName_vec <- apply(msVarName_mat[,1:3],1,PasteFun2)
  }
  
  varName_vec <- c(varName_vec,msVarName_vec)
  
  var_list <- list(varName_vec=varName_vec,varNum=varNum,varNum2=varNum2,varNum3=varNum3,msVar_mat=msVar_mat)
  return(var_list)
}

renjinFix <- function(frame, name) {
  d <- data.frame(frame);
  colnames(d) <- gsub(name, "", colnames(d));
  return(d);
}

callList2AmountVec <- function(callOutput_list,callId_vec,varName_vec){
  callNum <- length(callId_vec)
  varNum <- length(varName_vec)
  var_vec <- rep(0,varNum)
  
  for(i in 1:callNum){
    callId <- callId_vec[i]
    currentAlloc_df <- callOutput_list[[callId]]
    currentResource_vec <- PasteResource(currentAlloc_df$Asset,currentAlloc_df$CustodianAccount)
    currentVarName_vec <- PasteVarName(currentAlloc_df$marginStatement,currentAlloc_df$marginCall,currentResource_vec)
    currentAmount_vec <- currentAlloc_df$`Amount(USD)`
    #currentQuantity_vec <- currentAlloc_df$Quantity
    #currentVarValue_vec <- currentQuantity_vec/minUnit_vec
    currentVarLoc_vec <- match(currentVarName_vec,varName_vec)
    
    ## fill in the var_vec
    var_vec[currentVarLoc_vec] <- currentAmount_vec
  }
  return(var_vec)
}

VarVec2mat <- function(var_vec,varName_vec,callId_vec,resource_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  # row1: ms; row2: call; row3: resource.
  target <- 'all'
  varName_mat <- SplitVarName(varName_vec,target)
  var_mat <- matrix(0,nrow=callNum,ncol=resourceNum, dimnames = list(callId_vec,resource_vec))
  
  for(i in 1:callNum){
    callId <- callId_vec[i]
    idxTemp_vec <- which(varName_mat[2,]==callId) # the indexes of decision variables
    currentResource_vec <- varName_mat[3,idxTemp_vec]
    currentValue <- var_vec[idxTemp_vec]
    currentLoc_vec <- match(currentResource_vec,resource_vec)
    
    var_mat[i,currentLoc_vec] <- currentValue
  }
  return(var_mat)
}

LiquidFun <- function(quantity_vec,quantityTotal_vec,liquidity_vec,minUnitValue_vec){
  numerator <- sum(quantity_vec*liquidity_vec*minUnitValue_vec)
  denominator <- sum(quantityTotal_vec*liquidity_vec*minUnitValue_vec)
  ratio <- numerator/denominator
  return(ratio)
}

CostFun <- function(amount_vec,cost_vec){
  cost <- sum(amount_vec*cost_vec)
  return(cost)
}

OperationFun <- function(result,callInfo_df,method){
  movements <- 0
  if(method=='matrix'){
    result_mat <- result
    resultDummy_mat <- 1*(result_mat&1)
    msDul_vec <- callInfo_df$marginStatement
    msId_vec <- unique(msDul_vec)
    
    if(length(result_mat[1,])==1){
      for(m in 1:length(msId_vec)){
        idxTemp_vec <- which(msDul_vec==msId_vec[m])
        if(length(idxTemp_vec)==1){
          movements <- movements+sum(resultDummy_mat[idxTemp_vec])
        } else{
          movements <- movements+max(resultDummy_mat[idxTemp_vec])
        }
      }
    } else{
      for(m in 1:length(msId_vec)){
        idxTemp_vec <- which(msDul_vec==msId_vec[m])
        if(length(idxTemp_vec)==1){
          movements <- movements+sum(resultDummy_mat[idxTemp_vec,])
        } else{
          movements <- movements+sum(apply(resultDummy_mat[idxTemp_vec,],2,max))
        }
      }
    }
  } else if(method=='msList'){
    msOutput_list <- result
    msId_vec <- unique(callInfo_df$marginStatement)
    msNum <- length(msId_vec)
    for(i in 1:msNum){
      msId <- msId_vec[i]
      msAlloc_df <- msOutput_list[[msId]]
      resources <- unique(PasteResource(msAlloc_df$Asset,msAlloc_df$CustodianAccout))
      movements <- movements + length(resources)
    }
  } else if(method=='callList'){
    callOutput_list <- result
    callId_vec <- callInfo_df$id
    msId_vec <- unique(callInfo_df$marginStatement)
    callNum <- length(callId_vec)
    msNum <- length(msId_vec)
    for(i in 1:msNum){
      msId <- msId_vec[i]
      callIds <- callInfo_df$id[which(callInfo_df$marginStatement==msId)]
      
      callId <- callIds[1]
      msAlloc_df <- callOutput_list[[callId]]
      if(length(callIds>1)){
        # inside one margin statement
        for(m in 2:length(callIds)){
          callId <- callIds[m]
          callAlloc_df <- callOutput_list[[callId]]
          msAlloc_df <- rbind(callAlloc_df,msAlloc_df)
        }
      }
      resources <- unique(PasteResource(msAlloc_df$Asset,msAlloc_df$CustodianAccout))
      movements <- movements + length(resources)
    }
    
  } else{
    stop('Please input a valid method!')
  }
  
  return(movements)
}

PasteFun1 <- function(x1='',x2=''){
  temp=paste(x1,x2,sep='_',collapse = '')
  return(temp)
}

PasteFun2 <- function(x){
  temp=paste(x,collapse='_')
  return(temp)
}

PasteResource <- function(assetId_vec,custodianAccount_vec){
  temp <- paste(assetId_vec,custodianAccount_vec,sep='-')
  return(temp)
}

PasteVarName <- function(msId_vec,callId_vec,resource_vec){
  temp <- paste(msId_vec,callId_vec,resource_vec,sep='_')
  return(temp)
}

SplitResource <- function(resource_vec,target){
  resource_mat <- matrix(unlist(strsplit(resource_vec,'-')),nrow=2)
  if(missing(target)){
    target <- 'all'
  }
  if(target=='asset'){
    return(resource_mat[1,])
  } else if(target=='custodianAccount'){
    return(resource_mat[2,])
  } else{
    return(resource_mat)
  }
}

SplitVarName <- function(varName_vec,target){
  varName_mat <- matrix(unlist(strsplit(varName_vec,'_')),nrow=3)
  if(missing(target)){
    target <- 'all'
  }
  if(target=='call'){
    return(varName_mat[1,])
  } else if(target=='ms'){
    return(varName_mat[2,])
  } else if(target=='resource'){
    return(varName_mat[3,])
  } else if(target=='all'){
    return(varName_mat)
  } else{
    stop('Please input a valid target!')
  }
}

