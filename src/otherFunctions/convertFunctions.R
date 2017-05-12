
ResultMat2List <- function(result_mat,resource_vec,availAsset_df,coreInput_list,callSelect_list,msSelect_list){
  
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
  assetId_vec <- SplitResource(resource_vec,'asset') #### parallel with resource, not unique
  
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
      #tempQuantity_vec <- availAsset_df$totalQuantity[which(availAsset_df$assetCustacId==tempResource)]
      availAsset_df$quantity[which(availAsset_df$assetCustacId==tempResource)]<- tempAvailQuantity_vec-selectAssetQuantity_vec[k]
      #availAsset_df$totalQuantity[which(availAsset_df$assetCustacId==tempResource)]<- tempQuantity_vec-selectAssetQuantity_vec[k]
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
    rownames(selectAsset_df)<- 1:length(selectAsset_df[,1])
    
    callSelect_list[[callId_vec[i]]] <- selectAsset_df
    if(is.null(msSelect_list[[msId_vec[j]]])){
      msSelect_list[[msId_vec[j]]] <- selectAsset_df
    } else{
      tempAsset_df <- msSelect_list[[msId_vec[j]]]
      selectAsset_df <- rbind(selectAsset_df,tempAsset_df)
      rownames(selectAsset_df)<- 1:length(selectAsset_df[,1])
      msSelect_list[[msId_vec[j]]] <- selectAsset_df
    }
  }
  
  result_list <- list(callSelect_list=callSelect_list,  msSelect_list=msSelect_list,
                      availAsset_df=availAsset_df)
  return(result_list)
}

ResultVec2Mat <- function(solution_vec,callId_vec,resource_vec,idxEli_vec,varNum){
  callNum <- length(callId_vec); resourceNum <- length(resource_vec)
  result_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
  result_mat <- t(result_mat)
  result_mat[idxEli_vec]<-solution_vec[1:varNum]
  result_mat <- t(result_mat)
  return(result_mat)
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

ResultList2Vec <- function(callOutput_list,callId_vec,minUnit_vec,varName_vec,varNum,idxEli_vec,fCon4_mat){
  varNum2 <- length(varName_vec)
  result_vec <- rep(0,varNum2)
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
      
      result_vec[idxVarTemp] <- quantityTemp/minUnitEli_vec[idxVarTemp]
    }
  }
  temp <- varNum2-varNum
  result1_mat <- matrix(rep(result_vec[1:varNum],temp),ncol=varNum,byrow=T)
  result2_mat <- result1_mat*fCon4_mat[1:temp,1:varNum]
  if(temp>1){
    temp_vec <- apply(result2_mat,1,sum)
  } else{
    temp_vec <- sum(result2_mat) # by row
  }
  
  result_vec[(varNum+1):varNum2] <- 1*(temp_vec & 1) # recalculate the dummy value
  
  return(result_vec)
}

ResultDf2List <- function(result_df,callId_vec){
  callNum <- length(callId_vec)
  result_list <- list()
  for(i in 1:callNum){
    callId <- callId_vec[i]
    idx_vec <- which(result_df$marginCall==callId)
    call_df <- result_df[idx_vec,]
    result_list[[callId]] <- call_df
  }
  return(result_list)
}
ResultList2AmountVec <- function(callOutput_list,callId_vec,varName_vec){
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
ResultList2Df <- function(result_list,callId_vec){
  result_df <- result_list[[callId_vec[1]]]
  if(length(callId_vec)>1){
    for(i in 2:length(callId_vec)){
      alloc_df <- result_list[[callId_vec[i]]]
      result_df <- rbind(result_df,alloc_df)
    }
  }
  rownames(result_df) <- 1:length(result_df[,1])
  return(result_df)
}
