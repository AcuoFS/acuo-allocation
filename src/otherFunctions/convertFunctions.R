ResultMat2List <- function(result_mat,callId_vec,resource_vec,callInfo_df,haircut_mat,cost_mat,resourceInfo_df,
                           callSelect_list,msSelect_list){
  
  callNum <- length(callId_vec)
  
  #### construct the result
  for(i in 1:callNum){      
    # resource and result_mat columns have the same order 
    # the allocated indexes: 
    idx_vec <- which(result_mat[i,]!=0) 
    
    #### Get the information of the allocation Start ####
    selectResource_vec <- resource_vec[idx_vec]
    selectAssetId_vec <- resourceInfo_df$assetId[idx_vec]
    selectAssetCustodianAccount_vec <- resourceInfo_df$custodianAccount[idx_vec]
    selectAssetVenue_vec <- resourceInfo_df$venue[idx_vec]
    selectAssetName_vec <- resourceInfo_df$assetName[idx_vec]
    selectAssetHaircut_vec <- haircut_mat[i,idx_vec]
    selectAssetCostFactor_vec <- cost_mat[i,idx_vec]
    selectAssetCurrency_vec <- resourceInfo_df$currency[idx_vec]
    selectAssetMinUnitQuantity_vec <- result_mat[i,idx_vec]
    selectAssetQuantity_vec <- result_mat[i,idx_vec]*resourceInfo_df$minUnit[idx_vec]
    selectMarginType_vec <- rep(callInfo_df$marginType[i],length(idx_vec))
    selectMs_vec <- rep(callInfo_df$marginStatement[i],length(idx_vec))
    selectCall_vec <- rep(callId_vec[i],length(idx_vec))
    
    selectAssetFX_vec <- resourceInfo_df$FXRate[idx_vec]
    selectAssetUnitValue_vec <- resourceInfo_df$unitValue[idx_vec]/selectAssetFX_vec
    
    selectAssetAmountUSD_vec <- round(selectAssetQuantity_vec*selectAssetUnitValue_vec,2)
    selectAssetNetAmountUSD_vec <- selectAssetAmountUSD_vec*(1-haircut_mat[i,idx_vec])
    
    selectAssetCost_vec <- selectAssetCostFactor_vec*selectAssetAmountUSD_vec
    
    selectAssetAmount_vec <- selectAssetAmountUSD_vec*selectAssetFX_vec
    selectAssetNetAmount_vec <- selectAssetNetAmountUSD_vec*selectAssetFX_vec
    #### Get the information of the allocation END ######
    
    #### UPDATE THE ASSET QUANTITY START ########
    resourceInfo_df$quantity[idx_vec] <- resourceInfo_df$quantity[idx_vec]-selectAssetQuantity_vec
    
    #### UPDATE THE ASSET QUANTITY END ##########
    
    #### Construct alloc_df Start #############
    alloc_df <- data.frame(selectAssetId_vec,selectAssetName_vec,selectAssetNetAmount_vec,selectAssetNetAmountUSD_vec,selectAssetFX_vec,selectAssetHaircut_vec,selectAssetAmount_vec,selectAssetAmountUSD_vec,selectAssetCurrency_vec,
                           selectAssetQuantity_vec,selectAssetCustodianAccount_vec,selectAssetVenue_vec,selectMarginType_vec,selectMs_vec,selectCall_vec,selectAssetCostFactor_vec,selectAssetCost_vec)
    colnames(alloc_df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType','marginStatement','marginCall',
                           'CostFactor','Cost')
    rownames(alloc_df)<- 1:length(alloc_df[,1])
    #### Construct alloc_df END ###############
    
    #### Update callSelect_list Start ####
    callSelect_list[[callId_vec[i]]] <- alloc_df
    #### Update callSelect_list END ######
    
    #### Update msSelect_list Start ######
    msId <- callInfo_df$marginStatement[i]
    if(is.null(msSelect_list[[msId]])){
      msSelect_list[[msId]] <- alloc_df
    } else{
      tempAlloc_df <- msSelect_list[[msId]]
      alloc_df <- rbind(alloc_df,tempAlloc_df)
      rownames(alloc_df)<- 1:length(alloc_df[,1])
      msSelect_list[[msId]] <- alloc_df
    }
    #### Update msSelect_list END ########
  }
  result_list <- list(callSelect_list=callSelect_list,msSelect_list=msSelect_list)
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

ResultList2Vec <- function(callOutput_list,callId_vec,minUnit_vec,varName_vec,varNum,pos_vec){
  varNum2 <- length(varName_vec)
  result1_vec <- rep(0,varNum)
  callNum <- length(callId_vec)
  
  for(m in 1:callNum){
    callId <- callId_vec[m]
    callAlloc_df <- callOutput_list[[callId]]
    
    # find the corresponding decision variable index from the varName
    resourceTemp_vec <- PasteResource(callAlloc_df$Asset,callAlloc_df$CustodianAccount)
    varNameTemp_vec <- PasteVarName(callAlloc_df$marginStatement,callAlloc_df$marginCall,resourceTemp_vec)
    
    for(k in 1:length(resourceTemp_vec)){
      idxVarTemp <- which(varName_vec==varNameTemp_vec[k])
      quantityTemp <- callAlloc_df$Quantity[k]
      # the 'Quantity'= decision variable * minUnit
      result1_vec[idxVarTemp] <- quantityTemp/minUnit_vec[idxVarTemp]
    }
  }
  # derive the decision variables (varNum+1 ~ varNum2)
  var1_df <- data.frame(real=result1_vec,pos=pos_vec)
  var2_df <- aggregate(real~pos,data=var1_df,sum)
  result2_vec <- ((var2_df$real) & 1)*1
  
  result_vec <- c(result1_vec,result2_vec)
  
  return(result_vec)
}

ResultList2DummyVec <- function(callOutput_list,callId_vec,varName_vec,varNum){
  varNum2 <- length(varName_vec)
  result_vec <- rep(0,varNum2)
  callNum <- length(callId_vec)
  
  for(m in 1:callNum){
    callId <- callId_vec[m]
    callAlloc_df <- callOutput_list[[callId]]
    
    # find the corresponding decision variable index from the varName
    resourceTemp_vec <- PasteResource(callAlloc_df$Asset,callAlloc_df$CustodianAccount)
    varNameTemp_vec <- PasteVarName(callAlloc_df$marginStatement,callAlloc_df$marginCall,resourceTemp_vec)
    
    for(k in 1:length(resourceTemp_vec)){
      idxVarTemp <- which(varName_vec==varNameTemp_vec[k])
      result_vec[idxVarTemp] <- 1
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
