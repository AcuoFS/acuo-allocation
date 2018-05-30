
ConstructAllocDf <- function(resourceInfo_df,callInfo_df,haircutC_vec,haircutFX_vec,minUnitQuantity_vec,cost_vec ){
  assetId_vec <- resourceInfo_df$assetId
  assetCustodianAccount_vec <- resourceInfo_df$custodianAccount
  assetVenue_vec <- resourceInfo_df$venue
  assetName_vec <- resourceInfo_df$assetName
  assetHaircut_vec <- haircutC_vec+haircutFX_vec
  assetHaircutC_vec <- haircutC_vec
  ## add fx haircut
  assetHaircutFX_vec <- haircutFX_vec
  assetCostFactor_vec <- cost_vec
  assetCurrency_vec <- resourceInfo_df$currency
  assetMinUnitQuantity_vec <- minUnitQuantity_vec
  assetQuantity_vec <- minUnitQuantity_vec*resourceInfo_df$minUnit
  marginType_vec <- rep(callInfo_df$marginType,length(cost_vec))
  ms_vec <- rep(callInfo_df$marginStatement,length(cost_vec))
  call_vec <- rep(callInfo_df$id,length(cost_vec))
  
  assetFX_vec <- resourceInfo_df$FXRate
  assetOriFX_vec <- resourceInfo_df$oriFXRate
  assetUnitValue_vec <- resourceInfo_df$unitValue/assetFX_vec
  
  assetAmountUSD_vec <- assetQuantity_vec*assetUnitValue_vec
  assetNetAmountUSD_vec <- assetAmountUSD_vec*(1-assetHaircut_vec)
  
  assetCost_vec <- assetCostFactor_vec*assetAmountUSD_vec
  
  assetAmount_vec <- assetAmountUSD_vec*assetFX_vec
  assetNetAmount_vec <- assetNetAmountUSD_vec*assetFX_vec
  
  alloc_df <- data.frame(assetId_vec,assetName_vec,assetNetAmount_vec,assetNetAmountUSD_vec,assetOriFX_vec,assetFX_vec,assetHaircut_vec,assetHaircutC_vec,assetHaircutFX_vec,assetAmount_vec,assetAmountUSD_vec,
                         assetCurrency_vec,assetQuantity_vec,assetCustodianAccount_vec,assetVenue_vec,marginType_vec,ms_vec,call_vec,assetCostFactor_vec,assetCost_vec)
  colnames(alloc_df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','FXRatePerUSD','Haircut','Hc','Hfx','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType',
                         'marginStatement','marginCall','CostFactor','Cost')
  rownames(alloc_df)<- 1:length(alloc_df[,1])
  return(alloc_df)
}

ResultMat2List <- function(result_mat,callInfo_df,availAsset_df,resource_df){
  callOutput_list  <- list()    # store selected assets for each call, list by callId_vec
  msOutput_list <- list()   # store selected assets for each margin statement, list by msId
  
  haircutC_mat <- HaircutCVec2Mat(availAsset_df$haircut,availAsset_df,callInfo_df$id,resource_df$id)
  haircutFX_mat <- HaircutFXVec2Mat(availAsset_df$FXHaircut,availAsset_df,callInfo_df$id,resource_df$id)
  cost_mat <- CostVec2Mat(cost_vec = DefineCost(availAsset_df,resource_df),
                          availAsset_df,callInfo_df$id,resource_df$id)
  #### construct the result
  callId_vec <- callInfo_df$id
  for(i in 1:length(callId_vec)){      
    # resource and result_mat columns have the same order 
    # the allocated indexes: 
    idx_vec <- which(result_mat[i,]!=0) 
    if(length(idx_vec)==0){
      errormsg <- paste("ALERR3004: There's no asset allocated to margin call",callId_vec[i])
      stop(errormsg)
    }
    
    #### Construct the Allocation dataframe for This Call ####
    alloc_df <- ConstructAllocDf(resource_df[idx_vec,], callInfo_df[i,], haircutC_mat[i,idx_vec], haircutFX_mat[i,idx_vec],result_mat[i,idx_vec],cost_mat[i,idx_vec])
    
    #### Update callOutput_list ####
    callOutput_list[[callId_vec[i]]] <- alloc_df
    
    #### Update msOutput_list ######
    msId <- callInfo_df$marginStatement[i]
    msOutput_list[[msId]] <- rbind(msOutput_list[[msId]],alloc_df)
  }
  result_list <- list(callOutput_list=callOutput_list,msOutput_list=msOutput_list)
  return(result_list)
}

ResultMat2ListUpdate <- function(result_mat,callId_vec,resource_vec,callInfo_df, haircutC_mat,haircutFX_mat,cost_mat,resourceInfo_df,
                                 callSelect_list,msSelect_list){
  
  #### construct the result
  for(i in 1:length(callId_vec)){      
    # resource and result_mat columns have the same order 
    # the allocated indexes: 
    idx_vec <- which(result_mat[i,]!=0) 
    if(length(idx_vec)==0){
      errormsg <- paste("ALERR3004: There's no asset allocated to margin call",callId_vec[i])
      stop(errormsg)
    }
    
    alloc_df <- ConstructAllocDf(resourceInfo_df[idx_vec,], callInfo_df[i,], haircutC_mat[i,idx_vec], haircutFX_mat[i,idx_vec],result_mat[i,idx_vec],cost_mat[i,idx_vec])
    
    #### UPDATE THE ASSET QUANTITY START ########
    resourceInfo_df$quantity[idx_vec] <- resourceInfo_df$quantity[idx_vec]-alloc_df$Quantity #selectAssetQuantity_vec
    
    #### UPDATE THE ASSET QUANTITY END ##########
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

ResultVec2Mat <- function(solution_vec,callId_vec,resource_vec,varName_vec){
  # Convert the solution vector to matrix format with the row in callId_vec order 
  # and the column in resource_vec order
  #   Match call id and resource id in variable names to callId_vec and resource_vec
  # Returns:
  #   A matrix
  
  ## Extract callId and resourceId in variable names
  qtyVarNum <- GetQtyVarNum(varName_vec)
  callVar_vec <- SplitVarName(varName_vec[1:qtyVarNum],"call")
  resourceVar_vec <- SplitVarName(varName_vec[1:qtyVarNum],"resource")
  
  ## Initialize result_mat
  result_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),dimnames=list(callId_vec,resource_vec))
  
  ## Fill in result_mat by matching callId and resourceId 
  result_mat[cbind(match(callVar_vec,callId_vec),match(resourceVar_vec,resource_vec))] <- solution_vec[1:qtyVarNum]

  return(result_mat)
}

ResultList2Mat <- function(callOutput_list,callId_vec,resource_vec,minUnit_vec){
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
    minUnitQuantityTemp_vec <- quantityTemp_vec/minUnit_vec[idxTemp_vec]
    
    result_mat[m,idxTemp_vec] <- minUnitQuantityTemp_vec
  }
  return(result_mat)
}

ResultList2Vec <- function(callOutput_list,callId_vec,minUnit_vec,varName_vec){
  # Convert the allocation result list to a vector with values of decision variables
  # Remember to convert the quantity in result list to min unit quantity in the vector
  # 
  # Returns:
  #   A vector of decision variables' values
  
  result_vec <- rep(0,length(varName_vec))
  
  ## Iterate calls, extract Quantity and fill in result_vec
  for(m in 1:length(callId_vec)){
    callAlloc_df <- callOutput_list[[callId_vec[m]]] # allocation df for this call
    
    varNamePaste_vec <- PasteVarName(msId_vec = callAlloc_df$marginStatement,
                                     callId_vec = callAlloc_df$marginCall,
                                     resource_vec = PasteResource(callAlloc_df$Asset,callAlloc_df$CustodianAccount))
    idxVar_vec <- match(varNamePaste_vec,varName_vec)
    result_vec[idxVar_vec] <- callAlloc_df$Quantity/minUnit_vec[idxVar_vec]
  }
  
  ## Derive(Update) the dummy decision variables
  result_vec <- UpdateDummyVariable(result_vec,varName_vec)
  
  return(result_vec)
}

ResultList2DummyVec <- function(callOutput_list,callId_vec,varName_vec,qtyVarNum){
  totalVarNum <- length(varName_vec)
  result_vec <- rep(0,totalVarNum)
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
  temp <- totalVarNum-qtyVarNum
  result1_mat <- matrix(rep(result_vec[1:qtyVarNum],temp),ncol=qtyVarNum,byrow=T)
  result2_mat <- result1_mat*fCon4_mat[1:temp,1:qtyVarNum]
  
  if(temp>1){
    temp_vec <- apply(result2_mat,1,sum)
  } else{
    temp_vec <- sum(result2_mat) # by row
  }
  
  result_vec[(qtyVarNum+1):totalVarNum] <- 1*(temp_vec & 1) # recalculate the dummy value
  
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
  
  var_vec <- rep(0,length(varName_vec))
  
  for(i in 1:length(callId_vec)){
    currentAlloc_df <- callOutput_list[[callId_vec[i]]]
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
