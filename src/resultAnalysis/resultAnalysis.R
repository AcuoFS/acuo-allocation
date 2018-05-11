
ResultAnalysis <- function(availAsset_df,resource_df,callInfo_df,callOutput_list){
  # Calculate dailyCost, monthlyCost, reservedLiquidityRatio, movement
  # 
  # Returns:
  #   A list of the four values
  
  callId_vec <- callInfo_df$id
  resource_vec <- resource_df$id
  
  #### Cost
  varName_vec <- PasteVarName(msId_vec = callInfo_df$marginStatement[match(availAsset_df$callId,callInfo_df$id)],
                              callId_vec = availAsset_df$callId,
                              resource_vec = availAsset_df$assetCustacId)
  varAmount_vec <- ResultList2AmountVec(callOutput_list,callId_vec,varName_vec)
  cost_vec <- CostDefinition(availAsset_df,resource_df)
  
  dailyCost <- CostFun(varAmount_vec,cost_vec,"daily")
  monthlyCost <- CostFun(varAmount_vec,cost_vec,"monthly")
  
  #### Movement
  movements <- OperationFun(callOutput_list,callInfo_df,'callList')
  
  #### Reserved Liquidity Ratio
  quantityTotal_vec <- resource_df$qtyMin
  quantityLeft_vec <- quantityTotal_vec - UsedQtyFromResultList(callOutput_list, resource_vec,callId_vec)
  liquidity_vec <- LiquidityDefinition(availAsset_df,resource_df)
  minUnitValue_vec <- resource_df$minUnitValue/resource_df$FXRate
  reservedLiquidityRatio <- LiquidFun(quantityLeft_vec,quantityTotal_vec,liquidity_vec,minUnitValue_vec)
  
  resultAnalysis <- list(dailyCost=dailyCost,monthlyCost=monthlyCost,movements=movements,reservedLiquidityRatio=reservedLiquidityRatio)
  return(resultAnalysis)
}

CostFun <- function(amount_vec,cost_vec,term){
  # calculate daily cost or monthly cost
  if(term=="daily"){
    cost <- sum(amount_vec*cost_vec)
  } else if(term=="monthly"){
    cost <- sum(amount_vec*cost_vec)*30
  }
  return(cost)
}

LiquidFun <- function(quantityLeft_vec,quantityTotal_vec,liquidity_vec,minUnitValue_vec){
  numerator <- sum(quantityLeft_vec*liquidity_vec*minUnitValue_vec)
  denominator <- sum(quantityTotal_vec*liquidity_vec*minUnitValue_vec)
  ratio <- numerator/denominator
  return(ratio)
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
    stop('ALERR3005: Invalid OperationFun input method')
  }
  
  return(movements)
}

CostDefinition <- function(availAsset_df,resource_df){
  # The cost of a resource for a call is defined as follow:
  #   cost = internal + external + opportunity - interest rate
  #
  # Note that for noncash securities, the opportunity cost is 0 if repo is positive
  #
  # Returns:
  #   A vector of cost per each row of availAsset_df
  
  ## Derive Noncash idx in availAsset_df
  assetId_vec <- SplitResource(availAsset_df$assetCustacId,"asset")
  noncashId_vec <-  resource_df$assetId[which(resource_df$assetId!=resource_df$currency)]
  noncashIdx_vec <- which(assetId_vec %in% noncashId_vec)
  
  ## Floor positive repo rates to 0 (for noncash assets)
  negIdx <- which(availAsset_df$opptCost[noncashIdx_vec] < 0)
  if(length(negIdx)){
    availAsset_df$opptCost[noncashIdx_vec[negIdx]] <- 0
  }
  
  ## Calculate Cost
  cost <- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-availAsset_df$interestRate
  
  return(cost)
}

LiquidityDefinition <- function(availAsset_df,resource_df){
  # The liquidity of a resource is defined as follow:
  #   liquidity = min((1-haircut_i)^2), where haircut_i denotes the haircut of the resource for call i.
  #
  # Returns:
  #   A vector of liquidity per each row of resource_df
  
  callId_vec <- unique(availAsset_df$callId)
  resource_vec <- unique(resource_df$id)
  
  ## Derive eli_mat and haircut_mat
  base_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),dimnames=list(callId_vec,resource_vec))
  eli_mat <- base_mat
  haircut_mat <- base_mat
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  eli_mat[cbind(idxCallId_vec,idxResource_vec)]<- 1
  haircut_mat[cbind(idxCallId_vec,idxResource_vec)] <- availAsset_df$haircut + availAsset_df$FXHaircut
  
  ## Calculate Resource liquidity
  if(is.null(dim(haircut_mat))){
    liquidity_vec <- (1-haircut_mat*eli_mat)^2
  } else{
    liquidity_vec <- apply((1-haircut_mat*eli_mat)^2,2,min)
  }
  
  return(liquidity_vec)
}
