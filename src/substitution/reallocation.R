
Reallocation <- function(settledCollaterals,availAsset_df,callInfo_df,resource_df,pref_vec){
  
  callId_vec <- callInfo_df$id
  resource_vec <- resource_df$id
  
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  
  pref_vec <- pref_vec/sum(pref_vec[1:2])
  
  #### parameters ############
  availInfo_list <- AssetByCallInfo(callId_vec,resource_vec,availAsset_df)
  
  callAmount_mat <- matrix(rep(callInfo_df$callAmount,resourceNum),nrow=callNum,byrow=F)
  unitValue_mat<- matrix(rep(resource_df$unitValue/resource_df$FXRate, callNum),nrow=callNum,byrow=T)
  minUnit_mat <- matrix(rep(resource_df$minUnit,callNum),nrow=callNum,byrow=T)
  minUnitValue_mat <- unitValue_mat*minUnit_mat
  haircutC_mat<-availInfo_list$haircutC_mat
  haircutFX_mat<-availInfo_list$haircutFX_mat
  haircut_mat<-availInfo_list$haircut_mat
  costBasis_mat <- availInfo_list$cost_mat 
  eli_mat <- availInfo_list$eli_mat
  quantity_mat <- matrix(rep(resource_df$qtyMin,callNum),nrow=callNum,byrow=T)
  
  
  objParams_list <- ConstructModelObj(callAmount_mat,minUnitValue_mat,haircut_mat,costBasis_mat,eli_mat,callInfo_df,
                                      callId_vec,resource_vec)
  
  # cost+liquidity matrix
  normCost_mat <- objParams_list$cost_mat
  normLiquidity_mat <- objParams_list$liquidity_mat 
  
  optimal_mat <- normCost_mat*pref_vec[1]+normLiquidity_mat*pref_vec[2]
  colnames(optimal_mat) <- resource_vec
  rownames(optimal_mat)<-callId_vec
  
  #### Substitute ######
  
  settledCollaterals$resource <- PasteResource(settledCollaterals$asset,settledCollaterals$custodianAccount)
  
  settledAmount_mat <- SettledAmountVec2Mat(settledCollaterals,resource_vec)
  newSettledAmount_mat <- settledAmount_mat
  
  settledQuantity_mat <- SettledQuantityVec2Mat(settledCollaterals,resource_vec)
  newSettledQuantity_mat <- settledQuantity_mat
  
  # stop signal?
  # 1. reach movements limit
  # 2. reach cost saving
  # 3. try all margin calls(currently using)
  
  for(i in 1:callNum){
    # indexes of assets with the smallest score
    idxResourceTemp <- which(optimal_mat[i,]==min(optimal_mat[i,]))
    resourceTemp <- resource_vec[idxResourceTemp]
    idxAmountTemp <- which(newSettledAmount_mat[i,]>0)
    
    # different scenarios
    # Scenario 1, all settled collaterals are optimal resources
    # then no reallocation is needed
    
    # Scenario 2, some or none of settled collaterals are optimal resources
    # check for the settled collaterals that are not optimal
    
    idxIntersect <- intersect(resource_vec[idxAmountTemp],resourceTemp)
    
    if(length(idxIntersect) < length(idxAmountTemp)){ # scenario 2 
      # find the ones that are not optimal 
      idxTemp <- idxAmountTemp[which(is.na(match(resource_vec[idxAmountTemp],resourceTemp)))]
      
      resultTemp <- ReplaceWithinCall(idxTemp, idxResourceTemp,resource_vec,newSettledAmount_mat,resource_df,haircut_mat)
      newSettledAmount_mat <- resultTemp$newSettledAmount_mat
      newSettledQuantity_mat <- resultTemp$newSettledQuantity_mat
      resource_df <- resultTemp$resource_df
    } 
  }
  
  # initialize the callSelect_list and msSelect_list
  callSelect_list  <- list()    
  msSelect_list <- list()  
  newAllocation_list <- ResultMat2List(newSettledQuantity_mat,callId_vec,resource_vec,callInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df,
                                callSelect_list,msSelect_list)
  # allocation changes
  callChange_list <- list()
  msChange_list <- list()
  leftSettledQuantity_mat <- newSettledQuantity_mat-settledQuantity_mat
  if(callNum>1){
    idxChange_vec <- apply(leftSettledQuantity_mat&matrix(1,nrow=callNum,ncol=resourceNum),1,sum)
    idxTemp <- which(idxChange_vec!=0)
    leftSettledQuantity_mat <- leftSettledQuantity_mat[idxTemp,]
    leftSettledQuantity_mat <- matrix(leftSettledQuantity_mat,nrow=length(idxTemp))
    changeAllocation_list <- ResultMat2List(leftSettledQuantity_mat,callId_vec[idxTemp],resource_vec,callInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df,
                                            callChange_list,msChange_list)
  } else{
    if(length(which(leftSettledQuantity_mat!=0)) == 1){
      changeAllocation_list <- ResultMat2List(leftSettledQuantity_mat,callId_vec,resource_vec,callInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df,
                                              callChange_list,msChange_list)
    }
  }
  
  
  return(list(newAllocation_list=newAllocation_list,changeAllocation_list=changeAllocation_list))
}


#### OTHER FUNCTIONS ########
# function to replace all settled collaterals that are not optimal within a call
ReplaceWithinCall <- function(replaceIdx_vec, newIdx_vec,resource_vec,newSettledAmount_mat,resource_df,haircut_mat){
  
  for(idx in replaceIdx_vec){
    resourceSelect <- FindResourceToReplace(optimal_mat,newIdx_vec,idx,
                                            resource_df,resource_vec)
    # execute the subsitution in the matrix
    idxResourceSelect <- which(resource_vec==resourceSelect)
    resourceAmount <- newSettledAmount_mat[i,idxResourceSelect]
    # post haircut amount
    resourceAmountCall <- resourceAmount*(1-haircut_mat[i,idxResourceSelect])
    
    # check the sufficiency of the optimal resources
    optimalResourceAmount_vec <- resource_df$minUnitValue[newIdx_vec]*resource_df$qtyMin[match(resource_vec[newIdx_vec],resource_df$id)]
    # post haircut amount 
    optimalResourceAmountCall_vec <- optimalResourceAmount_vec*(1-haircut_mat[i,newIdx_vec])
    idxTemp <- which(optimalResourceAmountCall_vec > resourceAmountCall)
    if(length(idxTemp)==0){
      # insufficient
      # check other possibilities? the other resourceSelect
      # if no, then 1. skip OR 2. replace partial
      # do nothing for now
    } else {
      idxNew <- idxTemp[1] # choose the first one
      
      # replace
      # update the newSettledAmount_mat and resource_df
      qtyMinNew <- IntegralUnitQuantity(resourceAmountCall,haircut_mat[i,idxNew],resource_df$minUnitValue[idxNew])
      
      amountNew <- IntegralUnitAmount(resourceAmountCall,haircut_mat[i,idxNew],resource_df$minUnitValue[idxNew])
      
      # update newSettledAmount_mat
      newSettledAmount_mat[i,idxNew] <- amountNew
      newSettledAmount_mat[i,idxResourceSelect] <- 0
      
      # update newSettledQuantity_mat
      newSettledQuantity_mat[i,idxNew] <- qtyMinNew
      newSettledQuantity_mat[i,idxResourceSelect] <- 0
      
      # update resource_df
      # update $qtyMin 
      resource_df$qtyMin[idxNew] <- resource_df$qtyMin[idxNew] - qtyMinNew
      resource_df$qtyMin[idxResourceSelect] <- resource_df$qtyMin[idxResourceSelect] + newSettledQuantity_mat[i,idxResourceSelect]
    }
  }
  
  return(list(newSettledAmount_mat=newSettledAmount_mat,newSettledQuantity_mat=newSettledQuantity_mat,resource_df=resource_df))
}

# function to calculate integral minumal units quantity
IntegralUnitQuantity <- function(amount,haircut,minUnitValue){
  integralUnitQuantity <- ceiling(amount/(1-haircut)/minUnitValue)
  return(integralUnitQuantity)
}

# function to calculate integral units amount
IntegralUnitAmount <- function(amount,haircut,minUnitValue){
  integralUnitAmount <- (1-haircut)*minUnitValue*ceiling(amount/(1-haircut)/minUnitValue)
  return(integralUnitAmount)
}

# function to select the asset to be substitute
FindResourceToReplace <- function(optimal_mat,idxResourceTemp,idxAmountTemp,resource_df,resource_vec){
  
  # find the highest score from the existing resources
  # if more than one, then choose the one with largest amount settled to start
  idxSelect <- which(optimal_mat[i,]==max(optimal_mat[i,idxAmountTemp]))
  amountSelect <- resource_df$minUnitValue[idxSelect]*resource_df$qtyMin[match(resource_vec[idxSelect],resource_df$id)]
  if(length(idxSelect)==1){
    resourceSelect <- resource_vec[idxSelect]
  } else if(length(idxSelect)>1){
    resourceSelect <- resource_vec[idxSelect[which.max(amountSelect)]]
  }
  return(resourceSelect)
}

# function to construct settled collateral amount into an allocation matrix
SettledAmountVec2Mat <- function(settledCollaterals,resource_vec){
  settledAmount_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
  settledAmount_mat[cbind(match(settledCollaterals$call,callId_vec),match(settledCollaterals$resource,resource_vec))] <- settledCollaterals$amount
  return(settledAmount_mat)
}

# function to construct settled collateral quantity into an allocation matrix
SettledQuantityVec2Mat <- function(settledCollaterals,resource_vec){
  settledQuantity_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
  settledQuantity_mat[cbind(match(settledCollaterals$call,callId_vec),match(settledCollaterals$resource,resource_vec))] <- settledCollaterals$quantity
  return(settledQuantity_mat)
}

# function to aggregate margin calls by agreement
AggregateCallByAgreement <- function(){
  
}



