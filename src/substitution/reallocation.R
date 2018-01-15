
Reallocation <- function(settledCollaterals,availAsset_df,callInfo_df,resource_df,pref_vec){
  
  callId_vec <- callInfo_df$id
  resource_vec <- resource_df$id
  
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  
  pref_vec <- pref_vec/sum(pref_vec[1:2])
  
  #### parameters ############
  
  
  aggregateInfo_list <- AggregateCallByAgreement(settledCollaterals,availAsset_df,callInfo_df)
  
  newSettledCollaterals <- aggregateInfo_list$newSettledCollaterals
  newAvailAsset_df <- aggregateInfo_list$newAvailAsset_df
  newCallInfo_df <- aggregateInfo_list$newCallInfo_df
  
  newCallId_vec <- newCallInfo_df$newId
  newCallNum <- length(newCallId_vec)
  
  availInfo_list <- AssetByCallInfo(newCallId_vec,resource_vec,newAvailAsset_df)
  
  callAmount_mat <- matrix(rep(newCallInfo_df$callAmount,resourceNum),nrow=newCallNum,byrow=F)
  unitValue_mat<- matrix(rep(resource_df$unitValue/resource_df$FXRate, newCallNum),nrow=newCallNum,byrow=T)
  minUnit_mat <- matrix(rep(resource_df$minUnit,newCallNum),nrow=newCallNum,byrow=T)
  minUnitValue_mat <- unitValue_mat*minUnit_mat
  haircutC_mat<-availInfo_list$haircutC_mat
  haircutFX_mat<-availInfo_list$haircutFX_mat
  haircut_mat<-availInfo_list$haircut_mat
  costBasis_mat <- availInfo_list$cost_mat 
  eli_mat <- availInfo_list$eli_mat
  quantity_mat <- matrix(rep(resource_df$qtyMin,callNum),nrow=callNum,byrow=T)
  
  
  objParams_list <- ConstructModelObj(callAmount_mat,minUnitValue_mat,haircut_mat,costBasis_mat,eli_mat,newCallInfo_df,
                                      newCallId_vec,resource_vec)
  
  # cost+liquidity matrix
  normCost_mat <- objParams_list$cost_mat
  normLiquidity_mat <- objParams_list$liquidity_mat 
  
  optimal_mat <- normCost_mat*pref_vec[1]+normLiquidity_mat*pref_vec[2]
  colnames(optimal_mat) <- resource_vec
  rownames(optimal_mat)<- newCallId_vec
  
  #### Substitute ######
  
  newSettledCollaterals$resource <- PasteResource(newSettledCollaterals$asset,newSettledCollaterals$custodianAccount)
  
  settledAmount_mat <- SettledAmountVec2Mat(newSettledCollaterals,resource_vec,newCallId_vec)
  newSettledAmount_mat <- settledAmount_mat
  
  settledQuantity_mat <- SettledQuantityVec2Mat(newSettledCollaterals,resource_vec,newCallId_vec)
  newSettledQuantity_mat <- settledQuantity_mat
  
  # stop signal?
  # 1. reach movements limit
  # 2. reach cost saving
  # 3. try all margin calls(currently using)
  
  for(i in 1:newCallNum){
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
      
      resultTemp <- ReplaceWithinCall(idxTemp, optimal_mat,i,idxResourceTemp,resource_vec,newSettledAmount_mat,newSettledQuantity_mat,resource_df,haircut_mat)
      newSettledAmount_mat <- resultTemp$newSettledAmount_mat
      newSettledQuantity_mat <- resultTemp$newSettledQuantity_mat
      resource_df <- resultTemp$resource_df
    } 
  }
  
  # initialize the callSelect_list and msSelect_list
  callSelect_list  <- list()    
  msSelect_list <- list()  
  newAllocation_list <- ResultMat2List(newSettledQuantity_mat,newCallId_vec,resource_vec,newCallInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df,
                                callSelect_list,msSelect_list)
  # allocation changes
  callChange_list <- list()
  msChange_list <- list()
  leftSettledQuantity_mat <- newSettledQuantity_mat-settledQuantity_mat
  if(newCallNum>1){
    idxChange_vec <- apply(leftSettledQuantity_mat&matrix(1,nrow=newCallNum,ncol=resourceNum),1,sum)
    idxTemp <- which(idxChange_vec!=0)
    leftSettledQuantity_mat <- leftSettledQuantity_mat[idxTemp,]
    leftSettledQuantity_mat <- matrix(leftSettledQuantity_mat,nrow=length(idxTemp))
    changeAllocation_list <- ResultMat2List(leftSettledQuantity_mat,callId_vec[idxTemp],resource_vec,newCallInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df,
                                            callChange_list,msChange_list)
  } else{
    if(length(which(leftSettledQuantity_mat!=0)) == 1){
      changeAllocation_list <- ResultMat2List(leftSettledQuantity_mat,callId_vec,resource_vec,newCallInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df,
                                              callChange_list,msChange_list)
    }
  }
  
  
  return(list(newAllocation_list=newAllocation_list,changeAllocation_list=changeAllocation_list))
}


#### OTHER FUNCTIONS ########
# function to replace all settled collaterals that are not optimal within a call
ReplaceWithinCall <- function(replaceIdx_vec, optimal_mat,callIdx,newIdx_vec,resource_vec,newSettledAmount_mat,newSettledQuantity_mat,resource_df,haircut_mat){
  
  for(idx in replaceIdx_vec){
    resourceSelect <- FindResourceToReplace(optimal_mat[callIdx,],newIdx_vec,idx,
                                            resource_df,resource_vec)
    # execute the subsitution in the matrix
    idxResourceSelect <- which(resource_vec==resourceSelect)
    resourceAmount <- newSettledAmount_mat[callIdx,idxResourceSelect]
    # post haircut amount
    resourceAmountCall <- resourceAmount*(1-haircut_mat[callIdx,idxResourceSelect])
    
    # check the sufficiency of the optimal resources
    optimalResourceAmount_vec <- resource_df$minUnitValue[newIdx_vec]*resource_df$qtyMin[match(resource_vec[newIdx_vec],resource_df$id)]
    # post haircut amount 
    optimalResourceAmountCall_vec <- optimalResourceAmount_vec*(1-haircut_mat[callIdx,newIdx_vec])
    idxTemp <- newIdx_vec[which(optimalResourceAmountCall_vec > resourceAmountCall)]
    if(length(idxTemp)==0){
      # insufficient
      # check other possibilities? the other resourceSelect
      # if no, then 1. skip OR 2. replace partial
      # do nothing for now
    } else {
      idxNew <- idxTemp[1] # choose the first one
      
      # replace
      # update the newSettledAmount_mat and resource_df
      qtyMinNew <- IntegralUnitQuantity(resourceAmountCall,haircut_mat[callIdx,idxNew],resource_df$minUnitValue[idxNew])
      
      amountNew <- IntegralUnitAmount(resourceAmountCall,haircut_mat[callIdx,idxNew],resource_df$minUnitValue[idxNew])
      
      # update newSettledAmount_mat
      newSettledAmount_mat[callIdx,idxNew] <- amountNew
      newSettledAmount_mat[callIdx,idxResourceSelect] <- 0
      
      # update newSettledQuantity_mat
      newSettledQuantity_mat[callIdx,idxNew] <- qtyMinNew
      newSettledQuantity_mat[callIdx,idxResourceSelect] <- 0
      
      # update resource_df
      # update $qtyMin 
      resource_df$qtyMin[idxNew] <- resource_df$qtyMin[idxNew] - qtyMinNew
      resource_df$qtyMin[idxResourceSelect] <- resource_df$qtyMin[idxResourceSelect] + newSettledQuantity_mat[callIdx,idxResourceSelect]
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
FindResourceToReplace <- function(optimal_vec,idxResourceTemp,idxAmountTemp,resource_df,resource_vec){
  
  # find the highest score from the existing resources
  # if more than one, then choose the one with largest amount settled to start
  idxSelect <- idxAmountTemp[which(optimal_vec[idxAmountTemp]==max(optimal_vec[idxAmountTemp]))]
  amountSelect <- resource_df$minUnitValue[idxSelect]*resource_df$qtyMin[match(resource_vec[idxSelect],resource_df$id)]
  if(length(idxSelect)==1){
    resourceSelect <- resource_vec[idxSelect]
  } else if(length(idxSelect)>1){
    resourceSelect <- resource_vec[idxSelect[which.max(amountSelect)]]
  }
  return(resourceSelect)
}

# function to construct settled collateral amount into an allocation matrix
SettledAmountVec2Mat <- function(settledCollaterals,resource_vec,callId_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  settledAmount_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
  settledAmount_mat[cbind(match(settledCollaterals$newCallId,callId_vec),match(settledCollaterals$resource,resource_vec))] <- settledCollaterals$amount
  return(settledAmount_mat)
}

# function to construct settled collateral quantity into an allocation matrix
SettledQuantityVec2Mat <- function(settledCollaterals,resource_vec,callId_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  settledQuantity_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
  settledQuantity_mat[cbind(match(settledCollaterals$newCallId,callId_vec),match(settledCollaterals$resource,resource_vec))] <- settledCollaterals$quantity
  return(settledQuantity_mat)
}

# function to aggregate margin calls by agreement
AggregateCallByAgreement <- function(settledCollaterals,availAsset_df,callInfo_df){
  # differentiate initial calls and variation calls 
  # because they have different eligibility and haircut rules
  
  ## newSettledCollaterals
  newSettledCollaterals <- aggregate(cbind(quantity,amount)~agreement+marginType+asset+custodianAccount,data=settledCollaterals,sum)
  newSettledCollaterals$newCallId <- PasteResource(newSettledCollaterals$agreement,newSettledCollaterals$marginType)
  
  ## newAvailAsset_df
  newAvailAsset_df <- availAsset_df
  # change callId_vec to (agreementId---marginType)
  agreements <- settledCollaterals$agreement[match(callId_vec,settledCollaterals$call)]
  marginTypes <- settledCollaterals$marginType[match(callId_vec,settledCollaterals$call)]
  newCallId_vec <- PasteResource(agreements,marginTypes)
  
  # add newId column to newAvailAsset_df
  oriCallId_vec <- newAvailAsset_df$callId
  newAvailAsset_df$callId <- newCallId_vec[match(newAvailAsset_df$callId,callId_vec)]
  newAvailAsset_df <- unique(newAvailAsset_df)
  
  idxKeep_vec <- which(!duplicated(newAvailAsset_df))
  newAvailAsset_df$oriCallId <- oriCallId_vec[idxKeep_vec]
  
  ## newCallInfo_df
  newCallInfo_df <- callInfo_df
  # add agreement id to newCallInfo_df
  newCallInfo_df$newId <- newCallId_vec
  # aggregate
  temp_df <- aggregate(callAmount~newId,newCallInfo_df,sum)
  # mock the other data
  idxKeep_vec <- which(!duplicated(newCallInfo_df$newId))
  newCallInfo_df <- newCallInfo_df[idxKeep_vec,]
  newCallInfo_df$callAmount <-temp_df$callAmount[match(newCallInfo_df$newId,temp_df$newId)]

  
  return(list(newSettledCollaterals=newSettledCollaterals,newAvailAsset_df=newAvailAsset_df,newCallInfo_df=newCallInfo_df))
}



