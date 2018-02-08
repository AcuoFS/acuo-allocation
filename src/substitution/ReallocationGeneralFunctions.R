#### OTHER FUNCTIONS ########
# function to replace all settled collaterals that are not optimal within a agreement-callType
ReplaceWithinAgreementAndCallType <- function(optimal_mat,callIdx,resource_vec,newSettledAmount_mat,newSettledQuantity_mat,resource_df,haircut_mat,operLimitAg){
  
  replacedNum <- 0
  
  # indexes of assets with the smallest score
  newIdx_vec <- which(optimal_mat[callIdx,]==min(optimal_mat[callIdx,]))
  resourceTemp <- resource_vec[newIdx_vec]
  idxAmountTemp <- which(newSettledAmount_mat[callIdx,]>0)
  
  # different scenarios
  # Scenario 1, all settled collaterals are optimal resources
  # then no reallocation is needed
  
  # Scenario 2, some or none of settled collaterals are optimal resources
  # check for the settled collaterals that are not optimal
  
  idxIntersect <- intersect(resource_vec[idxAmountTemp],resourceTemp)
  
  if(length(idxIntersect) < length(idxAmountTemp)){ # scenario 2 
    # find the ones that are not optimal 
    replaceIdx_vec <- idxAmountTemp[which(is.na(match(resource_vec[idxAmountTemp],resourceTemp)))]
    
    # replace with the agreement with the movement constraint
    # need to choose the most beneficial replacements within limit
    # only choose the first operLimitAg to replace
    if(length(replaceIdx_vec)>operLimitAg){
      replaceIdx_vec <- replaceIdx_vec[1:operLimitAg]
    }
    
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
        replacedNum <- replacedNum+1
      }
    }
  }
  return(list(newSettledAmount_mat=newSettledAmount_mat,newSettledQuantity_mat=newSettledQuantity_mat,resource_df=resource_df,replacedNum=replacedNum))
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
AggregateCallByAgreementAndCallType <- function(settledCollaterals,availAsset_df,callInfo_df){
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
  agreementCallType_vec <- PasteResource(agreements,marginTypes)
  
  # add newId column to newAvailAsset_df
  oriCallId_vec <- newAvailAsset_df$callId
  newAvailAsset_df$callId <- agreementCallType_vec[match(newAvailAsset_df$callId,callId_vec)]
  newAvailAsset_df <- unique(newAvailAsset_df)
  
  idxKeep_vec <- which(!duplicated(newAvailAsset_df))
  newAvailAsset_df$oriCallId <- oriCallId_vec[idxKeep_vec]
  
  ## newCallInfo_df
  newCallInfo_df <- callInfo_df
  # add agreement id to newCallInfo_df
  newCallInfo_df$newId <- agreementCallType_vec
  newCallInfo_df$agreement <- agreements
  # aggregate
  temp_df <- aggregate(callAmount~newId,newCallInfo_df,sum)
  # mock the other data
  idxKeep_vec <- which(!duplicated(newCallInfo_df$newId))
  newCallInfo_df <- newCallInfo_df[idxKeep_vec,]
  newCallInfo_df$callAmount <-temp_df$callAmount[match(newCallInfo_df$newId,temp_df$newId)]
  
  
  return(list(newSettledCollaterals=newSettledCollaterals,newAvailAsset_df=newAvailAsset_df,newCallInfo_df=newCallInfo_df))
}


# function to group agreement-callType by agreement
GroupNewIdByAgreement <- function(agreementCallTypeLimit,agreementLimit,newCallInfo_df,agreementCallType_vec){
  
  groupNewCallId_list <- list()
  # if the total call numbers is equal or less than limitTotal, only one group
  if(length(newCallInfo_df[,1])<=agreementCallTypeLimit){
    groupNewCallId_list[[1]] <- agreementCallType_vec
  } else if(length(unique(newCallInfo_df$agreement))<=agreementLimit){
    groupNewCallId_list[[1]] <- agreementCallType_vec
  } else{
    groupAgreement_list <- list()
    agreement_vec <- unique(newCallInfo_df$agreement)
    agreementGroupNum <- ceiling(length(agreement_vec)/agreementLimit)
    
    for(i in 1:(agreementGroupNum-1)){
      tempCurrent <- agreementLimit*(i-1)
      tempAgreement_vec <- agreement_vec[(tempCurrent+1):(tempCurrent+agreementLimit)]
      tempNewCall_vec <- newCallInfo_df$newId[which((newCallInfo_df$agreement) %in% tempAgreement_vec)]
      groupAgreement_list[[i]]<- tempAgreement_vec
      groupNewCallId_list[[i]]<- tempNewCall_vec
    }
    tempCurrent <- agreementLimit*(agreementGroupNum-1)
    tempAgreement_vec <- na.omit(agreement_vec[(tempCurrent+1):(tempCurrent+agreementLimit)])
    tempNewCall_vec <- newCallInfo_df$newId[which((newCallInfo_df$agreement) %in% tempAgreement_vec)]
    groupAgreement_list[[agreementGroupNum]]<- tempAgreement_vec
    groupNewCallId_list[[agreementGroupNum]]<- tempNewCall_vec
  }
  return(groupNewCallId_list)
}


ResultMat2ListAgreement <- function(result_mat,newCallId_vec,resource_vec,callInfo_df, haircutC_mat,haircutFX_mat,cost_mat,resourceInfo_df,
                                    newCallSelect_list,agreementSelect_list){
  
  newCallNum <- length(newCallId_vec)
  
  #### construct the result
  for(i in 1:newCallNum){      
    # resource and result_mat columns have the same order 
    # the allocated indexes: 
    idx_vec <- which(result_mat[i,]!=0) 
    if(length(idx_vec)==0){
      errormsg <- paste("ALEER3004: There's no asset allocated to margin call",newCallId_vec[i])
      stop(errormsg)
    }
    
    alloc_df <- ConstructAllocDfAgreement(resourceInfo_df[idx_vec,], callInfo_df[i,], haircutC_mat[i,idx_vec], haircutFX_mat[i,idx_vec],result_mat[i,idx_vec],cost_mat[i,idx_vec])
    
    #### UPDATE THE ASSET QUANTITY START ########
    resourceInfo_df$quantity[idx_vec] <- resourceInfo_df$quantity[idx_vec]-alloc_df$Quantity #selectAssetQuantity_vec
    
    #### UPDATE THE ASSET QUANTITY END ##########
    #### Update newCallSelect_list Start ####
    newCallSelect_list[[newCallId_vec[i]]] <- alloc_df
    #### Update newCallSelect_list END ######
    
    #### Update agreementSelect_list Start ######
    agreement <- callInfo_df$marginStatement[i]
    if(is.null(agreementSelect_list[[agreement]])){
      agreementSelect_list[[agreement]] <- alloc_df
    } else{
      tempAlloc_df <- agreementSelect_list[[agreement]]
      alloc_df <- rbind(alloc_df,tempAlloc_df)
      rownames(alloc_df)<- 1:length(alloc_df[,1])
      agreementSelect_list[[agreement]] <- alloc_df
    }
    #### Update agreementSelect_list END ########
  }
  result_list <- list(newCallSelect_list=newCallSelect_list,agreementSelect_list=agreementSelect_list)
  return(result_list)
}

#### convertFunctions #### 
ConstructAllocDfAgreement <- function(resourceInfo_df,newCallInfo_df,haircutC_vec,haircutFX_vec,minUnitQuantity_vec,cost_vec ){
  
  oriCallId_vec <- newCallInfo_df$id
  newCallInfo_df$id <- newCallInfo_df$newId
  
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
  marginType_vec <- rep(newCallInfo_df$marginType,length(cost_vec))
  agreement_vec <- rep(newCallInfo_df$agreement,length(cost_vec))
  newCall_vec <- rep(newCallInfo_df$id,length(cost_vec))
  
  assetFX_vec <- resourceInfo_df$FXRate
  #oriAssetFX_vec <- resourceInfo_df$oriFXRate
  assetUnitValue_vec <- resourceInfo_df$unitValue/assetFX_vec
  
  assetAmountUSD_vec <- assetQuantity_vec*assetUnitValue_vec
  assetNetAmountUSD_vec <- assetAmountUSD_vec*(1-assetHaircut_vec)
  
  assetCost_vec <- assetCostFactor_vec*assetAmountUSD_vec
  
  assetAmount_vec <- assetAmountUSD_vec*assetFX_vec
  assetNetAmount_vec <- assetNetAmountUSD_vec*assetFX_vec
  
  alloc_df <- data.frame(assetId_vec,assetName_vec,assetNetAmount_vec,assetNetAmountUSD_vec,assetFX_vec,assetHaircut_vec,assetHaircutC_vec,assetHaircutFX_vec,assetAmount_vec,assetAmountUSD_vec,
                         assetCurrency_vec,assetQuantity_vec,assetCustodianAccount_vec,assetVenue_vec,marginType_vec,agreement_vec,newCall_vec,assetCostFactor_vec,assetCost_vec)
  colnames(alloc_df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Hc','Hfx','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType',
                         'agreement','agreementCallType','CostFactor','Cost')
  rownames(alloc_df)<- 1:length(alloc_df[,1])
  return(alloc_df)
}

