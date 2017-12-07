# manual allocation functions

CombineTwoAllocation2One <- function(oldAllocation_df, newPortion_df){
  # one line in each df
  oldPortion_df <- oldAllocation_df
  
  combinePortion_df <- oldPortion_df
  combinePortion_df$Quantity <- oldPortion_df$Quantity + newPortion_df$Quantity
  combinePortion_df$Amount <- oldPortion_df$Amount + newPortion_df$Amount
  combinePortion_df$NetAmount <- oldPortion_df$NetAmount + newPortion_df$NetAmount
  combinePortion_df$`Amount(USD)` <- oldPortion_df$`Amount(USD)` + newPortion_df$`Amount(USD)`
  combinePortion_df$`NetAmount(USD)` <- oldPortion_df$`NetAmount(USD)` + newPortion_df$`NetAmount(USD)`
  combinePortion_df$Cost <- oldPortion_df$Cost + newPortion_df$Cost
  
  return(combinePortion_df)
}



OrderResource <- function(resource_df,availAsset_df,newResource,callInfo_df){
  # note that the cost and liquidity parameters are normalized, to be consistent we need to 
  # normalize with all the assets
  resource_vec <- resource_df$id
  number <- length(resource_vec)
  
  callAmount_mat <- matrix(rep(1e8,number),nrow=1) # need a large number 
  minUnitValue_mat <- matrix(resource_df$minUnitValue,nrow=1)
  haircut_mat <- matrix((availAsset_df$haircut+availAsset_df$FXHaircut),nrow=1)
  costBasis_mat <- matrix(CostDefinition(availAsset_df),nrow=1)
  eli_mat <- matrix(rep(1,number),nrow=1)
  
  objParams_list <- ConstructModelObj(callAmount_mat,minUnitValue_mat,haircut_mat,costBasis_mat,eli_mat,
                                      callInfo_df,callId,resource_vec)
  # new asset not in the order
  resourceScore_vec <- objParams_list$cost_vec*pref_vec[1]+objParams_list$liquidity_vec*pref_vec[2]
  orderedResource_vec <- resource_vec[order(resourceScore_vec,decreasing = F)]
  orderedResource_vec <- orderedResource_vec[-which(orderedResource_vec==newResource)]
  return(orderedResource_vec)
}

ConstructNewAllocDf <- function(newResource,newResourceAmount,resource_df,availAsset_df){
  
  idxResourceNew <- which(resource_df$id==newResource)
  
  newResource_df <- resource_df[idxResourceNew,]
  newResourceAvail_df <- availAsset_df[which(availAsset_df$assetCustacId==newResource),]
  
  newResourceMinUnitQuantity <- newResourceAmount/resource_df$minUnitValue[idxResourceNew]
  
  newAlloc_df <- ConstructAllocDf(newResource_df,callInfo_df,newResourceAvail_df$haircut,newResourceAvail_df$FXHaircut,newResourceMinUnitQuantity,CostDefinition(newResourceAvail_df))
  return(newAlloc_df)
}

ConstructReplaceRecord <- function(selectionForCall,replaceResource,newResource,replaceAmount,resource_df){
  
  idxSelectionReplace <- which(selectionForCall$Asset==SplitResource(replaceResource,"asset") && selectionForCall$CustodianAccount==SplitResource(replaceResource,"custodianAccount"))
  idxResourceReplace <- which(resource_df$id==replaceResource)
  
  replaceMinUnitQuantity <- floor(replaceAmount*resource_df$FXRate[idxResourceReplace]/(resource_df$minUnitValue[idxResourceReplace]*(1-selectionForCall$Haircut[idxSelectionReplace])))
  
  replaceRecord_df <- selectionForCall[idxSelectionReplace,]
  replaceRecord_df$Quantity <- replaceMinUnitQuantity*resource_df[idxResourceReplace,]$minUnit
  replaceRecord_df$Amount <- replaceRecord_df$Quantity*resource_df[idxResourceReplace,]$unitValue
  replaceRecord_df$`Amount(USD)` <-round(replaceRecord_df$Amount/resource_df$FXRate[idxResourceReplace],3) #rounding
  replaceRecord_df$NetAmount <- replaceRecord_df$Amount*(1-replaceRecord_df$Haircut)
  replaceRecord_df$`NetAmount(USD)` <- replaceRecord_df$`Amount(USD)`*(1-replaceRecord_df$Haircut)
  replaceRecord_df$Cost <- replaceRecord_df$`Amount(USD)`*replaceRecord_df$CostFactor
  
  return(replaceRecord_df)
}

UpdateSelectionByQty <- function(selectionForCall,replaceResource,reduceQuantity,resource_df){
  
  idx <- which(selectionForCall$Asset==SplitResource(replaceResource,"asset") && selectionForCall$CustodianAccount==SplitResource(replaceResource,"custodianAccount"))
  thisResource_df <- resource_df[which(resource_df$id==replaceResource),]
  
  selectionForCall[idx,]$Quantity <- selectionForCall[idx,]$Quantity- reduceQuantity
  selectionForCall[idx,]$Amount <- selectionForCall[idx,]$Quantity*thisResource_df$unitValue
  selectionForCall[idx,]$`Amount(USD)` <-selectionForCall[idx,]$Amount/thisResource_df$FXRate
  selectionForCall[idx,]$NetAmount <- selectionForCall[idx,]$Amount*(1-selectionForCall[idx,]$Haircut)
  selectionForCall[idx,]$`NetAmount(USD)` <- selectionForCall[idx,]$NetAmount/thisResource_df$FXRate
  selectionForCall[idx,]$Cost <- selectionForCall[idx,]$CostFactor*selectionForCall[idx,]$`Amount(USD)`
  return(selectionForCall)
}

