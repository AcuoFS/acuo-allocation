UpdateQtyInAvailAsset <- function(resource_vec,quantity_vec,availAsset_df,qtyType,qtyLeft,minUnit_vec){
  ## quantity_vec: real quantity of corresponding resource
  if(qtyLeft){
    if(qtyType=='minUnit'){
      for(i in 1:length(resource_vec)){
        resource <- resource_vec[i]
        quantity <- quantity_vec[i]
        minUnit <- minUnit_vec[i]
        idx_vec <- which(availAsset_df$assetCustacId==resource)
        if(length(idx_vec)!=0){
          availAsset_df$quantity[idx_vec] <- quantity*minUnit
        }
      }
    } else{
      for(i in 1:length(resource_vec)){
        resource <- resource_vec[i]
        quantity <- quantity_vec[i]
        idx_vec <- which(availAsset_df$assetCustacId==resource)
        if(length(idx_vec)!=0){
          availAsset_df$quantity[idx_vec] <- quantity
        }
      }
    }
  } else{
    if(qtyType=='minUnit'){
      for(i in 1:length(resource_vec)){
        resource <- resource_vec[i]
        quantity <- quantity_vec[i]
        minUnit <- minUnit_vec[i]
        idx_vec <- which(availAsset_df$assetCustacId==resource)
        if(length(idx_vec)!=0){
          availAsset_df$quantity[idx_vec] <- availAsset_df$quantity[idx_vec]-quantity*minUnit
        }
      }
    } else{
      for(i in 1:length(resource_vec)){
        resource <- resource_vec[i]
        quantity <- quantity_vec[i]
        idx_vec <- which(availAsset_df$assetCustacId==resource)
        if(length(idx_vec)!=0){
          availAsset_df$quantity[idx_vec] <- availAsset_df$quantity[idx_vec]-quantity
        }
      }
    }
  }
  
  
  return(availAsset_df)
}

GetQtyFromAvailAsset <- function(resource_vec,availAsset_df,qtyType,minUnit_vec){ ## unit/minUnit quantity
  quantity_vec <- rep(0,length(resource_vec))
  if(qtyType=='minUnit'){
    for(i in 1:length(resource_vec)){
      resource <- resource_vec[i]
      minUnit <- minUnit_vec[i]
      idx_vec <- which(availAsset_df$assetCustacId==resource)
      quantity_vec[i] <- min(availAsset_df$quantity[idx_vec]/minUnit)
    }
  } else{
    for(i in 1:length(resource_vec)){
      resource <- resource_vec[i]
      idx_vec <- which(availAsset_df$assetCustacId==resource)
      quantity_vec[i] <- min(availAsset_df$quantity[idx_vec])
    }
  }
  return(quantity_vec)
}

CheckQtyInAvailAsset <- function(availAsset_df){
  resource_vec <- unique(availAsset_df$assetCustacId)
  for(i in 1:length(resource_vec)){
    resource <- resource_vec[i]
    idx_vec <- which(availAsset_df$assetCustacId==resource)
    minQty <- min(availAsset_df$quantity[idx_vec])
    if(!all(availAsset_df$quantity[idx_vec]==minQty)){
      errormsg <- paste('Quantities in availAsset_df are not consistent for asset',resource,'!')
      stop(errormsg)
    }
  }
  return(1)
}

UsedQtyFromResultList <- function(result_list,resource_vec,callId_vec){ ## quantity in result_list mostly are minUnitQuantity
  #### minUnitQuantity of resources used for allocation
  quantityUsed_vec <- rep(0,length(resource_vec))
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  for(i in 1:callNum){
    callId <- callId_vec[i]
    alloc_df <- result_list[[callId]]
    resourceTemp_vec <- PasteResource(alloc_df$Asset,alloc_df$CustodianAccount)
    idxInRes_vec <- na.omit(match(resourceTemp_vec,resource_vec))
    if(length(idxInRes_vec)!=0){
      idxInAlloc <- match(resource_vec[idxInRes_vec],resourceTemp_vec)
      quantityUsed_vec[idxInRes_vec] <- quantityUsed_vec[idxInRes_vec]+alloc_df$Quantity[idxInAlloc]
    }
  }
  return(quantityUsed_vec)
}
