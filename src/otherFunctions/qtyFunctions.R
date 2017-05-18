UpdateQtyOriInResourceDf <- function(resource_df){
  ## will be called at the very end of the allocation
  quantity_vec <- resource_df$qtyMin*resource_df$minUnit + resource_df$qtyRes
  resource_df$qtyOri <- quantity_vec
  return(resource_df)
}

ResetQtyMinInResourceDf <- function(resource_df){
  resource_df$qtyMin <- floor(resource_df$qtyOri/resource_df$minUnit)
  return(resource_df)
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

UsedQtyFromResultList <- function(result_list,resource_vec,callId_vec){ ## quantity in result_list are unitQuantity
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


