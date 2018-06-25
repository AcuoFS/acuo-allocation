
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

RemoveResourceNotInAvailAsset <- function(availAsset_df,resource_df){
  resource_df <- resource_df[which(resource_df$id %in% availAsset_df$resource),]
  return(resource_df)
}
