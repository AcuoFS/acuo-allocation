
ManualSubstitution <- function(newResource,resource_df,availAsset_df,
                              oldResource, oldResourceAmount,amountSelect,subCollateral_df){
  oldIdx <- which(resource_df$id==oldResource)
  newIdx <- which(resource_df$id==newResource)
  
  # original asset adjusted amount
  oldResourceRequireAmount <- ceiling(oldResourceAmount/resource_df$minUnitValue[newIdx])*resource_df$minUnitValue[newIdx]
  
  oldResourceAdjAmountUSD <- oldResourceRequireAmount/resource_df$FXRate[newIdx]*(1-subCollateral_df$haircut)
  
  # calculate the amount required for new asset
  newResourceMinQuantity <- ceiling(oldResourceAdjAmountUSD*resource_df$FXRate[newIdx]/(1-availAsset_df$haircut[newIdx]-availAsset_df$FXHaircut[newIdx])/resource_df$minUnitValue[newIdx])
  newResourceRequireAmount <- newResourceMinQuantity * resource_df$minUnitValue[newIdx]
  
  # check whether the new asset is sufficient 
  newResourceTotalAmount <- resource_df$qtyOri[newIdx]*resource_df$unitValue[newIdx]
  if(newResourceRequireAmount > newResourceTotalAmount){
    warning("New asset is not sufficient for substituting!")
    return()
  } else{
    # allocation for this asset, create AssetTransfer
    newAssetTransfer_df <- ConstructNewAllocDf(newResource,newResourceRequireAmount,resource_df,availAsset_df)
    
    return(newAssetTransfer_df)
  }
}





