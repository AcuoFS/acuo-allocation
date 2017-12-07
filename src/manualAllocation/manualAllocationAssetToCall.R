
ManualAllocationAssetToCall <- function(newResource,newResourceAmount,resource_df,availAsset_df,
                                        callInfo_df, selectionForCall,operLimitMc){
  # Initialize the outputs
  newSelectionForCall <- selectionForCall
  
  
  # check whether the selected asset exists in the current allocation
  existIdx <- which(PasteResource(selectionForCall$Asset, selectionForCall$CustodianAccount)==newResource)
  if(length(existIdx)==1){
    # the resource already exists in the current allocation
    # combine the new allocated with the existing
    
    # current portion of this asset in the selection
    oldPortion_df <- selectionForCall[existIdx,]
    
    # new added portion for this asset
    newPortion_df <- ConstructNewAllocDf(newResource,newResourceAmount,resource_df,availAsset_df)
    
    # combine with the old portion
    combinePortion_df <- CombineTwoAllocation2One(oldPortion_df,newPortion_df)
    
    # update the newSelectionForCall
    newSelectionForCall[existIdx,] <- combinePortion_df
    
  }else{
    # the resource does not exist in the current allocation
    # directly add another line in the selection of the new asset
    
    # allocation for this asset
    newPortion_df <- ConstructNewAllocDf(newResource,newResourceAmount,resource_df,availAsset_df)
    
    # add to the current selection
    newSelectionForCall <- rbind(newSelectionForCall,newPortion_df)
    
    if(length(selectionForCall$Asset)>=operLimitMc){
        warning("Allocation generates movements more than limit")
    }
  }
  return(newSelectionForCall)
}





