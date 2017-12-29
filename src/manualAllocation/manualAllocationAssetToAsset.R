
ManualAllocationAssetToAsset <- function(algoVersion, newResource,newResourceAmount,selectionForCall,
                                 replaceResource,resource_df,availAsset_df,
                                 operLimit,operLimitMc,fungible){
  # Initialize the outputs
  newSelectionForCall <- selectionForCall
  replaceRecord_df <- selectionForCall[-(1:length(selectionForCall$Asset)),]

  idxSelectionNew <- which(selectionForCall$Asset==SplitResource(newResource,"asset") && selectionForCall$CustodianAccount==SplitResource(newResource,"custodianAccount"))
  idxSelectionReplace <- which(selectionForCall$Asset== SplitResource(replaceResource,"asset") && selectionForCall$CustodianAccount==SplitResource(replaceResource,"custodianAccount"))
  
  scenario <- 0
  if(newResource==replaceResource){
    # Scenario 1
    scenario <- 1
    sumNewResourceAmount <- selectionForCall$`NetAmount(USD)`[idxSelectionReplace] + newResourceAmount
    newSelectionForCall <- ConstructNewAllocDf(newResource,sumNewResourceAmount,resource_df,availAsset_df)
  } else{
    if(newResourceAmount < selectionForCall$`NetAmount(USD)`[idxSelectionReplace]){
      # Scenario 2
      scenario <- 2
      newAlloc_df <- ConstructNewAllocDf(newResource,newResourceAmount,resource_df,availAsset_df)
      replaceRecord_df <- ConstructReplaceRecord(selectionForCall,replaceResource,newResource,newAlloc_df$`NetAmount(USD)`,resource_df)
      
      oldSelectionForCall <- UpdateSelectionByQty(selectionForCall,replaceResource,replaceRecord_df$Quantity,resource_df)
      if(length(idxSelectionNew)==1){ # already exists, need to combine with the old result
        oldSelectionForCall[idxSelectionNew,]
        reduceQuantity <- -newAlloc_df$Quantity # a negative number. 
        newSelectionForCall <- UpdateSelectionByQty(oldSelectionForCall,replaceResource,reduceQuantity,resource_df)
      
      } else{ # doesn't exist, add a new line
        newSelectionForCall <- rbind(oldSelectionForCall,newAlloc_df)
        if(length(newSelectionForCall$Asset)>operLimitMc){
          # Scenario 3
          scenario <- 3
          warning("Allocation generates movements more than limit")
        }
      }
    } else if(newResourceAmount == selectionForCall$`NetAmount(USD)`[idxSelectionReplace]){
      # Scenario 4
      scenario <- 4
      newAlloc_df <- ConstructNewAllocDf(newResource,newResourceAmount,resource_df,availAsset_df)
      replaceRecord_df <- ConstructReplaceRecord(selectionForCall,replaceResource,newResource,newAlloc_df$`NetAmount(USD)`,resource_df)
      oldSelectionForCall <- selectionForCall[-idxSelectionReplace,]
      newSelectionForCall <- rbind(oldSelectionForCall,newAlloc_df)
      
    } else{
      # Scenario 5
      scenario <- 5
      replaceRecord_df <- selectionForCall[idxSelectionReplace,]
      oldSelectionForCall <- selectionForCall[-idxSelectionReplace,]
      newAlloc_df <- ConstructNewAllocDf(newResource,newResourceAmount,resource_df,availAsset_df)
      newSelectionForCall <- rbind(oldSelectionForCall,newAlloc_df)
    }
      
  }
  
  return(list(scenario= scenario,replaceRecord_df=replaceRecord_df,newSelectionForCall=newSelectionForCall))
}

