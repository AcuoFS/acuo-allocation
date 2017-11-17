
ManualAllocationAssetToAsset <- function(algoVersion,selectCallId, newResource,newResourceAmount,selectionForCall,
                                 replaceResource,resource_df,callInfo_df,availAsset_df,
                                 pref_vec,operLimit,operLimitMc,fungible){
  pref_vec/sum(pref_vec[1:2])
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
    newSelectionForCall <- ConstructNewAllocDf(selectionForCall,newResource,sumNewResourceAmount,resource_df,availAsset_df)
  } else{
    if(newResourceAmount < selectionForCall$`NetAmount(USD)`[idxSelectionReplace]){
      # Scenario 2
      scenario <- 2
      newAlloc_df <- ConstructNewAllocDf(selectionForCall,newResource,newResourceAmount,resource_df,availAsset_df)
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
      newAlloc_df <- ConstructNewAllocDf(selectionForCall,newResource,newResourceAmount,resource_df,availAsset_df)
      replaceRecord_df <- ConstructReplaceRecord(selectionForCall,replaceResource,newResource,newAlloc_df$`NetAmount(USD)`,resource_df)
      oldSelectionForCall <- selectionForCall[-idxSelectionReplace,]
      newSelectionForCall <- rbind(oldSelectionForCall,newAlloc_df)
      
    } else{
      # Scenario 5
      scenario <- 5
      replaceRecord_df <- selectionForCall[idxSelectionReplace,]
      oldSelectionForCall <- selectionForCall[-idxSelectionReplace,]
      newAlloc_df <- ConstructNewAllocDf(selectionForCall,newResource,newResourceAmount,resource_df,availAsset_df)
      newSelectionForCall <- rbind(oldSelectionForCall,newAlloc_df)
    }
      
  }
  
  return(list(scenario= scenario,replaceRecord_df=replaceRecord_df,newSelectionForCall=newSelectionForCall))
}


ConstructNewAllocDf <- function(selectionForCall,newResource,newResourceAmount,resource_df,availAsset_df){
  
  idxResourceNew <- which(resource_df$id==newResource)
  
  newResource_df <- resource_df[idxResourceNew,]
  newResourceAvail_df <- availAsset_df[which(availAsset_df$assetCustacId==newResource),]
  
  newHaircut <- newResourceAvail_df$haircut+newResourceAvail_df$FXHaircut
  newResourceMinUnitQuantity <- floor(newResourceAmount*newResource_df$FXRate/(newResource_df$minUnitValue*(1-newHaircut)))
  
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

