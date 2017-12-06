
ManualAllocationAssetToCall <- function(algoVersion,selectCallId, newResource,newResourceAmount,selectionForCall,
                                 resource_df,callInfo_df,availAsset_df,
                                 pref_vec,operLimit,operLimitMc,fungible){
  pref_vec/sum(pref_vec[1:2])
  # Initialize the outputs
  newSelectionForCall <- selectionForCall
  replaceRecord_df <- selectionForCall[-(1:length(selectionForCall$Asset)),]
  
  #### Check for Exceptions ####
  # 1. movements limit
  # every asset in the current allocation => newAssetAmount
  movements <- length(selectionForCall$Asset)
  if(all(selectionForCall$`NetAmount(USD)` > newAssetAmount)){
    movementExcess <- T
  } else{
    movementExcess <- F
  }
  
  if(selectionForCall$Asset==SplitResource(newResource,"asset") && selectionForCall$CustodianAccount==SplitResource(newResource,"custodianAccount")){
    # Scenario 1
    sumNewResourceAmount <- selectionForCall$`NetAmount(USD)` + newResourceAmount
    newSelectionForCall <- ConstructNewAllocDf(selectionForCall,newResource,sumNewResourceAmount,resource_df,availAsset_df)
  } 
  
  idxSelectionNew <- which(selectionForCall$Asset==SplitResource(newResource,"asset") && selectionForCall$CustodianAccount==SplitResource(newResource,"custodianAccount"))
  if(length(idxSelectionNew)==1){
    # Scenario 3
    
  }
  
  #### Calculate the Optimal Asset
  orderedResource_vec <- OrderResource(resource_df,availAsset_df,newResource,callInfo_df)


  #### create the new asset to the selection
  newAlloc_df <- ConstructNewAllocDf(selectionForCall,newResource,newResourceAmount,resource_df,availAsset_df)

  replaceResource <- orderedResource_vec[1]
  idxSelectionReplace <- which(selectionForCall$Asset== SplitResource(replaceResource,"asset") && selectionForCall$CustodianAccount==SplitResource(replaceResource,"custodianAccount"))
  
  # compare netAmount
  if(selectionForCall$`NetAmount(USD)`[idxSelectionReplace]>newAlloc_df$`NetAmount(USD)`){
    if(length(selectionForCall$Asset)+1 > operLimitMc){ 
      # Scenario 2:
      # change the replaceResouce line in selectionForCall
      # record the amount & quantity of the asset to be replaced separately
      replaceRecord_df <- ConstructReplaceRecord(selectionForCall,replaceResource,newResource,newAlloc_df$`NetAmount(USD)`,resource_df)
      
      # update the old selection
      oldSelectionForCall <- UpdateSelectionByQty(selectionForCall,replaceResource,replaceRecord_df$Quantity,resource_df)
      
      # conbine the new allocation and the updated original selection
      newSelectionForCall <- rbind(oldSelectionForCall,newAlloc_df)
      rownames(newSelectionForCall)
    } else if(length(selectionForCall$Asset)+1 > operLimitMc && movementExcess){
      # Scenario 5:
      replaceRecord_df <- ConstructReplaceRecord(selectionForCall,replaceResource,newResource,newAlloc_df$`NetAmount(USD)`,resource_df)
      oldSelectionForCall <- UpdateSelectionByQty(selectionForCall,replaceResource,replaceRecord_df$Quantity,resource_df)
      
      newSelectionForCall <- rbind(oldSelectionForCall,newAlloc_df)
      rownames(newSelectionForCall)
      
      warning("Allocation generates movements more than limit")      
    } else if(selectionForCall+1 > operLimitMc && !movementExcess){ 
      # Scenario 4:
      # the entire allocation from this call
      idxSelectionReplace1 <- which(selectionForCall$`NetAmount(USD)`<= newAlloc_df$`NetAmount(USD)`)[1]
      replaceRecord1_df <- selectionForCall[idxSelectionReplace1,]
      
      # remove the line from selectionForCall
      oldSelectionForCall <- selectionForCall[-idxSelectionReplace1,]
      
      # check the amount needs to be removed from the first asset
      replaceAmount2 <- newAlloc_df$`NetAmount(USD)` - replaceRecord_df$`NetAmount(USD)`
      
      replaceResource2 <- PasteResource(selectionForCall$Asset[1],selectionForCall$CustodianAccount[idxSelectionReplace2])
      replaceRecord2_df <- ConstructReplaceRecord(selectionForCall,replaceResource,newResource,replaceAmount2,resource_df)
      oldSelectionForCall <- UpdateSelectionByQty(oldSelectionForCall,replaceResource,replaceRecord_df$Quantity,resource_df)
      
      # conbine the new allocation and the updated original selection
      replaceRecord_df <- rbind(replaceRecord1_df,replaceRecord2_df)
      newSelectionForCall <- rbind(oldSelectionForCall,newAlloc_df)
      rownames(newSelectionForCall)
    }
  } else {
    
    oldSelectionForCall <- selectionForCall
    amountLeft <- newResourceAmount
    for(idxSelectionReplace in 1:length(orderedResource_vec)){
      if(amountLeft==0){
        break
      }
      if(amountLeft >= oldSelectionForCall$`NetAmount(USD)`[idxSelectionReplace]){
        replaceAmount <- oldSelectionForCall$`NetAmount(USD)`[idxSelectionReplace]
        thisReplaceRecord_df <- ConstructReplaceRecord(oldSelectionForCall,replaceResource,newResource,replaceAmount,resource_df)
        
        oldSelectionForCall <- oldSelectionForCall[-idxSelectionReplace1,]
        
      } else{
        replaceAmount <- amountLeft
        thisReplaceRecord_df <- ConstructReplaceRecord(oldSelectionForCall,replaceResource,newResource,replaceAmount,resource_df)
        
        oldSelectionForCall <- UpdateSelectionByQty(oldSelectionForCall,replaceResource,replaceRecord_df$Quantity,resource_df)
      }
      replaceRecord_df <- rbind(replaceRecord_df,thisReplaceRecord_df) 
      idxSelectionReplace <- idxSelectionReplace+1
      
      amountLeft <- amountLeft - replaceAmount
    }
    
    # conbine the new allocation and the updated original selection
    newSelectionForCall <- rbind(oldSelectionForCall,newAlloc_df)
    rownames(newSelectionForCall)
  }
  
  return(list(replaceRecord_df=replaceRecord_df,newSelectionForCall=newSelectionForCall))
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

ConstructNewAllocDf <- function(selectionForCall,newResource,newResourceAmount,resource_df,availAsset_df){
  
  idxResourceNew <- which(resource_df$id==newResource)
  
  newResource_df <- resource_df[idxResourceNew,]
  newResourceAvail_df <- availAsset_df[which(availAsset_df$assetCustacId==newResource),]
  
  newResourceMinUnitQuantity <- floor(newResourceAmount*resource_df$FXRate[idxResourceReplace]/(resource_df$minUnitValue[idxResourceReplace]*(1-selectionForCall$Haircut[idxSelectionReplace])))
  
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

