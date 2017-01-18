
############ ALGORITHM ##########################################
secondAllocationFunction<- 
  function(callIds,clientId,pref,deselectAssetId,deselectCallId,current.selection,availAssets){
    
  pref <- pref/sum(pref)
  # update the asset quantity which can be used to allocate to the call
  call.num <- length(callIds)
  assetIds <- availAssets$assetId
  asset.num <- length(assetIds)
  minUnit.vec <- availAssets$minUnit
  quantity.vec <- availAssets$quantity/minUnit.vec
  haircut.vec <- availAssets$haircut+availAssets$FXHaircut
  minUnitValue.vec <- availAssets$minUnitValue
  cost.percent.vec <- availAssets$internalCost+availAssets$externalCost+availAssets$opptCost-availAssets$interestRate
  quantity.used <- rep(0,asset.num)
  
  # calculate the used quantity of each asset
  for(i in 1:call.num){
    for(j in 1:asset.num){
      asset.idx <- which(current.selection[[callIds[i]]]$Asset==assetIds[j])
      if(length(asset.idx)!=0){
        quantity.used[j] <- quantity.used[j]+current.selection[[callIds[i]]]$Quantity[asset.idx]/minUnit.vec[i]
      }
    }
  }
  quantity.left <- quantity.vec-quantity.used
  
  excess.call.percent <- 0.2
  callAmount <- callInfo$callAmount*(1+excess.call.percent*pref[1])
  
  allocation.deselectCall <- current.selection[[deselectCallId]]
  lackAmount <- callAmount- sum(allocation.deselectCall$`NetAmount(USD)`)  # the amount left needs to be fulfilled after deseleting one asset
  lackQuantity.vec <- ceiling(lackAmount/(1-haircut.vec)/minUnitValue.vec) # quantity needed for a single asset to fulfill each call
                                                       # could either add the amount of the original selection or add another one or several assets.
  new.allocation.deselectCall <- allocation.deselectCall
  
  # calculate the cost if only the integral units of asset can be allocated
  integer.call.vec <- ceiling(lackAmount/(1-haircut.vec)/minUnitValue.vec)*minUnitValue.vec*(1-haircut.vec)
  
  cost.vec<-integer.call.vec/(1-haircut.vec)*cost.percent.vec  # cost amount
  liquidity.vec <- (1-haircut.vec)^2                           # define asset liquidity
  
  # the operational efficiency #
  # the assets already allocated to the call should be considered more operation efficient
  # the value should be 0, because the the amount of the that asset won't cost more operations
  # 0, if the asset is already allocated to the call 
  # 1, if the asset is the call currency cash
  # 10, otherwise 
  operation.vec <- rep(10,asset.num)                            
  ccy.idx <- which(callInfo$currency==assetIds)  # return the index of mc currency cash in the assetId list
  if(length(ccy.idx)==1){                        # if there exist call currency cash in the inventory
      operation.vec[ccy.idx] <- 1
  }
  assetIds.allocated.deselctCall <- current.selection[[deselectCallId]]$Asset
  assetQuanity.allocated.deselectCall <- current.selection[[deselectCallId]]$Quantity
  if(length(assetIds.allocated.deselctCall)>=1){
    temp.idx <- match(assetIds.allocated.deselctCall,assetIds)
    operation.vec[temp.idx] <- 0
  }
  
  # normalize the the objectives
  norm.cost.vec <- cost.vec
  if(length(unique(cost.vec))==1){
    norm.cost.vec[]<-1
  }else{
    norm.cost.vec <- as.vector(scale(cost.vec))
    norm.cost.vec <- norm.cost.vec + (-min(norm.cost.vec)*2)
  }
  norm.liquidity.vec <- liquidity.vec
  if(length(unique(liquidity.vec))==1){
    norm.liquidity.vec[] <-1
  }else{
    norm.liquidity.vec <- as.vector(scale(liquidity.vec))
    norm.liquidity.vec <- norm.liquidity.vec+(-min(norm.liquidity.vec)*2)
  }
  
  norm.operation.vec <- operation.vec
  
  # calculate the overall(three objs) scores of the assets
  optimal.vec <- norm.operation.vec*pref[1]+norm.liquidity.vec*pref[2]+norm.cost.vec*pref[3]
  names(optimal.vec) <- assetIds
  optimal.vec <-sort(optimal.vec)  # sort the 'cost' of the assets, from the most to the least optimal

  # check whether the first optimal asset is already allocated to that call
  # check whether the first optimal asset is enough to fulfill the call
  for(i in 1:length(optimal.vec)){
    temp.asset <- names(optimal.vec[i])
    if(is.element(temp.asset,assetIds.allocated.deselctCall)){
      if(quantity.left[i]>=lackQuantity.vec[i]){
        asset.idx <- match(temp.asset,assetIds.allocated.deselctCall)
        add.quantity <- lackQuantity.vec[i]
        
        # update the arraies - quantity.left and quantity.used
        quantity.left[i] <- quantity.left[i]-add.quantity[i]
        quantity.used[i] <- quantity.vec[i] -quantity.left[i]
        add.amount <- add.quantity*minUnitValue.vec[i]
        add.netAmount <- add.amount*(1-haircut.vec[i])
        
        new.quantity <- new.allocation.deselectCall$Quantity[asset.Idx]+add.quantity
        new.amount <- new.allocation.deselectCall$Amount[asset.idx]+add.amount
        new.netAmount <- new.allocation.deselectCall$`NetAmount(USD)`[asset.idx]+ add.netAmount
        
        new.allocation.deselectCall$`NetAmount(USD)`<- new.netAmount
        new.allocation.deselectCall$Amount <- new.amount
        new.allocation.deselectCall$Quantity <- new.quantity
        
        # update the lackAmount and lackQuantity.vec
        lackAmount <- lackAmount-add.netAmount
        lackQuantity.vec[] <- 0
        
        break
      }else if(quantity.left[i] > 0){
        asset.idx <- match(temp.asset,assetIds.allocated.deselctCall)
        add.quantity <- quantity.left[i]
        quantity.left[i] <- quantity.left[i]-add.quantity[i]
        quantity.used[i] <- quantity.vec[i] -quantity.left[i]
        add.amount <- add.quantity*minUnitValue.vec[i]
        add.netAmount <- add.amount*(1-haircut.vec[i])
        
        new.quantity <- new.allocation.deselectCall$Quantity[asset.Idx]+add.quantity
        new.amount <- new.allocation.deselectCall$Amount[asset.idx]+add.amount
        new.netAmount <- new.allocation.deselectCall$`NetAmount(USD)`[asset.idx]+ add.netAmount
        
        new.allocation.deselectCall$`NetAmount(USD)`[asset.idx]<- round(new.netAmount,2)
        new.allocation.deselectCall$Amount[asset.idx] <- round(new.amount,2)
        new.allocation.deselectCall$Quantity[asset.idx] <- round(new.quantity,2)
        
        # update the lackAmount and lackQuantity.vec
        lackAmount <- lackAmount-new.netAmount
        lackQuantity.vec <- lackAmount/(1-haircut.vec)/minUnitValue.vec 
      }
    }else {
      if(quantity.left[i]>=lackQuantity.vec[i]){
        new.quantity <- lackQuantity.vec[i]
        new.amount <- new.quantity*minUnitValue.vec[i]
        new.netAmount <- new.amount*(1-haircut.vec[i])
        
        quantity.left[i] <- quantity.left[i]-lackQuantity.vec[i]
        quantity.used[i] <- quantity.vec[i] -quantity.left[i]
        
        # create the new line
        assetInfo.line <- assetInfo[which(assetInfo$id==temp.asset),]
        availAssets.line <- availAssets[which(availAssets$assetId==temp.asset),]
        new.asset <- c(temp.asset,assetInfo.line$name,new.netAmount,haircut.vec[i],new.amount,
                       assetInfo.line$currency,new.quantity,availAssets.line$CustodianAccount,availAssets.line$venue)
        new.allocation.deselectCall[length(allocation.deselectCall[,1])+1,]<- new.asset
        
        # update the lackAmount and lackQuantity.vec
        lackAmount <- lackAmount-new.netAmount
        lackQuantity.vec[] <- 0        
        break
      } else if(quantity.left[i] > 0){
        new.quantity <- quantity.left[i]
        new.amount <- new.quantity*minUnitValue.vec[i]
        new.netAmount <- new.amount*(1-haircut.vec[i])
        
        quantity.left[i] <- 0
        quantity.used[i] <- quantity.vec[i] -quantity.left[i]
        
        assetInfo.line <- assetInfo[which(assetInfo$id==temp.asset),]
        availAssets.line <- availAssets[which(availAssets$assetId==temp.asset),]
        new.asset <- c(temp.asset,assetInfo.line$name,new.netAmount,haircut.vec[i],new.amount,
                       assetInfo.line$currency,new.quantity,availAssets.line$CustodianAccount,availAssets.line$venue)
        new.allocation.deselectCall[length(allocation.deselectCall[,1])+1,]<- new.asset
        
        # update the lackAmount and lackQuantity.vec
        lackAmount <- lackAmount-new.netAmount
        lackQuantity.vec <- lackAmount/(1-haircut.vec)/minUnitValue.vec 
      }
    }
  }
  current.selection[[deselectCallId]]<- new.allocation.deselectCall
  output.list <- list(current.selection,new.allocation.deselectCall)
  return(output.list)
}





