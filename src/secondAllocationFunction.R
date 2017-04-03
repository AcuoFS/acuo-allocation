
############ ALGORITHM ##########################################
SecondAllocationAlgoV2<- 
  function(callId_vec,callInfo_df,resource_vec,pref_vec,deselectAssetId,deselectCallId,currentSelection_list,availAsset_df,assetInfo_df){
    
  pref_vec <- pref_vec/sum(pref_vec)
  # update the asset tempQuantity_vec which can be used to allocate to the call
  callNum <- length(callId_vec)
  assetId_vec <- as.character(data.frame(strsplit(resource_vec,'-'))[1,])
  resourceNum <- length(resource_vec)
  
  msIds <- callInfo_df$marginStatement
  idxDeselectCall <- which(callInfo_df$id==deselectCallId)
  deselectMs <- msIds[idxDeselectCall]
  idxTemp <- which(callInfo_df$marginStatement==deselectMs & callInfo_df$id!=deselectCallId)
  sameMsCallId <- 'na'
  if(length(idxTemp)==1){
    sameMsCallId <- callInfo_df$id[idxTemp]
  }
  
  minUnit_vec <- availAsset_df$minUnit
  quantity_vec <- availAsset_df$quantity/minUnit_vec
  haircut_vec <- availAsset_df$haircut+availAsset_df$FXHaircut
  FXRate_vec <- availAsset_df$FXRate
  minUnitValue_vec <- availAsset_df$minUnitValue
  costBasis_vec <- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-availAsset_df$interestRate
  quantityUsed_vec <- rep(0,resourceNum)
  
  # calculate the used tempQuantity_vec of each asset
  for(i in 1:callNum){
    for(j in 1:resourceNum){
      currentResource_vec <- paste(currentSelection_list[[callId_vec[i]]]$Asset,currentSelection_list[[callId_vec[i]]]$CustodianAccount,sep='-')
      idxResource_vec <- which(currentResource_vec==resource_vec[j]) 
      if(length(idxResource_vec)!=0){
        quantityUsed_vec[j] <- quantityUsed_vec[j]+currentSelection_list[[callId_vec[i]]]$Quantity[idxResource_vec]/minUnit_vec[j]
      }
    }
  }
  quantityLeft_vec <- quantity_vec-quantityUsed_vec
  
  excellCallPercent <- 0.2
  callAmount <- callInfo_df$callAmount[which(callInfo_df$id==deselectCallId)]*(1+excellCallPercent*pref_vec[1])
  
  allocationDeselectCall_df <- currentSelection_list[[deselectCallId]]
  lackAmount <- callAmount- sum(allocationDeselectCall_df$`NetAmount(USD)`)  # the amount left needs to be fulfilled after deseleting one asset
  # got incorrect result 03/03/2017
  # reason: calculation of lackQuantity didn't include the FX Rate
  lackQuantity_vec <- ceiling(lackAmount/(1-haircut_vec)/minUnitValue_vec*FXRate_vec) # tempQuantity_vec needed for a single asset to fulfill each call
                                                       # could either add the amount of the original selection or add another one or several assets.
  newAllocationDeselectCall_df <- allocationDeselectCall_df
  
  # calculate the cost if only the integral units of asset can be allocated
  integerCallAmount_vec <- ceiling(lackAmount/(1-haircut_vec)/minUnitValue_vec)*minUnitValue_vec*(1-haircut_vec)
  
  cost_vec<-integerCallAmount_vec/(1-haircut_vec)*costBasis_vec  # cost amount
  liquidity_vec <- (1-haircut_vec)^2                           # define asset liquidity
  
  # the operational efficiency #
  # the assets already allocated to the call should be considered more operation efficient
  # the value should be 0, because the the amount of the that asset won't cost more operations
  # 0, if the asset is already allocated to the call -> margin statement 
  # 1, if the asset is the call currency cash
  # 10, otherwise 
  operation_vec <- rep(10,resourceNum)                            
  idxCcy <- which(callInfo_df$currency[idxDeselectCall]==assetId_vec)  # return the index of mc currency cash in the assetId list
  if(length(idxCcy)>=1){                        # if there exist call currency cash in the inventory
      operation_vec[idxCcy] <- 1
  }
  # asset selection from the margin statement
  resourceAllocatedDeselectCall_vec <- paste(currentSelection_list[[deselectCallId]]$Asset,currentSelection_list[[deselectCallId]]$CustodianAccount,sep='-')
  if(sameMsCallId!='na'){
    sameMsSelection_vec <- paste(currentSelection_list[[sameMsCallId]]$Asset,currentSelection_list[[sameMsCallId]]$CustodianAccount,sep='-')
    resourceAllocatedDeselectMs_vec <- unique(c(resourceAllocatedDeselectCall_vec,sameMsSelection_vec))
  } else{
    resourceAllocatedDeselectMs_vec <- resourceAllocatedDeselectCall_vec
  }
    
  assetQuanityAllocatedDeselectCall_vec <- currentSelection_list[[deselectCallId]]$Quantity
  if(length(resourceAllocatedDeselectMs_vec)>=1){
    idxTemp <- match(resourceAllocatedDeselectMs_vec,resource_vec)
    operation_vec[idxTemp] <- 0
  }

# normalize the the objectives
normCost_vec <- cost_vec
if(length(unique(cost_vec))==1){
  normCost_vec[]<-1
}else{
  normCost_vec <- as.vector(scale(cost_vec))
  normCost_vec <- normCost_vec + (-min(normCost_vec)*2)
}
normLiquidity_vec <- liquidity_vec
if(length(unique(liquidity_vec))==1){
  normLiquidity_vec[] <-1
}else{
  normLiquidity_vec <- as.vector(scale(liquidity_vec))
  normLiquidity_vec <- normLiquidity_vec+(-min(normLiquidity_vec)*2)
}

normOperation_vec <- operation_vec

# calculate the overall(three objs) scores of the assets
optimal_vec <- normOperation_vec*pref_vec[1]+normLiquidity_vec*pref_vec[2]+normCost_vec*pref_vec[3]
names(optimal_vec) <- resource_vec
optimal_vec <-sort(optimal_vec)  # sort the 'cost' of the assets, from the most to the least optimal
# idxOptimal_vec: the index of the optimal assets in the resource_vec
idxOptimal_vec <- match(names(optimal_vec),resource_vec)
  
# check whether the first optimal asset is already allocated to that margin statement
# check whether the first optimal asset is enough to fulfill the margin call
scenario <- 0
for(i in idxOptimal_vec){
  # incorrect answer: 03/03/2017
  # reason: the order in optimal_vec and assetCustadIds are not unified
  tempResource <- resource_vec[i]
  
  tempAssetId <- as.character(data.frame(strsplit(tempResource,'-'))[1,])
  # create the new line
  lineAssetInfo_df <- assetInfo_df[which(assetInfo_df$id==tempAssetId),]
  lineAvailAsset_df <- availAsset_df[which(availAsset_df$callId==deselectCallId),]
  lineCallInfo_df <- callInfo_df[which(callInfo_df$id==deselectCallId),]
  
  if(is.element(tempResource,resourceAllocatedDeselectCall_vec)){
    # if asset[i] is alreadly selected in the margin statment
    
    if(quantityLeft_vec[i]>=lackQuantity_vec[i]){
      # scenario 1: asset[i] left tempQuantity_vec is larger than the insufficient tempQuantity_vec
      
      scenario <- 1
      idxResource_vec <- match(tempResource,resourceAllocatedDeselectMs_vec)
      add.tempQuantity_vec <- lackQuantity_vec[i]
      
      # update the arraies - quantityLeft_vec and quantityUsed_vec
      quantityLeft_vec[i] <- quantityLeft_vec[i]-add.tempQuantity_vec[i]
      quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
      
      addAmount <- add.tempQuantity_vec*minUnitValue_vec[i]
      addAmountUSD <- addAmount/lineAssetInfo_df$FXRate
      addNetamountUSD <- addAmountUSD*(1-haircut_vec[i])
      addNetAmount <- addNetamountUSD*FXRate_vec[i]

      newQuantity <- newAllocationDeselectCall_df$Quantity[idxResource_vec]+add.tempQuantity_vec
      newAmountUSD <- newAllocationDeselectCall_df$`Amount(USD)`[idxResource_vec]+addAmountUSD
      newAmount <- newAllocationDeselectCall_df$Amount[idxResource_vec]+addAmount
      newNetAmountUSD <- newAllocationDeselectCall_df$`NetAmount(USD)`[idxResource_vec]+ addNetamountUSD
      newNetAmount <- newAllocationDeselectCall_df$`NetAmount`[idxResource_vec]+ addNetAmount
      
      newAllocationDeselectCall_df$`NetAmount(USD)`<- newNetAmountUSD
      newAllocationDeselectCall_df$NetAmount<- newNetAmount
      newAllocationDeselectCall_df$`Amount(USD)` <- newAmountUSD
      newAllocationDeselectCall_df$Amount <- newAmount
      newAllocationDeselectCall_df$Quantity <- newQuantity
      
      # update the lackAmount and lackQuantity_vec
      lackAmount <- lackAmount-addNetamountUSD
      lackQuantity_vec[] <- 0
      
      break
    }else if(quantityLeft_vec[i] > 0){
      # scenario 2: asset[i] is less than the insufficient tempQuantity_vec but larger than 0
      scenario <- 2
      
      idxResource_vec <- match(tempResource,resourceAllocatedDeselectMs_vec)
      add.tempQuantity_vec <- quantityLeft_vec[i]
      quantityLeft_vec[i] <- quantityLeft_vec[i]-add.tempQuantity_vec[i]
      quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
      addAmount <- add.tempQuantity_vec*minUnitValue_vec[i]
      addAmountUSD <- addAmount/lineAssetInfo_df$FXRate
      addNetamountUSD <- addAmountUSD*(1-haircut_vec[i])
      addNetAmount <- addNetamountUSD*FXRate_vec[i]
      
      newQuantity <- newAllocationDeselectCall_df$Quantity[idxResource_vec]+add.tempQuantity_vec
      newAmountUSD <- newAllocationDeselectCall_df$`Amount(USD)`[idxResource_vec]+addAmountUSD
      newAmount <- newAllocationDeselectCall_df$Amount[idxResource_vec]+addAmount
      newNetAmountUSD <- newAllocationDeselectCall_df$`NetAmount(USD)`[idxResource_vec]+ addNetamountUSD
      newNetAmount <- newAllocationDeselectCall_df$`NetAmount`[idxResource_vec]+ addNetAmount
      
      newAllocationDeselectCall_df$`NetAmount(USD)`[idxResource_vec]<- round(newNetAmountUSD,2)
      newAllocationDeselectCall_df$NetAmount<- round(newNetAmount,2)
      newAllocationDeselectCall_df$`Amount(USD)`[idxResource_vec] <- round(newAmountUSD,2)
      newAllocationDeselectCall_df$Amount[idxResource_vec] <- round(newAmount,2)
      newAllocationDeselectCall_df$Quantity[idxResource_vec] <- round(newQuantity,2)
      
      # update the lackAmount and lackQuantity_vec
      lackAmount <- lackAmount-newNetAmountUSD
      lackQuantity_vec <- lackAmount/(1-haircut_vec)/minUnitValue_vec 
    }
  } else {
    # if asset[i] is not in the current selection for margin statement
    
    if(quantityLeft_vec[i]>=lackQuantity_vec[i]){
      # scenario 3: asset[i] left tempQuantity_vec is larger than the insufficient tempQuantity_vec
      scenario <- 3
      
      newQuantity <- lackQuantity_vec[i] 
      quantityLeft_vec[i] <- quantityLeft_vec[i]-lackQuantity_vec[i]
      quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]

      newAmount <- newQuantity*minUnitValue_vec[i]
      newAmountUSD <- newAmount/lineAssetInfo_df$FXRate
      newNetAmountUSD <- newAmountUSD*(1-haircut_vec[i])
      newNetAmount <- newNetAmountUSD*FXRate_vec[i]
      
      new.asset <- c(tempAssetId,lineAssetInfo_df$name,newNetAmount,newNetAmountUSD,lineAssetInfo_df$FXRate,haircut_vec[i],newAmount,newAmountUSD,
                     lineAssetInfo_df$currency,newQuantity,lineAvailAsset_df$CustodianAccount,lineAvailAsset_df$venue,lineCallInfo_df$marginType)
      newAllocationDeselectCall_df[length(allocationDeselectCall_df[,1])+1,]<- new.asset
      
      # update the lackAmount and lackQuantity_vec
      lackAmount <- lackAmount-newNetAmountUSD
      lackQuantity_vec[] <- 0        
      break
    } else if(quantityLeft_vec[i] > 0){
      # scenario 4: asset[i] left tempQuantity_vec is less than the insufficient tempQuantity_vec but larger than 0
      scenario <- 4
      
      newQuantity <- quantityLeft_vec[i]
      quantityLeft_vec[i] <- 0
      quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
      
      newAmount <- newQuantity*minUnitValue_vec[i]
      newAmountUSD <- newAmount/lineAssetInfo_df$FXRate
      newNetAmountUSD <- newAmountUSD*(1-haircut_vec[i])
      newNetAmount <- newNetAmountUSD*FXRate_vec[i]
      
      new.asset <- c(tempAssetId,lineAssetInfo_df$name,newNetAmount,newNetAmountUSD,lineAssetInfo_df$FXRate,haircut_vec[i],newAmount,newAmountUSD,
                     lineAssetInfo_df$currency,newQuantity,lineAvailAsset_df$CustodianAccount,lineAvailAsset_df$venue,lineCallInfo_df$marginType)
      newAllocationDeselectCall_df[length(allocationDeselectCall_df[,1])+1,]<- new.asset
      
      # update the lackAmount and lackQuantity_vec
      lackAmount <- lackAmount-newNetAmountUSD
      lackQuantity_vec <- lackAmount/(1-haircut_vec)/minUnitValue_vec 
    }
  }
}
#cat('scenario: ',scenario,'\n')
#cat('asset: ',i,'  ',tempResource,'\n')

currentSelection_list[[deselectCallId]]<- newAllocationDeselectCall_df
output_list <- list(new.selection=currentSelection_list)
return(output_list)
}




