SecondAllocationAlgoV1<- function(callId_vec, resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,
                                  currentSelection_list,dsAssetId,dsCallId){
  #### Remove the Deselected Asset Start ####
  idxTemp <- which(currentSelection_list[[dsCallId]]$Asset==dsAssetId)
  currentSelection_list[[dsCallId]] <- currentSelection_list[[dsCallId]][-idxTemp,]
  #### Remove the Deselected Asset END ######
  
  
  #### Prepare Parameters Start #############################
  pref_vec <- pref_vec/sum(pref_vec)
  
  # update the asset tempQuantity_vec which can be used to allocate to the call
  callNum <- length(callId_vec)
  assetId_vec <- matrix(unlist(strsplit(resource_vec,'-')),nrow=2)[1,]
  resourceNum <- length(resource_vec)
  
  msIds <- callInfo_df$marginStatement
  idxdsCall <- which(callInfo_df$id==dsCallId)
  deselectMs <- msIds[idxdsCall]
  idxTemp <- which(callInfo_df$marginStatement==deselectMs & callInfo_df$id!=dsCallId)
  sameMsCallId <- 'na'
  if(length(idxTemp)==1){
    sameMsCallId <- callInfo_df$id[idxTemp]
  }
  
  minUnit_vec <- availAsset_df$minUnit
  #### quantity_vec: minUnitQuantity
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
  
  
  #### calculate the sufficient amount & quantity of assets to ful fill the margin call
  callAmount <- callInfo_df$callAmount[which(callInfo_df$id==dsCallId)]
  
  allocationDsCall_df <- currentSelection_list[[dsCallId]]
  lackAmount <- callAmount- sum(allocationDsCall_df$`NetAmount(USD)`)  # the amount left needs to be fulfilled after deseleting one asset
  # got incorrect result 03/03/2017
  # reason: calculation of lackQuantity didn't include the FX Rate
  lackQuantity_vec <- ceiling(lackAmount/(1-haircut_vec)/minUnitValue_vec*FXRate_vec) # tempQuantity_vec needed for a single asset to fulfill each call
  # could either add the amount of the original selection or add another one or several assets.
  newAllocationDsCall_df <- allocationDsCall_df
  
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
  idxCcy <- which(callInfo_df$currency[idxdsCall]==assetId_vec)  # return the index of mc currency cash in the assetId list
  if(length(idxCcy)>=1){                        # if there exist call currency cash in the inventory
    operation_vec[idxCcy] <- 1
  }
  # asset selection from the margin statement
  resourceInDsCall_vec <- paste(currentSelection_list[[dsCallId]]$Asset,currentSelection_list[[dsCallId]]$CustodianAccount,sep='-')
  if(sameMsCallId!='na'){
    sameMsSelection_vec <- paste(currentSelection_list[[sameMsCallId]]$Asset,currentSelection_list[[sameMsCallId]]$CustodianAccount,sep='-')
    resourceInDeselectMs_vec <- unique(c(resourceInDsCall_vec,sameMsSelection_vec))
  } else{
    resourceInDeselectMs_vec <- resourceInDsCall_vec
  }
  
  assetQuanityAllocateddsCall_vec <- currentSelection_list[[dsCallId]]$Quantity
  if(length(resourceInDeselectMs_vec)>=1){
    idxTemp <- match(resourceInDeselectMs_vec,resource_vec)
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
  optimal_vec <- normOperation_vec*pref_vec[3]+normLiquidity_vec*pref_vec[2]+normCost_vec*pref_vec[1]
  names(optimal_vec) <- resource_vec
  optimal_vec <-sort(optimal_vec)  # sort the 'cost' of the assets, from the most to the least optimal
  # idxOptimal_vec: the index of the optimal assets in the resource_vec
  idxOptimal_vec <- match(names(optimal_vec),resource_vec)
  
  # check whether the first optimal asset is already allocated to that margin statement
  # check whether the first optimal asset is enough to fulfill the margin call
  scenario <- 0
  for(j in 1:length(idxOptimal_vec)){
    # incorrect answer: 03/03/2017
    # reason: the order in optimal_vec and assetCustadIds are not unified
    i <- idxOptimal_vec[j]
    resource <- resource_vec[i]
    
    assetId <- matrix(unlist(strsplit(resource,'-')),nrow=2)[1,]
    # create the new line
    lineAssetInfo_df <- assetInfo_df[which(assetInfo_df$id==assetId),]
    lineAvailAsset_df <- availAsset_df[which(availAsset_df$callId==dsCallId),]
    lineCallInfo_df <- callInfo_df[which(callInfo_df$id==dsCallId),]
    
    if(is.element(resource,resourceInDsCall_vec)){
      # if asset[i] is alreadly selected in the margin statment
      
      if(quantityLeft_vec[i]>=lackQuantity_vec[i]){
        # scenario 1: asset[i] left tempQuantity_vec is larger than the insufficient tempQuantity_vec
        
        scenario <- 1
        idxResource_vec <- match(resource,resourceInDeselectMs_vec)
        addTempQuantity_vec <- lackQuantity_vec[i]
        
        # update the arraies - quantityLeft_vec and quantityUsed_vec
        quantityLeft_vec[i] <- quantityLeft_vec[i]-addTempQuantity_vec
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        
        addAmount <- addTempQuantity_vec*minUnitValue_vec[i]
        addAmountUSD <- addAmount/lineAssetInfo_df$FXRate
        addNetamountUSD <- addAmountUSD*(1-haircut_vec[i])
        addNetAmount <- addNetamountUSD*FXRate_vec[i]
        
        newQuantity <- newAllocationDsCall_df$Quantity[idxResource_vec]+addTempQuantity_vec
        newAmountUSD <- newAllocationDsCall_df$`Amount(USD)`[idxResource_vec]+addAmountUSD
        newAmount <- newAllocationDsCall_df$Amount[idxResource_vec]+addAmount
        newNetAmountUSD <- newAllocationDsCall_df$`NetAmount(USD)`[idxResource_vec]+ addNetamountUSD
        newNetAmount <- newAllocationDsCall_df$`NetAmount`[idxResource_vec]+ addNetAmount
        
        newAllocationDsCall_df$`NetAmount(USD)`<- newNetAmountUSD
        newAllocationDsCall_df$NetAmount<- newNetAmount
        newAllocationDsCall_df$`Amount(USD)` <- newAmountUSD
        newAllocationDsCall_df$Amount <- newAmount
        newAllocationDsCall_df$Quantity <- newQuantity
        
        # update the lackAmount and lackQuantity_vec
        lackAmount <- lackAmount-addNetamountUSD
        lackQuantity_vec[] <- 0
        
        break
      }else if(quantityLeft_vec[i] > 0){
        # scenario 2: asset[i] is less than the insufficient tempQuantity_vec but larger than 0
        scenario <- 2
        
        idxResource_vec <- match(resource,resourceInDeselectMs_vec)
        addTempQuantity_vec <- quantityLeft_vec[i]
        quantityLeft_vec[i] <- quantityLeft_vec[i]-addTempQuantity_vec
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        addAmount <- addTempQuantity_vec*minUnitValue_vec[i]
        addAmountUSD <- addAmount/lineAssetInfo_df$FXRate
        addNetamountUSD <- addAmountUSD*(1-haircut_vec[i])
        addNetAmount <- addNetamountUSD*FXRate_vec[i]
        
        newQuantity <- newAllocationDsCall_df$Quantity[idxResource_vec]+addTempQuantity_vec
        newAmountUSD <- newAllocationDsCall_df$`Amount(USD)`[idxResource_vec]+addAmountUSD
        newAmount <- newAllocationDsCall_df$Amount[idxResource_vec]+addAmount
        newNetAmountUSD <- newAllocationDsCall_df$`NetAmount(USD)`[idxResource_vec]+ addNetamountUSD
        newNetAmount <- newAllocationDsCall_df$`NetAmount`[idxResource_vec]+ addNetAmount
        
        newAllocationDsCall_df$`NetAmount(USD)`[idxResource_vec]<- round(newNetAmountUSD,2)
        newAllocationDsCall_df$NetAmount<- round(newNetAmount,2)
        newAllocationDsCall_df$`Amount(USD)`[idxResource_vec] <- round(newAmountUSD,2)
        newAllocationDsCall_df$Amount[idxResource_vec] <- round(newAmount,2)
        newAllocationDsCall_df$Quantity[idxResource_vec] <- round(newQuantity,2)
        
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
        
        newVenue <- lineAvailAsset_df$venue[which(lineAvailAsset_df$assetCustacId==resource)]
        newCustodianAccount <- lineAvailAsset_df$CustodianAccount[which(lineAvailAsset_df$assetCustacId==resource)]
        
        newAsset_df <- data.frame(assetId,lineAssetInfo_df$name,newNetAmount,newNetAmountUSD,lineAssetInfo_df$FXRate,haircut_vec[i],newAmount,newAmountUSD,
                                  lineAssetInfo_df$currency,newQuantity,newCustodianAccount,newVenue,lineCallInfo_df$marginType,deselectMs,dsCallId)
        newAllocationDsCall_df[length(allocationDsCall_df[,1])+1,]<- newAsset_df
        
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
        
        newVenue <- lineAvailAsset_df$venue[which(lineAvailAsset_df$assetCustacId==resource)]
        newCustodianAccount <- lineAvailAsset_df$CustodianAccount[which(lineAvailAsset_df$assetCustacId==resource)]
        
        newAsset_df <- data.frame(assetId,lineAssetInfo_df$name,newNetAmount,newNetAmountUSD,lineAssetInfo_df$FXRate,haircut_vec[i],newAmount,newAmountUSD,
                                  lineAssetInfo_df$currency,newQuantity,newCustodianAccount,newVenue,lineCallInfo_df$marginType,deselectMs,dsCallId)
        newAllocationDsCall_df[length(allocationDsCall_df[,1])+1,]<- newAsset_df
        
        # update the lackAmount and lackQuantity_vec
        lackAmount <- lackAmount-newNetAmountUSD
        lackQuantity_vec <- lackAmount/(1-haircut_vec)/minUnitValue_vec
      }
    }
  }
  #cat('scenario: ',scenario,'\n')
  #cat('asset: ',i,'  ',resource,'\n')
  
  currentSelection_list[[dsCallId]]<- newAllocationDsCall_df
  output_list <- list(newSelection=currentSelection_list)
  return(output_list)
}
