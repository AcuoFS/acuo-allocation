
SecondAllocationAlgoV1<- function(callId_vec, resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,
                                  dsAssetId,dsCallId, # or a list of msId(exclude from all margin statements)
                                  currentSelection_list){
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
  
  allocationdsCall_df <- currentSelection_list[[dsCallId]]
  lackAmount <- callAmount- sum(allocationdsCall_df$`NetAmount(USD)`)  # the amount left needs to be fulfilled after deseleting one asset
  # got incorrect result 03/03/2017
  # reason: calculation of lackQuantity didn't include the FX Rate
  lackQuantity_vec <- ceiling(lackAmount/(1-haircut_vec)/minUnitValue_vec*FXRate_vec) # tempQuantity_vec needed for a single asset to fulfill each call
  # could either add the amount of the original selection or add another one or several assets.
  newAllocationdsCall_df <- allocationdsCall_df
  
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
  resourceIndsCall_vec <- paste(currentSelection_list[[dsCallId]]$Asset,currentSelection_list[[dsCallId]]$CustodianAccount,sep='-')
  if(sameMsCallId!='na'){
    sameMsSelection_vec <- paste(currentSelection_list[[sameMsCallId]]$Asset,currentSelection_list[[sameMsCallId]]$CustodianAccount,sep='-')
    resourceInDeselectMs_vec <- unique(c(resourceIndsCall_vec,sameMsSelection_vec))
  } else{
    resourceInDeselectMs_vec <- resourceIndsCall_vec
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
    
    if(is.element(resource,resourceIndsCall_vec)){
      # if asset[i] is alreadly selected in the margin statment
      
      if(quantityLeft_vec[i]>=lackQuantity_vec[i]){
        # scenario 1: asset[i] left tempQuantity_vec is larger than the insufficient tempQuantity_vec
        
        scenario <- 1
        idxResource_vec <- match(resource,resourceInDeselectMs_vec)
        addTempQuantity_vec <- lackQuantity_vec[i]
        
        # update the arraies - quantityLeft_vec and quantityUsed_vec
        quantityLeft_vec[i] <- quantityLeft_vec[i]-addTempQuantity_vec[i]
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        
        addAmount <- addTempQuantity_vec*minUnitValue_vec[i]
        addAmountUSD <- addAmount/lineAssetInfo_df$FXRate
        addNetamountUSD <- addAmountUSD*(1-haircut_vec[i])
        addNetAmount <- addNetamountUSD*FXRate_vec[i]
        
        newQuantity <- newAllocationdsCall_df$Quantity[idxResource_vec]+addTempQuantity_vec
        newAmountUSD <- newAllocationdsCall_df$`Amount(USD)`[idxResource_vec]+addAmountUSD
        newAmount <- newAllocationdsCall_df$Amount[idxResource_vec]+addAmount
        newNetAmountUSD <- newAllocationdsCall_df$`NetAmount(USD)`[idxResource_vec]+ addNetamountUSD
        newNetAmount <- newAllocationdsCall_df$`NetAmount`[idxResource_vec]+ addNetAmount
        
        newAllocationdsCall_df$`NetAmount(USD)`<- newNetAmountUSD
        newAllocationdsCall_df$NetAmount<- newNetAmount
        newAllocationdsCall_df$`Amount(USD)` <- newAmountUSD
        newAllocationdsCall_df$Amount <- newAmount
        newAllocationdsCall_df$Quantity <- newQuantity
        
        # update the lackAmount and lackQuantity_vec
        lackAmount <- lackAmount-addNetamountUSD
        lackQuantity_vec[] <- 0
        
        break
      }else if(quantityLeft_vec[i] > 0){
        # scenario 2: asset[i] is less than the insufficient tempQuantity_vec but larger than 0
        scenario <- 2
        
        idxResource_vec <- match(resource,resourceInDeselectMs_vec)
        addTempQuantity_vec <- quantityLeft_vec[i]
        quantityLeft_vec[i] <- quantityLeft_vec[i]-addTempQuantity_vec[i]
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        addAmount <- addTempQuantity_vec*minUnitValue_vec[i]
        addAmountUSD <- addAmount/lineAssetInfo_df$FXRate
        addNetamountUSD <- addAmountUSD*(1-haircut_vec[i])
        addNetAmount <- addNetamountUSD*FXRate_vec[i]
        
        newQuantity <- newAllocationdsCall_df$Quantity[idxResource_vec]+addTempQuantity_vec
        newAmountUSD <- newAllocationdsCall_df$`Amount(USD)`[idxResource_vec]+addAmountUSD
        newAmount <- newAllocationdsCall_df$Amount[idxResource_vec]+addAmount
        newNetAmountUSD <- newAllocationdsCall_df$`NetAmount(USD)`[idxResource_vec]+ addNetamountUSD
        newNetAmount <- newAllocationdsCall_df$`NetAmount`[idxResource_vec]+ addNetAmount
        
        newAllocationdsCall_df$`NetAmount(USD)`[idxResource_vec]<- round(newNetAmountUSD,2)
        newAllocationdsCall_df$NetAmount<- round(newNetAmount,2)
        newAllocationdsCall_df$`Amount(USD)`[idxResource_vec] <- round(newAmountUSD,2)
        newAllocationdsCall_df$Amount[idxResource_vec] <- round(newAmount,2)
        newAllocationdsCall_df$Quantity[idxResource_vec] <- round(newQuantity,2)
        
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
        newAllocationdsCall_df[length(allocationdsCall_df[,1])+1,]<- newAsset_df
        
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
        newAllocationdsCall_df[length(allocationdsCall_df[,1])+1,]<- newAsset_df
        
        # update the lackAmount and lackQuantity_vec
        lackAmount <- lackAmount-newNetAmountUSD
        lackQuantity_vec <- lackAmount/(1-haircut_vec)/minUnitValue_vec 
      }
    }
  }
  #cat('scenario: ',scenario,'\n')
  #cat('asset: ',i,'  ',resource,'\n')
  
  currentSelection_list[[dsCallId]]<- newAllocationdsCall_df
  output_list <- list(newSelection=currentSelection_list)
  return(output_list)
}

SecondAllocationAlgoV2<- function(callId_vec,callInfo_df,resourceTotal_vec,availAssetTotal_df,assetInfoTotal_df,
                                  dsAssetId,dsCallId,
                                  pref_vec,operLimit,currentSelection_list){
  
  #### Prepare Inputs Start #####
  
  ## derive the availAsset_df for deselected call
  ## derive the resource_vec for deselected call
  ## derive the assetInfo_df for deselected call
  availAsset_df <- availAssetTotal_df[which(availAssetTotal_df$callId==dsCallId),]
  resource_vec <- unique(availAsset_df$assetCustacId)
  assetId_vec <- SplitResource(resource_vec,'asset')
  assetInfo_df <- assetInfoTotal_df[which(assetInfoTotal_df$id %in% assetId_vec),]
  
  pref_vec <- pref_vec/sum(pref_vec[1:2])
  resourceNum <- length(resource_vec)
  
  callNum <- length(callId_vec)
  
  #### 
  
  # coreInput_list <- AllocationInputData(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df)
  
  ####
  
  minUnit_vec <- availAsset_df$minUnit
  quantity_vec <- availAsset_df$quantity/minUnit_vec
  
  haircut_vec <- availAsset_df$haircut+availAsset_df$FXHaircut
  FXRate_vec <- availAsset_df$FXRate
  minUnitValue_vec <- availAsset_df$minUnitValue
  costBasis_vec <- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-availAsset_df$interestRate
  quantityUsed_vec <- rep(0,resourceNum)
  #### Prepare Inputs Start #####
  
  #### Calculate the Current Movements Start #########
  resourceInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]
  minUnit_mat <- matrix(rep(resourceInfo_df$minUnit,callNum),nrow=callNum,byrow=TRUE)
  ResultList2Mat(callOutput_list,callId_vec,resource_vec,minUnit_mat)
  
  movementsUsed <- OperationFun(currentSelection_list,callInfo_df,'callList')
  movementsLeft <- operLimit- movementsUsed

  #### Calculate the Current Movements END #########
  
  #### movements left cases Start ##################
  # < 0, shouldn't happen. If happen, there's definitely a violation of the first level algo
  #   any new allocation will violate the operational constraint
  # = 0, the deselected asset is alrealdy allocated to the same margin statement to the other margin call
  #   excluding this asset means any new allocation will violate the operational constraint,
  #   but we still need to allocate a new asset. 
  # = 1, we can allocate at most one new asset, 
  #   (and adjust the already allocated asset quantity, ok?)
  # >=2, we can allocate several new assets
  #### movements left cases END ####################
  
  #### Find the Other Margin Call in the Same Margin Statement Start ####
  msIds <- callInfo_df$marginStatement 
  
  idxdsCall <- which(callInfo_df$id==dsCallId)
  deselectMs <- msIds[idxdsCall]
  idxTemp <- which(callInfo_df$marginStatement==deselectMs & callInfo_df$id!=dsCallId)
  sameMsCallId <- 'na'
  if(length(idxTemp)==1){
    sameMsCallId <- callInfo_df$id[idxTemp]
  }
  #### Find the Other Margin Call in the Same Margin Statement END #####
  
  #### Calculate the Quantity Left of Each Asset Start #######
  for(i in 1:callNum){
    for(j in 1:resourceNum){
      currentResource_vec <- PasteResource(currentSelection_list[[callId_vec[i]]]$Asset,currentSelection_list[[callId_vec[i]]]$CustodianAccount)
      idxResource_vec <- which(currentResource_vec==resource_vec[j]) 
      if(length(idxResource_vec)!=0){
        quantityUsed_vec[j] <- quantityUsed_vec[j]+currentSelection_list[[callId_vec[i]]]$Quantity[idxResource_vec]/minUnit_vec[j]
      }
    }
  }
  quantityLeft_vec <- quantity_vec-quantityUsed_vec
  #### Calculate the Quantity Left of Each Asset END #########
  
  #### Find Resources Allocated to the Deselected Margin Statement Start ####
  resourceIndsCall_vec <- PasteResource(currentSelection_list[[dsCallId]]$Asset,currentSelection_list[[dsCallId]]$CustodianAccount)
  if(sameMsCallId!='na'){
    sameMsSelection_vec <- PasteResource(currentSelection_list[[sameMsCallId]]$Asset,currentSelection_list[[sameMsCallId]]$CustodianAccount)
    resourceInDeselectMs_vec <- unique(c(resourceIndsCall_vec,sameMsSelection_vec))
  } else{
    resourceInDeselectMs_vec <- resourceIndsCall_vec
  }
  
  assetQuanityAllocateddsCall_vec <- currentSelection_list[[dsCallId]]$Quantity
  #### Find Resources Allocated to the Deselected Margin Statement END ######
  
  #### Calucate the Insufficient Amount, Initiate the New Allocation Start ######
  #### calculate the sufficient amount & quantity of assets to fulfill the margin call
  callAmount <- callInfo_df$callAmount[which(callInfo_df$id==dsCallId)]
  
  allocationdsCall_df <- currentSelection_list[[dsCallId]]
  lackAmount <- callAmount- sum(allocationdsCall_df$`NetAmount(USD)`)  # the amount left needs to be fulfilled after deseleting one asset
  lackQuantity_vec <- ceiling(lackAmount/(1-haircut_vec)/minUnitValue_vec*FXRate_vec) # tempQuantity_vec needed for a single asset to fulfill each call
  # could either add the amount of the original selection or add another one or several assets.
  newAllocationdsCall_df <- allocationdsCall_df
  #### Calucate the Insufficient Amount, Initiate the New Allocation END ######
  
  
  #### Calculate the Objective Parameters Start ############ 
  
  ## calculate the cost if only the integral units of asset can be allocated
  integerAmount_vec <- ceiling(lackAmount/(1-haircut_vec)/minUnitValue_vec)*minUnitValue_vec
  cost_vec<-integerAmount_vec*costBasis_vec  # cost amount
  liquidity_vec <- (1-haircut_vec)^2                           # define asset liquidity
  
  ## normalization
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
  
  # calculate the overall(2 objs) scores of the assets
  optimal_vec <- normCost_vec*pref_vec[1]+normLiquidity_vec*pref_vec[2]
  names(optimal_vec) <- resource_vec
  optimal_vec <-sort(optimal_vec)  # sort the score of the assets, from the most to the least optimal
  idxOptimal_vec <- match(names(optimal_vec),resource_vec) # the index of the optimal_vec in the resource_vec

  #### Calculate the Objective Parameters END ############ 
  
  if(movementsLeft < 0){
    warning('Current number of movements is larger than the limit!')
  } else if(movementsLeft==0){
    warning('Allocating other assets will create more asset movements than limit!')
  } else if(movementsLeft==1){
    #### sort the optimal_vec
    ## order the optimal asset by Score 
    ## put the sufficient assets in front
    optimal_vec
    idxOptimal_vec
    idxSuff_vec <- which(quantityLeft_vec >= lackQuantity_vec)
    if(length(idxSuff_vec)==0){
      warning('Allocating other assets will create more asset movements than limit!')
    } else{ # at least one asset is sufficient
      suffResource_vec <- resource_vec[idxSuff_vec]
      insuffResource_vec <- resource_vec[-idxSuff_vec]
      #### sorting: 
      suffSort_vec <- sort(optimal_vec[match(suffResource_vec,names(optimal_vec))])
      insuffSort_vec <- sort(optimal_vec[match(insuffResource_vec,names(optimal_vec))])
      optimal_vec <- c(suffSort_vec,insuffSort_vec)
      idxOptimal_vec <- match(names(optimal_vec),resource_vec) # the index of the optimal_vec in the resource_vec
    }
  } else if(movementsLeft>=2){
    # other scenarios will have more possibilities
    # for simplicity, use the same order method as the 1 movement left scenario
    # but warning at the end
    suffResource_vec <- resource_vec[idxSuff_vec]
    insuffResource_vec <- ifelse(length(idxSuff_vec)==0,resource_vec,resource_vec[-idxSuff_vec])
    #### sorting: 
    suffSort_vec <- sort(optimal_vec[match(suffResource_vec,names(optimal_vec))])
    insuffSort_vec <- sort(optimal_vec[match(insuffResource_vec,names(optimal_vec))])
    optimal_vec <- c(suffSort_vec,insuffSort_vec)
    idxOptimal_vec <- match(names(optimal_vec),resource_vec) # the index of the optimal_vec in the resource_vec
  }



  
  # check whether the first optimal asset is already allocated to that margin statement
  # check whether the first optimal asset is enough to fulfill the margin call
  scenario <- 0
  for(i in idxOptimal_vec){
    resource <- resource_vec[i]
    assetId <- matrix(unlist(strsplit(resource,'-')),nrow=2)[1,]
    # create the new line
    lineAssetInfo_df <- assetInfo_df[which(assetInfo_df$id==assetId),]
    lineAvailAsset_df <- availAsset_df[which(availAsset_df$callId==dsCallId),]
    lineCallInfo_df <- callInfo_df[which(callInfo_df$id==dsCallId),]
    
    if(is.element(resource,resourceIndsCall_vec)){
      # if asset[i] is alreadly selected in the margin statement
      
      if(quantityLeft_vec[i]>=lackQuantity_vec[i]){
        # scenario 1: asset[i] is alreadly selected in the margin statement
        # & left quantity is larger than the insufficient quantity
        
        scenario <- 1
        idxResource_vec <- match(resource,resourceInDeselectMs_vec)
        addTempQuantity_vec <- lackQuantity_vec[i]*minUnit_vec[i]
        
        # update the arraies - quantityLeft_vec and quantityUsed_vec
        quantityLeft_vec[i] <- quantityLeft_vec[i]-addTempQuantity_vec[i]
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        
        addAmount <- addTempQuantity_vec*unitValue_vec[i]
        addAmountUSD <- addAmount/lineAssetInfo_df$FXRate
        addNetamountUSD <- addAmountUSD*(1-haircut_vec[i])
        addNetAmount <- addNetamountUSD*FXRate_vec[i]
  
        newQuantity <- newAllocationdsCall_df$Quantity[idxResource_vec]+addTempQuantity_vec
        newAmountUSD <- newAllocationdsCall_df$`Amount(USD)`[idxResource_vec]+addAmountUSD
        newAmount <- newAllocationdsCall_df$Amount[idxResource_vec]+addAmount
        newNetAmountUSD <- newAllocationdsCall_df$`NetAmount(USD)`[idxResource_vec]+ addNetamountUSD
        newNetAmount <- newAllocationdsCall_df$`NetAmount`[idxResource_vec]+ addNetAmount
        
        newAllocationdsCall_df$`NetAmount(USD)`<- newNetAmountUSD
        newAllocationdsCall_df$NetAmount<- newNetAmount
        newAllocationdsCall_df$`Amount(USD)` <- newAmountUSD
        newAllocationdsCall_df$Amount <- newAmount
        newAllocationdsCall_df$Quantity <- newQuantity
        
        # update the lackAmount and lackQuantity_vec
        lackAmount <- lackAmount-addNetamountUSD
        lackQuantity_vec[] <- 0
        
        break
      }else if(quantityLeft_vec[i] > 0){
        # scenario 2: asset[i] asset[i] is alreadly selected in the margin statement
        # & is less than the insufficient tempQuantity_vec but larger than 0
        scenario <- 2
        
        idxResource_vec <- match(resource,resourceInDeselectMs_vec)
        addTempQuantity_vec <- quantityLeft_vec[i]*minUnit_vec[i]
        quantityLeft_vec[i] <- quantityLeft_vec[i]-addTempQuantity_vec[i]
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        addAmount <- addTempQuantity_vec*unitValue_vec[i]
        addAmountUSD <- addAmount/lineAssetInfo_df$FXRate
        addNetamountUSD <- addAmountUSD*(1-haircut_vec[i])
        addNetAmount <- addNetamountUSD*FXRate_vec[i]
        
        newQuantity <- newAllocationdsCall_df$Quantity[idxResource_vec]+addTempQuantity_vec
        newAmountUSD <- newAllocationdsCall_df$`Amount(USD)`[idxResource_vec]+addAmountUSD
        newAmount <- newAllocationdsCall_df$Amount[idxResource_vec]+addAmount
        newNetAmountUSD <- newAllocationdsCall_df$`NetAmount(USD)`[idxResource_vec]+ addNetamountUSD
        newNetAmount <- newAllocationdsCall_df$`NetAmount`[idxResource_vec]+ addNetAmount
        
        newAllocationdsCall_df$`NetAmount(USD)`[idxResource_vec]<- round(newNetAmountUSD,2)
        newAllocationdsCall_df$NetAmount<- round(newNetAmount,2)
        newAllocationdsCall_df$`Amount(USD)`[idxResource_vec] <- round(newAmountUSD,2)
        newAllocationdsCall_df$Amount[idxResource_vec] <- round(newAmount,2)
        newAllocationdsCall_df$Quantity[idxResource_vec] <- round(newQuantity,2)
        
        # update the lackAmount and lackQuantity_vec
        lackAmount <- lackAmount-newNetAmountUSD
        lackQuantity_vec <- lackAmount/(1-haircut_vec)/minUnitValue_vec 
      }
    } else {
      # if asset[i] is not in the current selection for margin statement
      
      if(quantityLeft_vec[i]>=lackQuantity_vec[i]){
        # scenario 3: asset[i] is not in the current selection for margin statement,
        # & left quantity is larger than the insufficient quantity
        scenario <- 3
        
        newQuantity <- lackQuantity_vec[i] *minUnit_vec[i]
        quantityLeft_vec[i] <- quantityLeft_vec[i]-lackQuantity_vec[i]
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
  
        newAmount <- newQuantity*unitValue_vec[i]
        newAmountUSD <- newAmount/lineAssetInfo_df$FXRate
        newNetAmountUSD <- newAmountUSD*(1-haircut_vec[i])
        newNetAmount <- newNetAmountUSD*FXRate_vec[i]
        
        newVenue <- lineAvailAsset_df$venue[which(lineAvailAsset_df$assetCustacId==resource)]
        newCustodianAccount <- lineAvailAsset_df$CustodianAccount[which(lineAvailAsset_df$assetCustacId==resource)]
        
        newAsset_df <- data.frame(assetId,lineAssetInfo_df$name,newNetAmount,newNetAmountUSD,lineAssetInfo_df$FXRate,haircut_vec[i],newAmount,newAmountUSD,
                       lineAssetInfo_df$currency,newQuantity,newCustodianAccount,newVenue,lineCallInfo_df$marginType,deselectMs,dsCallId)
        newAllocationdsCall_df[length(allocationdsCall_df[,1])+1,]<- newAsset_df
        
        # update the lackAmount and lackQuantity_vec
        lackAmount <- lackAmount-newNetAmountUSD
        lackQuantity_vec[] <- 0        
        break
      } else if(quantityLeft_vec[i] > 0){
        # scenario 4: asset[i] is not in the current selection for margin statement
        # & left tempQuantity_vec is less than the insufficient tempQuantity_vec but larger than 0
        scenario <- 4
        
        newQuantity <- quantityLeft_vec[i]*minUnit_vec[i]
        quantityLeft_vec[i] <- 0
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        
        newAmount <- newQuantity*unitValue_vec[i]
        newAmountUSD <- newAmount/lineAssetInfo_df$FXRate
        newNetAmountUSD <- newAmountUSD*(1-haircut_vec[i])
        newNetAmount <- newNetAmountUSD*FXRate_vec[i]
        
        newVenue <- lineAvailAsset_df$venue[which(lineAvailAsset_df$assetCustacId==resource)]
        newCustodianAccount <- lineAvailAsset_df$CustodianAccount[which(lineAvailAsset_df$assetCustacId==resource)]
        
        newAsset_df <- data.frame(assetId,lineAssetInfo_df$name,newNetAmount,newNetAmountUSD,lineAssetInfo_df$FXRate,haircut_vec[i],newAmount,newAmountUSD,
                       lineAssetInfo_df$currency,newQuantity,newCustodianAccount,newVenue,lineCallInfo_df$marginType,deselectMs,dsCallId)
        newAllocationdsCall_df[length(allocationdsCall_df[,1])+1,]<- newAsset_df
        
        # update the lackAmount and lackQuantity_vec
        lackAmount <- lackAmount-newNetAmountUSD
        lackQuantity_vec <- lackAmount/(1-haircut_vec)/minUnitValue_vec 
      }
    }
  }
  #cat('scenario: ',scenario,'\n')
  #cat('asset: ',i,'  ',resource,'\n')
  
  currentSelection_list[[dsCallId]]<- newAllocationdsCall_df
  
  #### Update Result Analysis Output Start #### OW-560 ###############
  coreInputTotal_list <- AllocationInputData(callId_vec,resourceTotal_vec,callInfo_df,availAssetTotal_df,assetInfoTotal_df)
  
  #### Cost
  minUnitTotal_mat <- coreInputTotal_list$minUnit_mat
  minUnitValueTotal_mat <- coreInputTotal_list$minUnitValue_mat
  costBasisTotal_mat <- coreInputTotal_list$cost_mat
  
  #### allocation presented in minUnitQuatity matrix
  result_mat <- ResultList2Mat(currentSelection_list,callId_vec,resourceTotal_vec,minUnitTotal_mat)
  idxEli_vec <- which(coreInputTotal_list$eli_mat==1)

  dailyCost <- CostFun(result_mat[idxEli_vec]*minUnitValueTotal_mat[idxEli_vec],costBasisTotal_mat[idxEli_vec])
  monthlyCost <- dailyCost*30
  dailyCost <- round(dailyCost,2)
  monthlyCost <- round(monthlyCost,2)
  
  #### Liquidity
  
  #reservedLiquidityRatio <- LiquidFun(quantityLeft_vec,quantity_vec,liquidity_vec,minUnitValue_vec)
  reservedLiquidityRatio <-0
  
  #### Movements
  movements <- OperationFun(currentSelection_list,callInfo_df,'callList')
  if(movements > operLimit){
    warning('Allocating other assets will create more asset movements than limit!')
  }
  resultAnalysis <- list(dailyCost=dailyCost,monthlyCost=monthlyCost,movements=movements,reservedLiquidityRatio=reservedLiquidityRatio)
  #### Update Result Analysis Output END ##### OW-560 ################
  
  output_list <- list(newSuggestion=currentSelection_list,resultAnalysis=resultAnalysis)
  return(output_list)
}




