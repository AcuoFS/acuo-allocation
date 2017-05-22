
SecondAllocationAlgoAllMsV2<- function(callId_vec,callInfo_df,resourceTotal_vec,availAssetTotal_df,resourceTotal_df,
                                       dsAssetId,dsCallId_vec,currentSelection_list,
                                       pref_vec,operLimit,operLimitMs_vec,fungible){
  if(0){
    #### basic model Start #######
    for(i in 1:length(dsCallId_vec)){
      dsCallId <- dsCallId_vec[i]
      
      #### Remove the Deselected Asset Start
      #### Only If the Asset is in the Selection
      idxTemp <- which(currentSelection_list[[dsCallId]]$Asset==dsAssetId)
      if(length(idxTemp)>=1){
        currentSelection_list[[dsCallId]] <- currentSelection_list[[dsCallId]][-idxTemp,]
        
        #### Update the Quantity of Resource Start #####
        #### restore the quantity of resources
        resourceTotal_df <- ResetQtyMinInResourceDf(resourceTotal_df)
        
        #### Get minUnit from assetInfo
        tempResult <- SecondAllocationAlgoV2(callId_vec,callInfo_df,resourceTotal_vec,availAssetTotal_df,resourceTotal_df,
                                             dsAssetId,dsCallId,currentSelection_list,pref_vec,operLimit,operLimitMs_vec,fungible)
        
        currentSelection_list <- tempResult$newSuggestion
        resultAnalysis <- tempResult$resultAnalysis
      }
    }
    #### basic model END #########
  }
  
  #### Advanced model Start ####
  
  ## 0. Temporarily remove the deselect assets of calls from the availAsset 
  idxRm_vec <- which(SplitResource(availAsset_df$assetCustacId,'asset')==dsAssetId & availAsset_df$callId %in% dsCallId_vec)
  if(length(idxRm_vec)>=1){
    availAssetDs_df <- availAsset_df[-idxRm_vec,]
  }
  ## keep only the availAsset for deselect calls
  availAssetDs_df <- availAssetDs_df[which(availAssetDs_df$callId %in% dsCallId_vec),]
  resourceDs_vec <- unique(availAssetDs_df$assetCustacId)
  resourceDs_df <- resource_df[match(resourceDs_vec,resource_df$id),]
  
  ## 1. Temporarily deduct the quantity used from current selection, excluding the deselected assets 
  for(dsCall in dsCallId_vec){
    idxTemp <- which(currentSelection_list[[dsCall]]$Asset==dsAssetId)
    if(length(idxTemp)>=1){
      currentSelection_list[[dsCall]] <- currentSelection_list[[dsCall]][-idxTemp,]
    }
  }
  
  quantityUsedDs_vec <- UsedQtyFromResultList(currentSelection_list,resourceDs_vec,callId_vec)
  resourceDs_df$qtyMin <- resourceDs_df$qtyMin - quantityUsedDs_vec
  
  ## 2. Deduct the amount already fulfilled for each dsCall after removing the deselected assets
  callInfoDs_df <- callInfo_df[match(dsCallId_vec,callInfo_df$id),]
  idxTemp_vec <- match(dsCallId_vec,names(currentSelection_list))
  currentSelectionDs_list <- currentSelection_list[idxTemp_vec]
  
  lackAmount_vec <- rep(0,length(dsCallId_vec))
  for(i in 1:length(dsCallId_vec)){
    dsCallId <- dsCallId_vec[i]
    alloc_df <- currentSelectionDs_list[[dsCallId]]
    lackAmount_vec[i] <- callInfoDs_df$callAmount[i] - sum(alloc_df$`NetAmount(USD)`)
  }
  
  callInfoDs_df$callAmount <- lackAmount_vec
  
  ## 3. Deduct the movements already used
  ##    a. if fungible = TRUE, can use the left movements from other calls
  if(fungible){
    movementsAll <- OperationFun(currentSelection_list,callInfo_df,'callList')
    movementsLeft <- operLimit - movementsAll
  } else{
    ##    b. if fungible = FALSE, can only use the left movements from deselect calls
    msId_vec <- unique(callInfo_df$marginStatement)
    msDsId_vec <- unique(callInfoDs_df$marginStatement)
    
    msNum <- length(msId_vec)
    msDsNum <- length(msDsId_vec)
    
    movementsDs <- OperationFun(currentSelectionDs_list,callInfo_df,'callList')
    operLimitDs <- operLimit*msDsNum/msNum
    
    movementsLeft <- operLimitDs - movementsDs
    movementsMsLeft_vec <- rep(0,msDsNum)
    for(i in 1:msDsNum){
      msId <- msDsId_vec[i]
      CallInThisMs_vec <- callInfoDs_df$id[which(callInfoDs_df$marginStatement==msId)]
      idxCall_vec <- match(CallInThisMs_vec,names(currentSelectionDs_list))
      movementsMsLeft_vec[i] <- operLimitMs_vec[i]- OperationFun(currentSelectionDs_list[idxCall_vec],callInfoDs_df,'callList')
    }
  }
  
  
  ## 4. Assign 0 to the dummy variable if already allocated
  ##    a. an indicator vector for variable already allocated
  ##    allocated_vec: (varNum2-varNum), 1 if allocated
  ##    b. a new DummyConst() function 
  
  ## 5. Run the Algo with the new constraints
  newResult<- CallAllocation(algoVersion,scenario=1,dsCallId_vec,resourceDs_vec,callInfoDs_df,availAssetDs_df,resourceDs_df,
                             pref_vec,movementsLeft,movementsMsLeft_vec,fungible,
                             ifNewAlloc=F,currentSelectionDs_list)
  resultAnalysis <- newResult$resultAnalysis
  newSelection_list <- newResult$callOutput
  
  ## 6. Combine with the current allocation
  for(i in 1:length(callId_vec)){
    callId <- callId_vec[i]
    current_df <- currentSelection_list[[callId]]
    new_df <- newSelection_list[[callId]]
    currentResource_vec <- PasteResource(current_df$Asset,current_df$CustodianAccount)
    newResource_vec <- PasteResource(new_df$Asset,new_df$CustodianAccount)
    idxSame_vec <- intersect(currentResource_vec,newResource_vec)
    if(length(idxSame_vec)>=1){
      idx1 <- match(idxSame_vec,currentResource_vec)
      idx2 <- match(idxSame_vec,newResource_vec)
      newCurrent_df <- current_df
      cols <- c('NetAmount','NetAmount(USD)','Amount','Amount(USD)','Quantity','Cost')
      idxTemp_vec <- match(cols,colnames(newCurrent_df))
      newCurrent_df[idx1,idxTemp_vec] <- current_df[idx1,idxTemp_vec]+new_df[idx2,idxTemp_vec]
      newNew_df <- new_df[-idx2,]
      alloc_df <- rbind(newCurrent_df,newNew_df)
    } else{
      alloc_df <- rbind(current_df,new_df)
    }
    currentSelection_list[[callId]] <- alloc_df
  }
  
  ## 7. Result analysis
  
  availInfo_list <- AssetByCallInfo(callId_vec,resource_vec,availAssetTotal_df)
  
  eli_mat <- availInfo_list$eli_mat; 
  eli_vec <-  as.vector(t(eli_mat)) 
  idxEli_vec <- which(eli_vec==1)     
  
  cost_mat <- availInfo_list$cost_mat
  cost_vec <- as.vector(t(cost_mat))[idxEli_vec]
  
  varInfo_list <- VarInfo(eli_vec,callInfo_df,resource_vec,callId_vec)
  varName_vec <- varInfo_list$varName_vec; 
  varNum <- varInfo_list$varNum
  varAmount_vec <- ResultList2AmountVec(currentSelection_list,callId_vec,varName_vec[1:varNum])
  
  #### Costs
  dailyCost <- CostFun(varAmount_vec,cost_vec)
  monthlyCost <- dailyCost*30
  dailyCost <- round(dailyCost,2)
  monthlyCost <- round(monthlyCost,2)
  
  #### Movements
  varAmount_mat <- VarVec2mat(varAmount_vec[1:varNum],varName_vec[1:varNum],callId_vec,resource_vec)
  movements <- OperationFun(varAmount_mat,callInfo_df,'matrix')
  
  #### Liquidity
  liquidity_vec <- apply((1-availInfo_list$haircut_mat)^2,2,min)
  
  qtyUsed <- UsedQtyFromResultList(currentSelection_list,resource_vec,callId_vec)
  qtyLeft <- resource_df$qtyMin - qtyUsed/resource_df$minUnit
  
  reservedLiquidityRatio <- LiquidFun(qtyLeft,resource_df$qtyMin,liquidity_vec,resource_df$minUnitValue/resource_df$FXRate)
  
  resultAnalysis <- list(dailyCost=dailyCost,monthlyCost=monthlyCost,movements=movements,reservedLiquidityRatio=reservedLiquidityRatio)
  
  #### Advanced model END ######
  
  #### quantity left
  quantityTotalLeft_vec <- resourceTotal_df$qtyMin
  
  output_list <- list(callOutput=currentSelection_list,resultAnalysis=resultAnalysis,
                      quantityTotalLeft_vec=quantityTotalLeft_vec)
  return(output_list)
}



SecondAllocationAlgoV2<- function(callId_vec,callInfo_df,resourceTotal_vec,
                                  availAssetTotal_df, resourceTotal_df, 
                                  dsAssetId,dsCallId,currentSelection_list,
                                  pref_vec,operLimit,operLimitMs,fungible){
  availAssetOri_df <- availAssetTotal_df
  
  #### Get minUnit from assetInfo
  quantityTotalUsed_vec <- UsedQtyFromResultList(currentSelection_list,resourceTotal_vec,callId_vec)
  resourceTotal_df$qtyMin <- resourceTotal_df$qtyMin - quantityTotalUsed_vec
  
  #### Remove Deselect Asset in The AvailAsset_df For Testing 
  rmRow_vec <- which(availAssetTotal_df$callId==dsCallId & availAssetTotal_df$assetId==dsAssetId)
  if(length(rmRow_vec)>=1){
    availAssetTotal_df <- availAssetTotal_df[-rmRow_vec,]
  }
  #### Update the Quantity of Resource END #######
  
  
  
  #### Prepare Inputs Start #####
  ## derive the availAsset_df for deselected call
  ## derive the resource_vec for deselected call
  ## derive the assetInfo_df for deselected call
  availAsset_df <- availAssetTotal_df[which(availAssetTotal_df$callId==dsCallId),]  
  resource_vec <- unique(availAsset_df$assetCustacId)
  resource_df <- resourceTotal_df[match(resource_vec,resourceTotal_df$id),]
  availInfo_list <- AssetByCallInfo(dsCallId,resource_vec,availAsset_df)
  
  pref_vec <- pref_vec/sum(pref_vec[1:2])
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  dsCallInfo <- callInfo_df[which(callInfo_df$id==dsCallId),]
  
  eli_mat <- availInfo_list$eli_mat; 
  eli_vec <-  as.vector(t(eli_mat)) # eligibility matrix & vector
  idxEli_vec <- which(eli_vec==1)         # eligible indext
  
  haircut_mat<-availInfo_list$haircut_mat; 
  haircut_vec <- as.vector(t(haircut_mat))[idxEli_vec]      # haircut mat & vec
  
  #### Persist the Quantity Used in Algo
  quantity_vec <- resource_df$qtyMin
  
  unitValue_vec <- resource_df$unitValue/resource_df$FXRate
  
  minUnit_vec <- resource_df$minUnit
  
  minUnitValue_vec <- unitValue_vec*minUnit_vec
  
  callAmount_vec <- rep(dsCallInfo$callAmount,resourceNum)
  
  
  costBasis_mat <- availInfo_list$cost_mat  
  costBasis_vec <- as.vector(t(costBasis_mat))[idxEli_vec]
  
  
  #### Prepare Inputs Start #####
  
  #### Calculate the Current Movements Start #########
  movementsUsed <- OperationFun(currentSelection_list,callInfo_df,'callList')
  movementsLeft <- operLimit- movementsUsed
  
  dsMsId <- callInfo_df$marginStatement[which(callInfo_df$id==dsCallId)]
  allCallInDsMs_vec <- callInfo_df$id[which(callInfo_df$marginStatement==dsMsId)]
  
  idxTemp1 <- which(names(currentSelection_list) %in% allCallInDsMs_vec)
  idxTemp2 <- which(callInfo_df$id %in% allCallInDsMs_vec)
  
  
  movementsUsedMs <- OperationFun(currentSelection_list[idxTemp1],callInfo_df[idxTemp2,],'callList')
  movementsLeftMs <- operLimitMs- movementsUsedMs
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
  #quantityUsed_vec <- UsedQtyFromResultList(currentSelection_list,resource_vec,callId_vec)
  quantityLeft_vec <- quantity_vec #-quantityUsed_vec
  quantityUsed_vec <- rep(0,length(quantity_vec))
  #### Calculate the Quantity Left of Each Asset END #########
  
  #### Find Resources Allocated to the Deselected Margin Statement Start ####
  resourceInDsCall_vec <- PasteResource(currentSelection_list[[dsCallId]]$Asset,currentSelection_list[[dsCallId]]$CustodianAccount)
  
  if(length(resourceInDsCall_vec)==0){
    resourceInDsCall_vec <- 'na'
  }
  if(sameMsCallId!='na'){
    sameMsSelection_vec <- PasteResource(currentSelection_list[[sameMsCallId]]$Asset,currentSelection_list[[sameMsCallId]]$CustodianAccount)
    resourceInDeselectMs_vec <- unique(c(resourceInDsCall_vec,sameMsSelection_vec))
  } else{
    resourceInDeselectMs_vec <- resourceInDsCall_vec
  }
  
  assetQuanityAllocateddsCall_vec <- currentSelection_list[[dsCallId]]$Quantity
  #### Find Resources Allocated to the Deselected Margin Statement END ######
  
  #### Calucate the Insufficient Amount, Initiate the New Allocation Start ######
  #### calculate the sufficient amount & quantity of assets to fulfill the margin call
  callAmount <- callInfo_df$callAmount[which(callInfo_df$id==dsCallId)]
  
  allocationDsCall_df <- currentSelection_list[[dsCallId]]
  lackAmount <- callAmount- sum(allocationDsCall_df$`NetAmount(USD)`)  # the amount left needs to be fulfilled after deseleting one asset
  lackQuantity_vec <- ceiling(lackAmount/(1-haircut_vec)/minUnitValue_vec) # tempQuantity_vec needed for a single asset to fulfill each call
  # could either add the amount of the original selection or add another one or several assets.
  newAllocationDsCall_df <- allocationDsCall_df
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
  
  #### Check the Movements Limit Start #####
  if(movementsLeft < 0){
    warning('Current number of movements is larger than the limit!')
  } else if(movementsLeft==0){
    warning('Allocating other assets may create more asset movements than limit, thus need reallocation of entire margin statement!')
  } else if(movementsLeft==1){
    #### sort the optimal_vec
    ## order the optimal asset by Score
    ## put the sufficient assets in front
    idxSuff_vec <- which(quantityLeft_vec >= lackQuantity_vec)
    if(length(idxSuff_vec)==0){
      warning('Allocating other assets may create more asset movements than limit, thus need reallocation of entire margin statement!')
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
    idxSuff_vec <- which(quantityLeft_vec >= lackQuantity_vec)
    suffResource_vec <- resource_vec[idxSuff_vec]
    insuffResource_vec <- ifelse(length(idxSuff_vec)==0,resource_vec,resource_vec[-idxSuff_vec])
    #### sorting:
    suffSort_vec <- sort(optimal_vec[match(suffResource_vec,names(optimal_vec))])
    insuffSort_vec <- sort(optimal_vec[match(insuffResource_vec,names(optimal_vec))])
    optimal_vec <- c(suffSort_vec,insuffSort_vec)
    idxOptimal_vec <- match(names(optimal_vec),resource_vec) # the index of the optimal_vec in the resource_vec
  }
  #### Check the Movements Limit END #######
  
  #### Alternative Assets Selection Start ####
  # check whether the first optimal asset is already allocated to that margin statement
  # check whether the first optimal asset is enough to fulfill the margin call
  scenario <- 0
  for(i in idxOptimal_vec){
    resource <- resource_vec[i]
    # create the new line
    lineResource_df <- resource_df[which(resource_df$id==resource),]
    lineAvailAsset_df <- availAsset_df[which(availAsset_df$callId==dsCallId),]
    lineCallInfo_df <- callInfo_df[which(callInfo_df$id==dsCallId),]
    if(is.element(resource,resourceInDsCall_vec) & resourceInDsCall_vec!='na'){
      # if asset[i] is alreadly selected in the margin statement
      if(quantityLeft_vec[i]>=lackQuantity_vec[i]){
        # scenario 1: asset[i] is alreadly selected in the margin statement
        # & left quantity is larger than the insufficient quantity
        scenario <- 1
        idxResource_vec <- match(resource,resourceInDeselectMs_vec)
        addTempQuantity_vec <- lackQuantity_vec[i]
        
        # update the arraies - quantityLeft_vec and quantityUsed_vec
        quantityLeft_vec[i] <- quantityLeft_vec[i]-addTempQuantity_vec
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        addAmountUSD <- addTempQuantity_vec*minUnitValue_vec[i]
        addAmount <- addAmount*lineResource_df$FXRate
        addNetamountUSD <- addAmountUSD*(1-haircut_vec[i])
        addNetAmount <- addNetamountUSD*lineResource_df$FXRate
        
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
        # scenario 2: asset[i] asset[i] is alreadly selected in the margin statement
        # & is less than the insufficient tempQuantity_vec but larger than 0
        scenario <- 2
        
        idxResource_vec <- match(resource,resourceInDeselectMs_vec)
        addTempQuantity_vec <- quantityLeft_vec[i]
        quantityLeft_vec[i] <- quantityLeft_vec[i]-addTempQuantity_vec
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        addAmountUSD <- addTempQuantity_vec*minUnitValue_vec[i]
        addAmount<- addAmount*lineResource_df$FXRate
        addNetamountUSD <- addAmountUSD*(1-haircut_vec[i])
        addNetAmount <- addNetamountUSD*lineResource_df$FXRate
        
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
        # scenario 3: asset[i] is not in the current selection for margin statement,
        # & left quantity is larger than the insufficient quantity
        scenario <- 3
        
        newQuantity <- lackQuantity_vec[i]
        quantityLeft_vec[i] <- quantityLeft_vec[i]-lackQuantity_vec[i]
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        
        newAmountUSD <- newQuantity*minUnitValue_vec[i]
        newAmount <- newAmountUSD*lineResource_df$FXRate
        newNetAmountUSD <- newAmountUSD*(1-haircut_vec[i])
        newNetAmount <- newNetAmountUSD*lineResource_df$FXRate
        
        newAsset_df <- data.frame(lineResource_df$assetId,lineResource_df$assetName,newNetAmount,newNetAmountUSD,lineResource_df$FXRate,haircut_vec[i],newAmount,newAmountUSD,
                                  lineResource_df$currency,newQuantity,lineResource_df$custodianAccount,lineResource_df$venue,lineCallInfo_df$marginType,deselectMs,dsCallId)
        newAllocationDsCall_df[length(newAllocationDsCall_df[,1])+1,]<- newAsset_df
        
        # update the lackAmount and lackQuantity_vec
        lackAmount <- lackAmount-newNetAmountUSD
        lackQuantity_vec[] <- 0
        break
      } else if(quantityLeft_vec[i] > 0){
        # scenario 4: asset[i] is not in the current selection for margin statement
        # & left tempQuantity_vec is less than the insufficient tempQuantity_vec but larger than 0
        scenario <- 4
        
        newQuantity <- quantityLeft_vec[i]
        quantityLeft_vec[i] <- 0
        quantityUsed_vec[i] <- quantity_vec[i] -quantityLeft_vec[i]
        
        newAmountUSD <- newQuantity*minUnitValue_vec[i]
        newAmount <- newAmountUSD*lineResource_df$FXRate
        newNetAmountUSD <- newAmountUSD*(1-haircut_vec[i])
        newNetAmount <- newNetAmountUSD*lineResource_df$FXRate
        
        newAsset_df <- data.frame(lineResource_df$assetId,lineResource_df$assetName,newNetAmount,newNetAmountUSD,lineResource_df$FXRate,haircut_vec[i],newAmount,newAmountUSD,
                                  lineResource_df$currency,newQuantity,lineResource_df$custodianAccount,lineResource_df$venue,lineCallInfo_df$marginType,deselectMs,dsCallId)
        newAllocationDsCall_df[length(newAllocationDsCall_df[,1])+1,]<- newAsset_df
        
        # update the lackAmount and lackQuantity_vec
        lackAmount <- lackAmount-newNetAmountUSD
        lackQuantity_vec <- lackAmount/(1-haircut_vec)/minUnitValue_vec
      }
    }
  }
  #cat('scenario: ',scenario,'\n')
  #cat('asset: ',i,'  ',resource,'\n')
  
  #### Re-allocation Start #############################
  #### After allocation, if violate the constraint limit, then reallocate the entire statement.
  #### call first level algo ####
  #### only re-allocate the deselect from margin statement
  #### before re-allocate, update the quantity
  newSelection_list <- currentSelection_list
  newSelection_list[[dsCallId]]<- newAllocationDsCall_df
  
  idxTemp1 <- which(names(newSelection_list) %in% allCallInDsMs_vec)
  idxTemp2 <- which(callInfo_df$id %in% allCallInDsMs_vec)
  movementsUsedMs <- OperationFun(newSelection_list[idxTemp1],callInfo_df[idxTemp2,],'callList')
  movementsUsedAll <- OperationFun(newSelection_list,callInfo_df,'callList')
  
  #### get the current allocation besides the MS
  currentSelectionOther_list <- list()
  otherCall_vec <- callId_vec[-idxTemp2]
  for(call in otherCall_vec){
    currentSelectionOther_list[[call]] <- newSelection_list[[call]]
  }
  #currentSelectionOther_list <- newSelection_list
  #for(i in 1:length(allCallInDsMs_vec)){
  #  tempCall <- allCallInDsMs_vec[i]
  #  currentSelectionOther_list[[tempCall]] <- currentSelectionOther_list[[tempCall]][0,]
  #}
  availAssetMsOri_df <- availAssetTotal_df[which(availAssetTotal_df$callId %in% allCallInDsMs_vec),]
  resourceMs_vec <- unique(availAssetMsOri_df$assetCustacId)
  assetIdMs_vec <- SplitResource(resourceMs_vec,'asset')
  resourceMs_df <- resourceTotal_df[match(resourceMs_vec,resourceTotal_df$id),]
  callInfoMs_df <- callInfo_df[which(callInfo_df$id %in% allCallInDsMs_vec),]
  
  quantityUsedOther_vec <- UsedQtyFromResultList(currentSelectionOther_list,resourceMs_vec,otherCall_vec)
  ### review...
  #### restore before deducting
  resourceMs_df <- ResetQtyMinInResourceDf(resourceMs_df)
  resourceMs_df$qtyMin <- resourceMs_df$qtyMin - quantityUsedOther_vec
  
  #### remove the lines with deselect asset & call
  availAssetMs_df <- availAssetMsOri_df[which(availAssetMsOri_df$callId %in% allCallInDsMs_vec),]
  rmRow_vec <- which(availAssetMs_df$callId==dsCallId & availAssetMs_df$assetId==dsAssetId)
  if(length(rmRow_vec)>=1){
    availAssetMs_df <- availAssetMs_df[-rmRow_vec,]
  }
  
  availInfoMs_list <- AssetByCallInfo(allCallInDsMs_vec,resourceMs_vec,availAssetMs_df)
  
  
  if(movementsUsedMs>operLimitMs & fungible==F){
    newMsResult_list <- CoreAlgoV2(callInfoMs_df, resourceMs_df, availInfoMs_list,timeLimit=5,pref_vec,operLimitMs,operLimitMs,fungible,minMoveValue=1000)
    newSelection_list[idxTemp1] <- newMsResult_list$callOutput_list
  } else if(movementsUsedAll>operLimit & fungible==T){
    operLimitMsLeft <- operLimit-(movementsUsedAll-movementsUsedMs)
    if(operLimitMsLeft<=0){
      stop('Movements for other margin statements exceed the total limits!')
    }
    newMsResult_list <- CoreAlgoV2(callInfoMs_df,resourceMs_df,availAssetMs_df,timeLimit=5,pref_vec,operLimitMsLeft,operLimitMs,fungible,minMoveValue=1000)
    newSelection_list[idxTemp1] <- newMsResult_list$callOutput_list
  }
  #### Re-allocation END ###############################
  
  currentSelection_list <-newSelection_list 
  
  
  #### Alternative Assets Selection END ######
  
  #### Update Result Analysis Output Start #### OW-560 ###############
  
  
  availInfoTotal_list <- AssetByCallInfo(callId_vec,resourceTotal_vec,availAssetTotal_df)
  eli_mat <- availInfoTotal_list$eli_mat; 
  eli_vec <-  as.vector(t(eli_mat)) # eligibility matrix & vector
  idxEli_vec <- which(eli_vec==1)         # eligible indext
  
  cost_mat <- availInfoTotal_list$cost_mat
  cost_vec <- as.vector(t(cost_mat))[idxEli_vec]
  
  varInfo_list <- VarInfo(eli_vec,callInfo_df,resourceTotal_vec,callId_vec)
  varName_vec <- varInfo_list$varName_vec; 
  varNum <- varInfo_list$varNum
  
  
  
  #### Cost
  unitValueTotal_mat<- matrix(rep(resourceTotal_df$unitValue/resourceTotal_df$FXRate, callNum),nrow=callNum,byrow=T)
  unitValueTotal_vec <- as.vector(t(unitValueTotal_mat))[idxEli_vec]
  
  minUnitTotal_mat <- matrix(rep(resourceTotal_df$minUnit,callNum),nrow=callNum,byrow=T); 
  minUnitTotal_vec <- as.vector(t(minUnitTotal_mat))[idxEli_vec]
  
  minUnitValueTotal_mat <- unitValueTotal_mat*minUnitTotal_mat;
  minUnitValueTotal_vec <- as.vector(t(minUnitValueTotal_mat))[idxEli_vec]
  
  
  #### allocation presented in minUnitQuatity matrix
  
  varAmount_vec <- ResultList2AmountVec(currentSelection_list,callId_vec,varName_vec[1:varNum])
  
  dailyCost <- CostFun(varAmount_vec,cost_vec)
  monthlyCost <- dailyCost*30
  dailyCost <- round(dailyCost,2)
  monthlyCost <- round(monthlyCost,2)
  
  #### Liquidity
  resourceTotal_df <- ResetQtyMinInResourceDf(resourceTotal_df)
  quantityTotal_vec <- resourceTotal_df$qtyMin
  
  quantityRes_vec <- quantityTotal_vec
  idxTemp_vec <-match(resource_vec,resourceTotal_vec)
  quantityRes_vec[idxTemp_vec] <- resource_df$qtyMin
  
  liquidity_vec <- apply((1-availInfoTotal_list$haircut_mat)^2,2,min)
  
  minUnitValueTemp_vec <- resourceTotal_df$unitValue/resourceTotal_df$minUnit/resourceTotal_df$FXRate
  reservedLiquidityRatio <- LiquidFun(quantityRes_vec,quantityTotal_vec,liquidity_vec,minUnitValueTemp_vec)
  
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

