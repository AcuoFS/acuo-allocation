
SecondAllocationV2<- function(callId_vec,callInfo_df,resourceTotal_vec,availAssetTotal_df,resourceTotal_df,
                              dsAssetId,dsCallId_vec,currentSelection_list,
                              pref_vec,operLimit,operLimitMs_vec,fungible){
  
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
  ## a. Get the insufficient amount vector 
  callInfoDs_df <- callInfo_df[match(dsCallId_vec,callInfo_df$id),]
  
  lackAmount_vec <- rep(0,length(dsCallId_vec))
  for(i in 1:length(dsCallId_vec)){
    dsCallId <- dsCallId_vec[i]
    alloc_df <- currentSelection_list[[dsCallId]]
    lackAmount_vec[i] <- callInfoDs_df$callAmount[i] - sum(alloc_df$`NetAmount(USD)`)
  }
  
  ## b. Remove the calls which already fulfilled
  #### delete from dsCallId_vec if correspond lackAmount is nagative
  idxSuff_vec <- which(lackAmount_vec <= 0)
  if(length(idxSuff_vec)>=1){
    callInfoDs_df$callAmount <- lackAmount_vec
    dsCallId_vec <- dsCallId_vec[-idxSuff_vec]
    callInfoDs_df <- callInfoDs_df[-idxSuff_vec,]
    availAssetDs_df <- availAssetDs_df[which(availAssetDs_df$callId %in% dsCallId_vec),] 
    resourceDs_vec <- unique(availAssetDs_df$assetCustacId)
    resourceDs_df <- resourceDs_df[match(resourceDs_vec,resourceDs_df$id),]
  }
  
  idxTemp_vec <- match(dsCallId_vec,names(currentSelection_list))
  currentSelectionDs_list <- currentSelection_list[idxTemp_vec]
  
  #### if there are still some calls haven't been fulfilled
  if(length(dsCallId_vec)>=1){
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
  }
  
  #### Advanced model END ######
  
  #### Result analysis Start #########
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
  #### Result analysis END ###########
  
  output_list <- list(callOutput=currentSelection_list,resultAnalysis=resultAnalysis)
  return(output_list)
}

SecondAllocationBasicV2<- function(callId_vec,callInfo_df,resourceTotal_vec,availAssetTotal_df,resourceTotal_df,
                                   dsAssetId,dsCallId_vec,currentSelection_list,
                                   pref_vec,operLimit,operLimitMs_vec,fungible){
  
  #### Basic model Start ####
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
      tempResult <- SecondAllocationOneCallBasicV2(callId_vec,callInfo_df,resourceTotal_vec,availAssetTotal_df,resourceTotal_df,
                                                   dsAssetId,dsCallId,currentSelection_list,pref_vec,operLimit,operLimitMs_vec[1],fungible)
      
      currentSelection_list <- tempResult$callOutput
    }
  }
  #### Basic model END ######
  
  #### Result analysis Start #########
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
  #### Result analysis END ###########
  
  output_list <- list(callOutput=currentSelection_list,resultAnalysis=resultAnalysis)
  return(output_list)
}

SecondAllocationOneCallBasicV2<- function(callId_vec,callInfo_df,resourceTotal_vec,
                                          availAssetTotal_df, resourceTotal_df, 
                                          dsAssetId,dsCallId,currentSelection_list,
                                          pref_vec,operLimit,operLimitMs,fungible){
  #### keep the original availAssetTotal_df
  availAssetOri_df <- availAssetTotal_df
  
  #### Remove the Deselected Asset Start
  #### Only If the Asset is in the Selection
  idxTemp <- which(currentSelection_list[[dsCallId]]$Asset==dsAssetId)
  if(length(idxTemp)>=1){
    currentSelection_list[[dsCallId]] <- currentSelection_list[[dsCallId]][-idxTemp,]
  }
  
  #### Get minUnit from assetInfo
  quantityTotalUsed_vec <- UsedQtyFromResultList(currentSelection_list,resourceTotal_vec,callId_vec)
  resourceTotal_df$qtyMin <- resourceTotal_df$qtyMin - quantityTotalUsed_vec
  
  #### Remove Deselect Asset in The AvailAsset_df For Testing 
  assetTemp_vec <- SplitResource(availAssetTotal_df$assetCustacId,'asset')
  rmRow_vec <- which(availAssetTotal_df$callId==dsCallId & assetTemp_vec==dsAssetId)
  if(length(rmRow_vec)>=1){
    availAssetTotal_df <- availAssetTotal_df[-rmRow_vec,]
  }
  #### Update the Quantity of Resource END #######
  
  
  
  #### Prepare Inputs Start #####
  ## derive the availAsset_df for deselected call
  ## derive the resource_vec for deselected call
  ## derive the assetInfo_df for deselected call
  availAssetDs_df <- availAssetTotal_df[which(availAssetTotal_df$callId==dsCallId),]  
  dsCallInfo <- callInfo_df[which(callInfo_df$id==dsCallId),]
  resourceDs_vec <- unique(availAssetDs_df$assetCustacId)
  resourceDs_df <- resourceTotal_df[match(resourceDs_vec,resourceTotal_df$id),]
  availInfoDs_list <- AssetByCallInfo(dsCallId,resourceDs_vec,availAssetDs_df)
  
  pref_vec <- pref_vec/sum(pref_vec[1:2])
  resourceDsNum <- length(resourceDs_vec)
  callNum <- length(callId_vec)
  
  eli_mat <- availInfoDs_list$eli_mat; 
  eli_vec <-  as.vector(t(eli_mat)) # eligibility matrix & vector
  idxEli_vec <- which(eli_vec==1)         # eligible indext
  
  haircut_mat<-availInfoDs_list$haircut_mat; 
  haircut_vec <- as.vector(t(haircut_mat))[idxEli_vec]      # haircut mat & vec
  
  #### Persist the Quantity Used in Algo
  quantity_vec <- resourceDs_df$qtyMin
  
  unitValue_vec <- resourceDs_df$unitValue/resourceDs_df$FXRate
  minUnit_vec <- resourceDs_df$minUnit
  minUnitValue_vec <- unitValue_vec*minUnit_vec
  callAmount_vec <- rep(dsCallInfo$callAmount,resourceDsNum)
  costBasis_mat <- availInfoDs_list$cost_mat  
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
    resourceInDsMs_vec <- unique(c(resourceInDsCall_vec,sameMsSelection_vec))
  } else{
    resourceInDsMs_vec <- resourceInDsCall_vec
  }
  
  assetQuanityAllocateddsCall_vec <- currentSelection_list[[dsCallId]]$Quantity
  #### Find Resources Allocated to the Deselected Margin Statement END ######
  
  
  #### New Allocation Start #####################
  allocationDsCall_df <- currentSelection_list[[dsCallId]]
  callAmount <- callInfo_df$callAmount[which(callInfo_df$id==dsCallId)]
  fulfilledAmount <- sum(allocationDsCall_df$`NetAmount(USD)`)
  ## If the call is not fully fulfilled after removing dsAsset
  if(callAmount > fulfilledAmount){
    #### Calucate the Insufficient Amount, Initiate the New Allocation Start ######
    lackAmount <- callAmount-fulfilledAmount
    lackQuantity_vec <- ceiling(lackAmount/(1-haircut_vec)/minUnitValue_vec) # tempQuantity_vec needed for a single asset to fulfill each call
    # could either add the amount of the original selection or add another one or several assets.
    
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
    names(optimal_vec) <- resourceDs_vec
    optimal_vec <-sort(optimal_vec)  # sort the score of the assets, from the most to the least optimal
    idxOptimal_vec <- match(names(optimal_vec),resourceDs_vec) # the index of the optimal_vec in the resource_vec
    
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
        suffResource_vec <- resourceDs_vec[idxSuff_vec]
        insuffResource_vec <- resourceDs_vec[-idxSuff_vec]
        #### sorting:
        suffSort_vec <- sort(optimal_vec[match(suffResource_vec,names(optimal_vec))])
        insuffSort_vec <- sort(optimal_vec[match(insuffResource_vec,names(optimal_vec))])
        optimal_vec <- c(suffSort_vec,insuffSort_vec)
        idxOptimal_vec <- match(names(optimal_vec),resourceDs_vec) # the index of the optimal_vec in the resourceDs_vec
      }
    } else if(movementsLeft>=2){
      # other scenarios will have more possibilities
      # for simplicity, use the same order method as the 1 movement left scenario
      # but warning at the end
      idxSuff_vec <- which(quantityLeft_vec >= lackQuantity_vec)
      suffResource_vec <- resourceDs_vec[idxSuff_vec]
      insuffResource_vec <- ifelse(length(idxSuff_vec)==0,resourceDs_vec,resourceDs_vec[-idxSuff_vec])
      #### sorting:
      suffSort_vec <- sort(optimal_vec[match(suffResource_vec,names(optimal_vec))])
      insuffSort_vec <- sort(optimal_vec[match(insuffResource_vec,names(optimal_vec))])
      optimal_vec <- c(suffSort_vec,insuffSort_vec)
      idxOptimal_vec <- match(names(optimal_vec),resourceDs_vec) # the index of the optimal_vec in the resourceDs_vec
    }
    #### Check the Movements Limit END #######
    
    #### Alternative Assets Selection Start ####
    # check whether the first optimal asset is already allocated to that margin statement
    # check whether the first optimal asset is enough to fulfill the margin call
    result_mat <- ResultList2Mat(currentSelection_list[dsCallId],dsCallId,resourceDs_vec,minUnit_vec)
    scenario <- 0
    for(i in idxOptimal_vec){
      resource <- resourceDs_vec[i]
      # create the new line
      lineResource_df <- resourceDs_df[which(resourceDs_df$id==resource),]
      if(is.element(resource,resourceInDsMs_vec) && resourceInDsMs_vec!='na'){
        # if asset[i] is alreadly selected in the margin statement
        if(quantityLeft_vec[i]>=lackQuantity_vec[i]){
          # scenario 1: asset[i] is alreadly selected in the margin statement
          # & left quantity is larger than the insufficient quantity
          scenario <- 1
          idxResource <- match(resource,resourceInDsMs_vec)
          addQuantity <- lackQuantity_vec[i]
          result_mat[i] <- result_mat[i] + addQuantity
          break
        }else if(quantityLeft_vec[i] > 0){
          # scenario 2: asset[i] asset[i] is alreadly selected in the margin statement
          # & is less than the insufficient tempQuantity_vec but larger than 0
          scenario <- 2
          idxResource <- match(resource,resourceInDsMs_vec)
          addQuantity <- lackQuantity_vec[i]
          result_mat[i] <- result_mat[i] + addQuantity
        }
      } else {
        # if asset[i] is not in the current selection for margin statement
        if(quantityLeft_vec[i]>=lackQuantity_vec[i]){
          # scenario 3: asset[i] is not in the current selection for margin statement,
          # & left quantity is larger than the insufficient quantity
          scenario <- 3
          newQuantity <- lackQuantity_vec[i]
          result_mat[i] <- newQuantity
          break
        } else if(quantityLeft_vec[i] > 0){
          # scenario 4: asset[i] is not in the current selection for margin statement
          # & left tempQuantity_vec is less than the insufficient tempQuantity_vec but larger than 0
          scenario <- 4
          
          newQuantity <- quantityLeft_vec[i]
          result_mat[i] <- newQuantity
        }
      }
    }
    callDs_list <- list();
    msDs_list <- list();
    result_list <- ResultMat2List(result_mat,dsCallId,resourceDs_vec,dsCallInfo,haircut_mat,costBasis_mat,resourceDs_df,
                                  callDs_list,msDs_list)
    
    #cat('scenario: ',scenario,'\n')
    #cat('asset: ',i,'  ',resource,'\n')
    
    #### Re-allocation Start #############################
    #### After allocation, if violate the constraint limit, then reallocate the entire statement.
    #### call first level algo ####
    #### only re-allocate the deselect from margin statement
    #### before re-allocate, update the quantity
    newSelection_list <- currentSelection_list
    newSelection_list[[dsCallId]]<- result_list$callSelect_list[[dsCallId]]
    
    idxTemp1 <- which(names(newSelection_list) %in% allCallInDsMs_vec)
    idxTemp2 <- which(callInfo_df$id %in% allCallInDsMs_vec)
    movementsUsedMs <- OperationFun(newSelection_list[idxTemp1],callInfo_df[idxTemp2,],'callList')
    movementsUsedAll <- OperationFun(newSelection_list,callInfo_df,'callList')
    
    #### get the info related to the MS
    availAssetMsOri_df <- availAssetTotal_df[which(availAssetTotal_df$callId %in% allCallInDsMs_vec),]
    resourceMs_vec <- unique(availAssetMsOri_df$assetCustacId)
    assetIdMs_vec <- SplitResource(resourceMs_vec,'asset')
    resourceMs_df <- resourceTotal_df[match(resourceMs_vec,resourceTotal_df$id),]
    callInfoMs_df <- callInfo_df[which(callInfo_df$id %in% allCallInDsMs_vec),]
    
    #### get the allocation other than this ms
    currentSelectionOther_list <- list()
    otherCall_vec <- callId_vec[-idxTemp2]
    
    if(length(otherCall_vec)>=1){
      for(call in otherCall_vec){
        currentSelectionOther_list[[call]] <- newSelection_list[[call]]
      }
      
      quantityUsedOther_vec <- UsedQtyFromResultList(currentSelectionOther_list,resourceMs_vec,otherCall_vec)
      ### review...
      #### restore before deducting
      resourceMs_df <- ResetQtyMinInResourceDf(resourceMs_df)
      resourceMs_df$qtyMin <- resourceMs_df$qtyMin - quantityUsedOther_vec
    }
    
    #### remove the lines with deselect asset & call
    availAssetMs_df <- availAssetMsOri_df[which(availAssetMsOri_df$callId %in% allCallInDsMs_vec),]
    rmRow_vec <- which(availAssetMs_df$callId==dsCallId & availAssetMs_df$assetId==dsAssetId)
    if(length(rmRow_vec)>=1){
      availAssetMs_df <- availAssetMs_df[-rmRow_vec,]
    }
    
    availInfoMs_list <- AssetByCallInfo(allCallInDsMs_vec,resourceMs_vec,availAssetMs_df)
    
    
    if(movementsUsedMs>operLimitMs & fungible==F){
      newMsResult_list <- CoreAlgoV2(callInfoMs_df, resourceMs_df, availInfoMs_list,timeLimit=5,pref_vec,operLimitMs,operLimitMs,fungible,minMoveValue=1000,
                                     ifNewAlloc=T)
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
  }
  #### New Allocation END #######################
  
  #### Result Analysis Start ###################
  
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
  quantityUsed_vec <- UsedQtyFromResultList(currentSelection_list,resourceTotal_vec,callId_vec)
  resourceTotal_df <- ResetQtyMinInResourceDf(resourceTotal_df)
  quantityTotal_vec <- resourceTotal_df$qtyMin
  
  quantityRes_vec <- quantityTotal_vec-quantityUsed_vec/resourceTotal_df$minUnit
  
  liquidity_vec <- apply((1-availInfoTotal_list$haircut_mat)^2,2,min)
  
  minUnitValueTemp_vec <- resourceTotal_df$unitValue*resourceTotal_df$minUnit/resourceTotal_df$FXRate
  reservedLiquidityRatio <- LiquidFun(quantityRes_vec,quantityTotal_vec,liquidity_vec,minUnitValueTemp_vec)
  
  #### Movements
  movements <- OperationFun(currentSelection_list,callInfo_df,'callList')
  if(movements > operLimit){
    warning('Allocating other assets will create more asset movements than limit!')
  }
  resultAnalysis <- list(dailyCost=dailyCost,monthlyCost=monthlyCost,movements=movements,reservedLiquidityRatio=reservedLiquidityRatio)
  #### Result Analysis END #####################
  
  output_list <- list(callOutput=currentSelection_list,resultAnalysis=resultAnalysis)
  return(output_list)
}

