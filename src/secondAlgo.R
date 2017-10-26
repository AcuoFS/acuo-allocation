
SecondAllocationV2<- function(callId_vec,callInfo_df,resource_vec,availAsset_df,resource_df,
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
  callInfoDs_df$callAmount <- lackAmount_vec
  
  ## b. Remove the calls which already fulfilled
  #### delete from dsCallId_vec if correspond lackAmount is nagative
  idxSuff_vec <- which(lackAmount_vec <= 0)
  if(length(idxSuff_vec)>=1){
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
  availInfo_list <- AssetByCallInfo(callId_vec,resource_vec,availAsset_df)
  
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

