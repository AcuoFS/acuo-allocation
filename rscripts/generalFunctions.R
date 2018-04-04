#### analysisFunctions #######
LiquidFun <- function(quantityLeft_vec,quantityTotal_vec,liquidity_vec,minUnitValue_vec){
  numerator <- sum(quantityLeft_vec*liquidity_vec*minUnitValue_vec)
  denominator <- sum(quantityTotal_vec*liquidity_vec*minUnitValue_vec)
  ratio <- numerator/denominator
  return(ratio)
}

CostDefinition <- function(availAsset_df){
  cost <- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-(availAsset_df$interestRate+availAsset_df$yield)
  return(cost)
}

CostFun <- function(amount_vec,cost_vec){
  cost <- sum(amount_vec*cost_vec)
  return(cost)
}

OperationFun <- function(result,callInfo_df,method){
  movements <- 0
  if(method=='matrix'){
    result_mat <- result
    resultDummy_mat <- 1*(result_mat&1)
    msDul_vec <- callInfo_df$marginStatement
    msId_vec <- unique(msDul_vec)
    
    if(length(result_mat[1,])==1){
      for(m in 1:length(msId_vec)){
        idxTemp_vec <- which(msDul_vec==msId_vec[m])
        if(length(idxTemp_vec)==1){
          movements <- movements+sum(resultDummy_mat[idxTemp_vec])
        } else{
          movements <- movements+max(resultDummy_mat[idxTemp_vec])
        }
      }
    } else{
      for(m in 1:length(msId_vec)){
        idxTemp_vec <- which(msDul_vec==msId_vec[m])
        if(length(idxTemp_vec)==1){
          movements <- movements+sum(resultDummy_mat[idxTemp_vec,])
        } else{
          movements <- movements+sum(apply(resultDummy_mat[idxTemp_vec,],2,max))
        }
      }
    }
  } else if(method=='msList'){
    msOutput_list <- result
    msId_vec <- unique(callInfo_df$marginStatement)
    msNum <- length(msId_vec)
    for(i in 1:msNum){
      msId <- msId_vec[i]
      msAlloc_df <- msOutput_list[[msId]]
      resources <- unique(PasteResource(msAlloc_df$Asset,msAlloc_df$CustodianAccout))
      movements <- movements + length(resources)
    }
  } else if(method=='callList'){
    callOutput_list <- result
    callId_vec <- callInfo_df$id
    msId_vec <- unique(callInfo_df$marginStatement)
    callNum <- length(callId_vec)
    msNum <- length(msId_vec)
    for(i in 1:msNum){
      msId <- msId_vec[i]
      callIds <- callInfo_df$id[which(callInfo_df$marginStatement==msId)]
      
      callId <- callIds[1]
      msAlloc_df <- callOutput_list[[callId]]
      if(length(callIds>1)){
        # inside one margin statement
        for(m in 2:length(callIds)){
          callId <- callIds[m]
          callAlloc_df <- callOutput_list[[callId]]
          msAlloc_df <- rbind(callAlloc_df,msAlloc_df)
        }
      }
      resources <- unique(PasteResource(msAlloc_df$Asset,msAlloc_df$CustodianAccout))
      movements <- movements + length(resources)
    }
    
  } else{
    stop('ALERR3005: Invalid OperationFun input method!')
  }
  
  return(movements)
}

#### checkFunctions #######
AdjustResultVec <- function(solution_vec,varNum,varName_vec,fCon4_mat,
                            callAmount_vec,minUnitQuantity_vec,minUnitValue_vec){
  
  # round up the decimal quantity to the nearest integer.
  # if it's larger than 0.5
  # if close to 0, then set both real and dummies to 0, and if this action causes the 
  # the insufficiency of the total amount, make it up at the checking module
  # not only update result_mat but also the original solution_vec
  
  # Round the extreme values E to a reasonable number R
  # extreme definition: ext = min(quantity limit,2*margin call)
  # reasonable number definition: rea = min(quantity limit, margin call)
  
  varNum2 <- length(varName_vec)
  
  solNum1_vec <- solution_vec[1:varNum]
  solNum2_vec <- solution_vec[(varNum+1):varNum2]
  
  # Rounding
  solNum1_vec[which(solNum1_vec >= 0.5)] <- ceiling(solNum1_vec[which(solNum1_vec >= 0.5)])
  solNum1_vec[which(solNum1_vec < 0.5)] <- 0
  
  ext <- pmin(minUnitQuantity_vec,2*callAmount_vec/minUnitValue_vec)
  rea <- pmin(minUnitQuantity_vec,2*callAmount_vec/minUnitValue_vec)
  extIdx_vec <- which((solNum1_vec>=ext)==TRUE)
  solNum1_vec[extIdx_vec] <- rea[extIdx_vec]
  
  
  ## update
  temp <- varNum2-varNum
  solNum1_mat <- matrix(rep(solNum1_vec,temp),ncol=varNum,byrow=T)
  solNum2_mat <- solNum1_mat*fCon4_mat[1:temp,1:varNum]
  if(temp>1){
    temp_vec <- apply(solNum2_mat,1,sum)
  } else{
    temp_vec <- sum(solNum2_mat) # by row
  }
  
  solNum2_vec <- 1*(temp_vec & 1) # recalculate the dummy value
  
  # substitute
  solution_vec[1:varNum] <- solNum1_vec 
  solution_vec[(varNum+1):varNum2] <- solNum2_vec
  
  return(solution_vec)
}

CheckResultVec <- function(result_mat,quantityTotal_vec,callId_vec,callAmount_vec,minUnitValue_mat,haircut_mat,eli_mat){
  #### CHECK ALLOCATION RESULT ###############
  # STATUS: Developing
  #
  callNum <- length(callId_vec)
  # 1. whether all variables are non-negative
  for(k in 1:callNum){
    idxNeg_vec <- which(result_mat[k,]<0)
    if(length(idxNeg_vec)>=1){
      quantityTotal_vec[idxNeg_vec] <- quantityTotal_vec[idxNeg_vec] - result_mat[k,idxNeg_vec]
      result_mat[k,idxNeg_vec] <-0 # set to 0 first, then check the other two criteria
    }
  }
  # whether meet the constraint
  
  
  # 2. whether statisfy the quantity limits
  quantityUsed_vec <- apply(result_mat,2,sum)
  quantityLeft_vec <- quantityTotal_vec-quantityUsed_vec
  idxExcess_vec <- which(quantityUsed_vec>quantityTotal_vec)
  if(length(idxExcess_vec)>=1){
    
    for(i in idxExcess_vec){          # i: the index of the excess quantity asset in assetId_vec
      currentAlloc_mat <- matrix(c(which(result_mat[,i]>0),result_mat[which(result_mat[,i]>0),i]),nrow=2,byrow=T)
      if(length(currentAlloc_mat[1,])>1){
        currentAlloc_mat<-currentAlloc_mat[,order(currentAlloc_mat[2,])]
      }
      for(k in 1:length(currentAlloc_mat[1,])){ # k: the kth margin call which asset[i] allocated to
        j = currentAlloc_mat[1,k]  # j: the index of the the kth margin call in callId_vec
        # current allocated quantity < excess quanity
        if(currentAlloc_mat[2,k]< (-quantityLeft_vec[i])){
          # the amount missing for the margin call j if excluding the asset i
          newQuantity <- 0
          otherAmount <- sum(result_mat[j,1+which(result_mat[j,-i]>0)]*minUnitValue_mat[j,1+which(result_mat[j,-i]>0)]*(1-haircut_mat[j,1+which(result_mat[j,-i]>0)]))
          missingAmount <- callAmount_vec[j]-(otherAmount+newQuantity/(1-haircut_mat[j,i])/minUnitValue_mat[j,i])
          # missingAmount<0, means even we substract the exceed quantity of the asset, 
          # the sub-total is still larger than call amount, then, we update asset to the 
          # least quantity(already 0) which can meet the margin call requirement, no swaps occur
          if(missingAmount<=0){
            result_mat[j,i]<- newQuantity
            
            quantityUsed_vec <- apply(result_mat,2,sum)
            quantityLeft_vec <- quantityTotal_vec-quantityUsed_vec
            break
          }
          # first check whether the other previous allocated assets are sufficient,based on the operation efficiency
          # find the other asset which is sufficient and eligible for margin call j
          
          missingQuantity_vec <- ceiling((missingAmount/(1-haircut_mat)/minUnitValue_mat)[j,])
          idxSuff_vec <- intersect(which(missingQuantity_vec<=quantityLeft_vec),which(eli_mat[j,]==1))
          
          # whether there are other assets allocated to call j
          idxSwapProb_vec <- intersect(which(result_mat[j,]>0),idxSuff_vec)
          if(length(idxSwapProb_vec)>=1){
            idxSwapNew <- idxSwapProb_vec[1]
          }else{
            idxSwapNew <- idxSuff_vec[1]
          }
          swapNewQuantity <- missingQuantity_vec[idxSwapNew]+result_mat[j,idxSwapNew]
          newAllocation_mat <- matrix(currentAlloc_mat[,-which(currentAlloc_mat[1,]==j)],nrow=2)
          
          if(length(which(result_mat[,idxSwapNew]>0))){
            swapAllocation_mat<- matrix(c(which(result_mat[,idxSwapNew]>0),result_mat[which(result_mat[,idxSwapNew]>0),idxSwapNew]),nrow=2,byrow=T)
            swapAllocation_mat[2,which(swapAllocation_mat[1,]==j)] <- swapNewQuantity
          }else{
            swapAllocation_mat<- matrix(c(idxSwapNew,swapNewQuantity),nrow=2)
          }
          # update the result_mat
          result_mat[j,c(i,idxSwapNew)]<- c(newQuantity,swapNewQuantity)
          
          quantityUsed_vec <- apply(result_mat,2,sum)
          quantityLeft_vec <- quantityTotal_vec-quantityUsed_vec
        }
        else{
          # the amount missing for the margin call j if excluding the asset i
          # shouldn't exclude the asset i, just reduce to the sufficient amount, and use other assets to fulfil the left call amount
          newQuantity<- currentAlloc_mat[2,which(currentAlloc_mat[1,]==j)]+quantityLeft_vec[i]
          
          # if this asset is the only selection
          if(callNum==1){
            otherAmount <- sum(result_mat[,-i][which(result_mat[-i]>0)]*minUnitValue_mat[,-i][which(result_mat[-i]>0)]*
                                 (1-haircut_mat[,-i][which(result_mat[-i]>0)]))
          } else{
            otherAmount <- sum(result_mat[,-i][j,which(result_mat[j,-i]>0)]*minUnitValue_mat[,-i][j,which(result_mat[j,-i]>0)]*
                                 (1-haircut_mat[,-i][j,which(result_mat[j,-i]>0)]))
          }
          missingAmount <- callAmount_vec[j]-(otherAmount+newQuantity*minUnitValue_mat[j,i]*(1-haircut_mat[j,i]))
          # missingAmount<0, means even we substract the exceed quantity of the asset, 
          # the sub-total is still larger than call amount, then, we update asset to the 
          # least quantity which can meet the margin call requirement, no swaps occur
          if(missingAmount<=0){
            newQuantity <-  ceiling((callAmount_vec[j]-otherAmount)/minUnitValue_mat[j,i]/(1-haircut_mat[j,i]))
            result_mat[j,i]<- newQuantity
            quantityUsed_vec <- apply(result_mat,2,sum)
            quantityLeft_vec <- quantityTotal_vec-quantityUsed_vec
            break
          }
          
          # first check whether the other previous allocated assets are sufficient,based on the operation efficiency
          # find the other asset which is sufficient and eligible for margin call j
          missingQuantity_vec <- ceiling((missingAmount/(1-haircut_mat)/minUnitValue_mat)[j,])
          idxSuff_vec <- intersect(which(missingQuantity_vec<=quantityLeft_vec),which(eli_mat[j,]==1))
          
          if(length(idxSuff_vec)==0){
            # sacrifice the fulfilled call amount if the it is still larger than the shreshod
            if((callAmount_vec[j]-missingAmount)>=callAmount_vec[j]){
              result_mat[j,i]<- newQuantity
            }
            # left quantity of each available asset for this call is not sufficient
            # need more than one assets to allocate to this call
            # compare the missing amount and the sum of the left asset left amount
            # asset.amount.left <- matrix(c(1:resourceNum,quantityLeft_vec*minUnitValue_mat[j,]),nrow=2,byrow=T)
            
            # there should be more than one assets available(else will be detected in the pre-check sufficiency part)
            # order by amount from larger to smaller, make sure the least movements
            # asset.amount.left <- asset.amount.left[,order(asset.amount.left[2,])]
            
            # the index of available assets, excluding the 
            # idxTemp <- intersect(which(quantityLeft_vec>0),which(eli_mat[j,]==1))
          } else{
            # whether there are other assets allocated to call j
            idxSwapProb_vec <- intersect(which(result_mat[j,]>0),idxSuff_vec)
            if(length(idxSwapProb_vec)>=1){
              idxSwapNew <- idxSwapProb_vec[1]
            } else{
              idxSwapNew <- idxSuff_vec[1]
            }
            swapNewQuantity <- missingQuantity_vec[idxSwapNew]+result_mat[j,idxSwapNew]
            
            newAllocation_mat <- currentAlloc_mat
            newAllocation_mat[,-which(currentAlloc_mat[1,]==j)] <- newQuantity
            
            if(length(which(result_mat[,idxSwapNew]>0))){
              swapAllocation_mat<- matrix(c(which(result_mat[,idxSwapNew]>0),result_mat[which(result_mat[,idxSwapNew]>0),idxSwapNew]),nrow=2,byrow=T)
              swapAllocation_mat[2,which(swapAllocation_mat[1,]==j)] <- swapNewQuantity
            }else{
              swapAllocation_mat<- matrix(c(idxSwapNew,swapNewQuantity),nrow=2)
            }
            
            # update the result_mat
            result_mat[j,c(i,idxSwapNew)]<- c(newQuantity,swapNewQuantity)
          }
          
          quantityUsed_vec <- apply(result_mat,2,sum)
          quantityLeft_vec <- quantityTotal_vec-quantityUsed_vec
          break
        }
      } 
    }
  }
  
  # 3. whether meet all margin call requirements
  quantityUsed_vec <- apply(result_mat,2,sum)
  quantityLeft_vec <- quantityTotal_vec-quantityUsed_vec
  
  # compare with the call amount, not the custimized amount based on the user preference
  callFulfilled_vec <- apply(result_mat*minUnitValue_mat*(1-haircut_mat),1,sum)
  callMissingAmount_vec <- callAmount_vec-callFulfilled_vec
  idxCallMissing_vec <- which(callMissingAmount_vec>0)
  if(length(idxCallMissing_vec)>=1){
    
    for(i in idxCallMissing_vec){
      
      currentAlloc_mat <- matrix(c(which(result_mat[i,]>0),result_mat[i,which(result_mat[i,]>0)]),nrow=2,byrow=T)
      
      missingAmount <- callMissingAmount_vec[i]
      missingQuantity_vec <- ceiling((missingAmount/(1-haircut_mat)/minUnitValue_mat)[i,])
      idxSuff_vec <- intersect(which(missingQuantity_vec<=quantityLeft_vec),which(eli_mat[i,]==1))
      if(length(idxSuff_vec)==0){
        # which means none of the asset itself is enough to to fulfill the left amount of the margin call
        # This should be a very extreme case, and it's more complicated to develop for this case
        # so, I will leave here blank, once I'm done the rest part I'll return to check
        # Also, the exception handling will be a long-run development, and it will be raised once we have exception
      }
      
      # whether there are assets which are sufficient allocated to call i
      idxCurrentProb_vec <- intersect(idxSuff_vec,currentAlloc_mat[1,])
      if(length(idxCurrentProb_vec)==0){
        idxCurrentProb_vec<- idxSuff_vec
      }
      idxAddNew <- idxCurrentProb_vec[1]
      addNewQuantity <- missingQuantity_vec[idxAddNew]+result_mat[i,idxAddNew]
      result_mat[i,idxAddNew] <- addNewQuantity
    }
  }
  
  return(list(result_mat=result_mat,quantityTotal_vec=quantityTotal_vec))
}

#### convertFunctions #### 
ConstructAllocDf <- function(resourceInfo_df,callInfo_df,haircutC_vec,haircutFX_vec,minUnitQuantity_vec,cost_vec ){
  assetId_vec <- resourceInfo_df$assetId
  assetCustodianAccount_vec <- resourceInfo_df$custodianAccount
  assetVenue_vec <- resourceInfo_df$venue
  assetName_vec <- resourceInfo_df$assetName
  assetHaircut_vec <- haircutC_vec+haircutFX_vec
  assetHaircutC_vec <- haircutC_vec
  ## add fx haircut
  assetHaircutFX_vec <- haircutFX_vec
  assetCostFactor_vec <- cost_vec
  assetCurrency_vec <- resourceInfo_df$currency
  assetMinUnitQuantity_vec <- minUnitQuantity_vec
  assetQuantity_vec <- minUnitQuantity_vec*resourceInfo_df$minUnit
  marginType_vec <- rep(callInfo_df$marginType,length(cost_vec))
  ms_vec <- rep(callInfo_df$marginStatement,length(cost_vec))
  call_vec <- rep(callInfo_df$id,length(cost_vec))
  
  assetFX_vec <- resourceInfo_df$FXRate
  assetOriFX_vec <- resourceInfo_df$oriFXRate
  assetUnitValue_vec <- resourceInfo_df$unitValue/assetFX_vec
  
  assetAmountUSD_vec <- assetQuantity_vec*assetUnitValue_vec
  assetNetAmountUSD_vec <- assetAmountUSD_vec*(1-assetHaircut_vec)
  
  assetCost_vec <- assetCostFactor_vec*assetAmountUSD_vec
  
  assetAmount_vec <- assetAmountUSD_vec*assetFX_vec
  assetNetAmount_vec <- assetNetAmountUSD_vec*assetFX_vec
  
  alloc_df <- data.frame(assetId_vec,assetName_vec,assetNetAmount_vec,assetNetAmountUSD_vec,assetOriFX_vec,assetFX_vec,assetHaircut_vec,assetHaircutC_vec,assetHaircutFX_vec,assetAmount_vec,assetAmountUSD_vec,
                         assetCurrency_vec,assetQuantity_vec,assetCustodianAccount_vec,assetVenue_vec,marginType_vec,ms_vec,call_vec,assetCostFactor_vec,assetCost_vec)
  colnames(alloc_df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','FXRatePerUSD','Haircut','Hc','Hfx','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType',
                         'marginStatement','marginCall','CostFactor','Cost')
  rownames(alloc_df)<- 1:length(alloc_df[,1])
  return(alloc_df)
}

ResultMat2List <- function(result_mat,callId_vec,resource_vec,callInfo_df, haircutC_mat,haircutFX_mat,cost_mat,resourceInfo_df,
                           callSelect_list,msSelect_list){
  
  callNum <- length(callId_vec)
  
  #### construct the result
  for(i in 1:callNum){      
    # resource and result_mat columns have the same order 
    # the allocated indexes: 
    idx_vec <- which(result_mat[i,]!=0) 
    if(length(idx_vec)==0){
      errormsg <- paste("ALEER3004: There's no asset allocated to margin call",callId_vec[i])
      stop(errormsg)
    }
    
    alloc_df <- ConstructAllocDf(resourceInfo_df[idx_vec,], callInfo_df[i,], haircutC_mat[i,idx_vec], haircutFX_mat[i,idx_vec],result_mat[i,idx_vec],cost_mat[i,idx_vec])
    
    #### UPDATE THE ASSET QUANTITY START ########
    resourceInfo_df$quantity[idx_vec] <- resourceInfo_df$quantity[idx_vec]-alloc_df$Quantity #selectAssetQuantity_vec
    
    #### UPDATE THE ASSET QUANTITY END ##########
    #### Update callSelect_list Start ####
    callSelect_list[[callId_vec[i]]] <- alloc_df
    #### Update callSelect_list END ######
    
    #### Update msSelect_list Start ######
    msId <- callInfo_df$marginStatement[i]
    if(is.null(msSelect_list[[msId]])){
      msSelect_list[[msId]] <- alloc_df
    } else{
      tempAlloc_df <- msSelect_list[[msId]]
      alloc_df <- rbind(alloc_df,tempAlloc_df)
      rownames(alloc_df)<- 1:length(alloc_df[,1])
      msSelect_list[[msId]] <- alloc_df
    }
    #### Update msSelect_list END ########
  }
  result_list <- list(callSelect_list=callSelect_list,msSelect_list=msSelect_list)
  return(result_list)
}

ResultVec2Mat <- function(solution_vec,callId_vec,resource_vec,idxEli_vec,varNum){
  callNum <- length(callId_vec); resourceNum <- length(resource_vec)
  result_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
  result_mat <- t(result_mat)
  result_mat[idxEli_vec]<-solution_vec[1:varNum]
  result_mat <- t(result_mat)
  return(result_mat)
}

ResultList2Mat <- function(callOutput_list,callId_vec,resource_vec,minUnit_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  result_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
  
  for(m in 1:callNum){
    callId <- callId_vec[m]
    callAlloc_df <- callOutput_list[[callId]]
    
    # the 'Quantity'= decision variable * minUnit
    # find the corresponding decision variable index from the varName
    resourceTemp_vec <- PasteResource(callAlloc_df$Asset,callAlloc_df$CustodianAccount)
    #varNameTemp_vec <- PasteVarName(callAlloc_df$marginStatement,callAlloc_df$marginCall,resourceTemp_vec)
    
    idxTemp_vec <- match(resourceTemp_vec,resource_vec)
    
    quantityTemp_vec <- callAlloc_df$Quantity
    minUnitQuantityTemp_vec <- quantityTemp_vec/minUnit_vec[idxTemp_vec]
    
    result_mat[m,idxTemp_vec] <- minUnitQuantityTemp_vec
  }
  return(result_mat)
}

ResultList2Vec <- function(callOutput_list,callId_vec,minUnit_vec,varName_vec,varNum,pos_vec){
  varNum2 <- length(varName_vec)
  result1_vec <- rep(0,varNum)
  callNum <- length(callId_vec)
  
  for(m in 1:callNum){
    callId <- callId_vec[m]
    callAlloc_df <- callOutput_list[[callId]]
    
    # find the corresponding decision variable index from the varName
    resourceTemp_vec <- PasteResource(callAlloc_df$Asset,callAlloc_df$CustodianAccount)
    varNameTemp_vec <- PasteVarName(callAlloc_df$marginStatement,callAlloc_df$marginCall,resourceTemp_vec)
    
    for(k in 1:length(resourceTemp_vec)){
      idxVarTemp <- which(varName_vec==varNameTemp_vec[k])
      quantityTemp <- callAlloc_df$Quantity[k]
      # the 'Quantity'= decision variable * minUnit
      result1_vec[idxVarTemp] <- quantityTemp/minUnit_vec[idxVarTemp]
    }
  }
  # derive the decision variables (varNum+1 ~ varNum2)
  var1_df <- data.frame(real=result1_vec,pos=pos_vec)
  var2_df <- aggregate(real~pos,data=var1_df,sum)
  result2_vec <- ((var2_df$real) & 1)*1
  
  result_vec <- c(result1_vec,result2_vec)
  
  return(result_vec)
}

ResultList2DummyVec <- function(callOutput_list,callId_vec,varName_vec,varNum){
  varNum2 <- length(varName_vec)
  result_vec <- rep(0,varNum2)
  callNum <- length(callId_vec)
  
  for(m in 1:callNum){
    callId <- callId_vec[m]
    callAlloc_df <- callOutput_list[[callId]]
    
    # find the corresponding decision variable index from the varName
    resourceTemp_vec <- PasteResource(callAlloc_df$Asset,callAlloc_df$CustodianAccount)
    varNameTemp_vec <- PasteVarName(callAlloc_df$marginStatement,callAlloc_df$marginCall,resourceTemp_vec)
    
    for(k in 1:length(resourceTemp_vec)){
      idxVarTemp <- which(varName_vec==varNameTemp_vec[k])
      result_vec[idxVarTemp] <- 1
    }
  }
  temp <- varNum2-varNum
  result1_mat <- matrix(rep(result_vec[1:varNum],temp),ncol=varNum,byrow=T)
  result2_mat <- result1_mat*fCon4_mat[1:temp,1:varNum]
  
  if(temp>1){
    temp_vec <- apply(result2_mat,1,sum)
  } else{
    temp_vec <- sum(result2_mat) # by row
  }
  
  result_vec[(varNum+1):varNum2] <- 1*(temp_vec & 1) # recalculate the dummy value
  
  return(result_vec)
}

ResultDf2List <- function(result_df,callId_vec){
  callNum <- length(callId_vec)
  result_list <- list()
  for(i in 1:callNum){
    callId <- callId_vec[i]
    idx_vec <- which(result_df$marginCall==callId)
    call_df <- result_df[idx_vec,]
    result_list[[callId]] <- call_df
  }
  return(result_list)
}

ResultList2AmountVec <- function(callOutput_list,callId_vec,varName_vec){
  callNum <- length(callId_vec)
  varNum <- length(varName_vec)
  var_vec <- rep(0,varNum)
  
  for(i in 1:callNum){
    callId <- callId_vec[i]
    currentAlloc_df <- callOutput_list[[callId]]
    currentResource_vec <- PasteResource(currentAlloc_df$Asset,currentAlloc_df$CustodianAccount)
    currentVarName_vec <- PasteVarName(currentAlloc_df$marginStatement,currentAlloc_df$marginCall,currentResource_vec)
    currentAmount_vec <- currentAlloc_df$`Amount(USD)`
    #currentQuantity_vec <- currentAlloc_df$Quantity
    #currentVarValue_vec <- currentQuantity_vec/minUnit_vec
    currentVarLoc_vec <- match(currentVarName_vec,varName_vec)
    
    ## fill in the var_vec
    var_vec[currentVarLoc_vec] <- currentAmount_vec
  }
  return(var_vec)
}

VarVec2mat <- function(var_vec,varName_vec,callId_vec,resource_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  # row1: ms; row2: call; row3: resource.
  target <- 'all'
  varName_mat <- SplitVarName(varName_vec,target)
  var_mat <- matrix(0,nrow=callNum,ncol=resourceNum, dimnames = list(callId_vec,resource_vec))
  
  for(i in 1:callNum){
    callId <- callId_vec[i]
    idxTemp_vec <- which(varName_mat[2,]==callId) # the indexes of decision variables
    currentResource_vec <- varName_mat[3,idxTemp_vec]
    currentValue <- var_vec[idxTemp_vec]
    currentLoc_vec <- match(currentResource_vec,resource_vec)
    
    var_mat[i,currentLoc_vec] <- currentValue
  }
  return(var_mat)
}

ResultList2Df <- function(result_list,callId_vec){
  result_df <- result_list[[callId_vec[1]]]
  if(length(callId_vec)>1){
    for(i in 2:length(callId_vec)){
      alloc_df <- result_list[[callId_vec[i]]]
      result_df <- rbind(result_df,alloc_df)
    }
  }
  rownames(result_df) <- 1:length(result_df[,1])
  return(result_df)
}

#### improveFunctions #### 
OrderCallId <- function(callOrderMethod,callInfo_df){
  ## method 0: Keep original
  ## method 1: By margin call amount, decreasing
  ## method 2: By margin type, VM then IM; sub order by call amount
  ## method 3: By total call amount in margin statement, decreasing
  
  if(callOrderMethod==0){ # keep original
    callInfo_df <- callInfo_df
  }else if(callOrderMethod==1){ # by call amount, decreasing
    callInfo_df <- callInfo_df[order(callInfo_df$callAmount,decreasing=T),]
  }else if(callOrderMethod==2){ # by margin type(VM first) and call amount, decreasing
    callInfoVM <- callInfo_df[which(toupper(callInfo_df$marginType)=='VARIATION'),]
    callInfoVM <- callInfoVM[order(callInfoVM$callAmount,decreasing=T),]
    
    callInfoIM <- callInfo_df[which(toupper(callInfo_df$marginType)=='INITIAL'),]
    callInfoIM <- callInfoIM[order(callInfoIM$callAmount,decreasing=T),]
    callInfo_df <- rbind(callInfoVM,callInfoIM)
  }else if(callOrderMethod==3){ # by margin statement, call amount in margin statement, decreasing
    msAggrCall_df <- aggregate(callAmount~marginStatement,data=callInfo_df,sum)
    msAggrCall_df <- msAggrCall_df[order(msAggrCall_df$callAmount,decreasing=T),]
    tempMs_vec <- msAggrCall_df$marginStatement
    newCallInfo_df <- callInfo_df
    idxCurrent <- 0
    for(i in 1:length(tempMs_vec)){
      idxTemp_vec <- which(tempMs_vec[i]==callInfo_df$marginStatement)
      tempCallInfo_df <- callInfo_df[idxTemp_vec,]
      tempCallInfo_df <- tempCallInfo_df[order(tempCallInfo_df$callAmount,decreasing=F),]
      idxNewTemp_vec <- idxCurrent+1:length(idxTemp_vec)
      newCallInfo_df[idxNewTemp_vec,] <- tempCallInfo_df
      
      idxCurrent <- idxCurrent+length(idxTemp_vec)
    }
    callInfo_df<- newCallInfo_df
  }
  return(callInfo_df)
}

GroupCallIdByMs <- function(callLimit,msLimit,callInfo_df,callId_vec){
  
  groupCallId_list <- list()
  # if the total call numbers is equal or less than limitTotal, only one group
  if(length(callInfo_df[,1])<=callLimit){
    groupCallId_list[[1]] <- callId_vec
  } else if(length(unique(callInfo_df$marginStatement))<=msLimit){
    groupCallId_list[[1]] <- callId_vec
  } else{
    groupMsId_list <- list()
    callMs_vec <- callInfo_df$marginStatement
    ms_vec <- unique(callMs_vec)
    msGroupNum <- ceiling(length(ms_vec)/msLimit)
    
    for(i in 1:(msGroupNum-1)){
      tempCurrent <- msLimit*(i-1)
      tempMs_vec <- ms_vec[(tempCurrent+1):(tempCurrent+msLimit)]
      tempCall_vec <- callInfo_df$id[which((callInfo_df$marginStatement) %in% tempMs_vec)]
      groupMsId_list[[i]]<- tempMs_vec
      groupCallId_list[[i]]<- tempCall_vec
    }
    tempCurrent <- msLimit*(msGroupNum-1)
    tempMs_vec <- na.omit(ms_vec[(tempCurrent+1):(tempCurrent+msLimit)])
    tempCall_vec <- callInfo_df$id[which((callInfo_df$marginStatement) %in% tempMs_vec)]
    groupMsId_list[[msGroupNum]]<- tempMs_vec
    groupCallId_list[[msGroupNum]]<- tempCall_vec
  }
  return(groupCallId_list)
}

#### infoFunctions #### 
ResourceInfo <- function(resource_vec,assetInfo_df,availAsset_df){
  ## better retrieve from DB
  ## keep useful columns from assetInfo
  ## asset id, name, currency, unitValue, minUnit, minUnitValue, FXRate
  assetId_vec <- SplitResource(resource_vec,'asset')
  custodianAccount_vec <- SplitResource(resource_vec,'custodianAccount')
  idx1_vec <- match(c('id', 'name', 'unitValue', 'minUnit', 'minUnitValue','currency', 'FXRate','oriFXRate'),names(assetInfo_df))
  resource_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),idx1_vec]
  
  ## add resource id, custodianAccount id, quantity, minQty, qtyRes
  resource_df <- cbind(id=resource_vec,resource_df,custodianAccount_vec)
  idx2_vec <- match(resource_vec, availAsset_df$assetCustacId)
  venue_vec <- availAsset_df$venue[idx2_vec] 
  qtyOri_vec <- availAsset_df$quantity[idx2_vec]
  qtyMin_vec <- floor(qtyOri_vec/resource_df$minUnit) # interal minUnit quantity
  qtyRes_vec <- qtyOri_vec - qtyMin_vec*resource_df$minUnit # quantity left after integral minQty
  
  resource_df <- cbind(resource_df[,1:3],qtyOri_vec,qtyMin_vec,qtyRes_vec,resource_df[,4:10],venue_vec)
  resource_df$id <- as.character(resource_df$id)
  
  names(resource_df) <- c('id','assetId','assetName','qtyOri','qtyMin','qtyRes','unitValue', 'minUnit','minUnitValue','currency','FXRate','oriFXRate',
                          'custodianAccount','venue')
  
  return(resource_df)
}

AvailAsset <- function(availAsset_df){
  ## keep useful columns
  ## "callId","assetCustacId","internalCost", "opptCost", "yield", "haircut","FXHaircut","externalCost","interestRate"
  idx_vec <- match(c("callId","assetCustacId","internalCost", "opptCost", "yield", "haircut","FXHaircut","externalCost","interestRate"),names(availAsset_df))
  new_df <- availAsset_df[,idx_vec]
  
  return(new_df)
}

AssetByCallInfo <- function(callId_vec,resource_vec,availAsset_df){
  
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  availAsset_df <- availAsset_df[order(availAsset_df$callId),] # order the availAsset_df by callId_vec
  
  base_mat <- matrix(0,nrow=callNum,ncol=resourceNum, dimnames = list(callId_vec,resource_vec))
  eli_mat <- base_mat
  haircut_mat <- base_mat
  haircutC_mat <- base_mat
  haircutFX_mat <- base_mat
  cost_mat <- base_mat
  
  # fill in matrixes with the data from availAsset_df
  idxTempCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxTempResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  eli_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- 1
  haircutC_mat[cbind(idxTempCallId_vec,idxTempResource_vec)] <- availAsset_df$haircut
  haircutFX_mat[cbind(idxTempCallId_vec,idxTempResource_vec)] <- availAsset_df$FXHaircut
  haircut_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$haircut+availAsset_df$FXHaircut
  cost_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- CostDefinition(availAsset_df)
  
  # convert the matrix format data to vector format
  # thinking of keeping only eligible parts
  eli_vec <- as.vector(t(eli_mat))
  haircut_vec <- as.vector(t(haircut_mat))
  cost_vec <- as.vector(t(cost_mat))
  
  output_list <- list(base_mat=base_mat,eli_mat=eli_mat,haircut_mat=haircut_mat,haircutC_mat=haircutC_mat,haircutFX_mat=haircutFX_mat,cost_mat=cost_mat)
  return (output_list)
}


#### modelFunction #### 
QtyConst <- function(varName_vec,varNum,resource_vec,quantityTotal_vec){
  
  resourceDul_vec <- SplitVarName(varName_vec,'resource')[1:varNum]
  resourceNum <- length(resource_vec)
  varNum2 <- length(varName_vec)
  
  
  fCon2_mat <- matrix(0,nrow=resourceNum,ncol=varNum2)
  
  colIdx_vec <- 1:varNum
  rowIdx_vec <- match(resourceDul_vec,resource_vec)
  
  fCon2_mat[cbind(rowIdx_vec,colIdx_vec)] <- 1
  
  fDir2_vec <- rep('<=',resourceNum)
  fRhs2_vec <- quantityTotal_vec
  
  fCon2_list <- list(fCon2_mat=fCon2_mat,fDir2_vec=fDir2_vec,fRhs2_vec=fRhs2_vec)
  
  return(fCon2_list)
}

MarginConst <- function(varName_vec,varNum,minUnitValue_vec,haircut_vec,callId_vec,callAmount_vec){
  
  callIdDul_vec <- SplitVarName(varName_vec,'call')[1:varNum]
  
  callNum <- length(callId_vec)
  varNum2 <- length(varName_vec)
  
  fCon3_mat <- matrix(0,nrow=callNum,ncol=varNum2)
  
  colIdx_vec <- 1:varNum
  rowIdx_vec <- match(callIdDul_vec,callId_vec)
  
  fCon3_mat[cbind(rowIdx_vec,colIdx_vec)] <- minUnitValue_vec*(1-haircut_vec)
  
  fDir3_vec <- rep('>=',callNum)
  fRhs3_vec <- callAmount_vec
  
  fCon3_list <- list(fCon3_mat=fCon3_mat,fDir3_vec=fDir3_vec,fRhs3_vec=fRhs3_vec)
  return(fCon3_list)
}

DummyConst <- function(varName_vec,varNum,quantity_vec,callAmount_vec,minUnitValue_vec){
  
  varNum2 <- length(varName_vec)
  newName_mat <- SplitVarName(varName_vec[1:varNum],'all')
  newName_vec <- PasteVarName(newName_mat[1,],rep('dummy',varNum),newName_mat[3,])
  newNameDummy_vec <- varName_vec[(varNum+1):varNum2]
  
  fCon4_mat <- matrix(0,nrow=(varNum2-varNum),ncol=varNum2)
  
  colIdx1_vec <- 1:varNum
  rowIdx1_vec <- match(newName_vec,newNameDummy_vec)
  colIdx2_vec <- (varNum+1):varNum2
  rowIdx2_vec <- 1:(varNum2-varNum)
  
  fCon4_mat[cbind(rowIdx1_vec,colIdx1_vec)] <- 1
  scaleFactor_vec <- pmin(quantity_vec,callAmount_vec/minUnitValue_vec)*20
  scaleFactor_vec <- scaleFactor_vec[match(newNameDummy_vec,newName_vec)]
  fCon4_mat[cbind(rowIdx2_vec,colIdx2_vec)] <- -scaleFactor_vec
  
  fDir4_vec <- rep('<=',varNum2-varNum)
  fRhs4_vec <- rep(0,varNum2-varNum)
  
  fCon5_mat <- matrix(0,nrow=varNum2-varNum,ncol=varNum2)
  fCon5_mat[cbind(rowIdx1_vec,colIdx1_vec)] <- 1
  fCon5_mat[cbind(rowIdx2_vec,colIdx2_vec)] <- -1
  
  fDir5_vec <- rep('>=',varNum2-varNum)
  fRhs5_vec <- rep(-0.1,varNum2-varNum)
  
  fCon4_mat <- rbind(fCon4_mat,fCon5_mat)
  fDir4_vec <- c(fDir4_vec,fDir5_vec)
  fRhs4_vec <- c(fRhs4_vec,fRhs5_vec)
  
  fCon4_list <- list(fCon4_mat=fCon4_mat,fDir4_vec=fDir4_vec,fRhs4_vec=fRhs4_vec)
  return(fCon4_list)
}

DummyConstInherit <- function(allocated_vec,varName_vec,varNum,quantity_vec,callAmount_vec,minUnitValue_vec){
  
  varNum2 <- length(varName_vec)
  newName_mat <- SplitVarName(varName_vec[1:varNum],'all')
  newName_vec <- PasteVarName(newName_mat[1,],rep('dummy',varNum),newName_mat[3,])
  newNameDummy_vec <- varName_vec[(varNum+1):varNum2]
  
  fCon4_mat <- matrix(0,nrow=(varNum2-varNum),ncol=varNum2)
  
  colIdx1_vec <- 1:varNum
  rowIdx1_vec <- match(newName_vec,newNameDummy_vec)
  colIdx2_vec <- (varNum+1):varNum2
  rowIdx2_vec <- 1:(varNum2-varNum)
  
  fCon4_mat[cbind(rowIdx1_vec,colIdx1_vec)] <- 1
  scaleFactor_vec <- pmin(quantity_vec,callAmount_vec/minUnitValue_vec)*20
  scaleFactor_vec <- scaleFactor_vec[match(newNameDummy_vec,newName_vec)]
  fCon4_mat[cbind(rowIdx2_vec,colIdx2_vec)] <- -scaleFactor_vec
  
  fDir4_vec <- rep('<=',varNum2-varNum)
  fDir4_vec[which(allocated_vec==1)] <- '>='  ## new added 18 May
  fRhs4_vec <- rep(0,varNum2-varNum)
  
  fCon5_mat <- matrix(0,nrow=varNum2-varNum,ncol=varNum2)
  fCon5_mat[cbind(rowIdx1_vec,colIdx1_vec)] <- 1
  fCon5_mat[cbind(rowIdx2_vec,colIdx2_vec)] <- -1
  
  fDir5_vec <- rep('>=',varNum2-varNum)
  fRhs5_vec <- rep(-0.1,varNum2-varNum)
  
  fCon4_mat <- rbind(fCon4_mat,fCon5_mat)
  fDir4_vec <- c(fDir4_vec,fDir5_vec)
  fRhs4_vec <- c(fRhs4_vec,fRhs5_vec)
  
  fCon4_list <- list(fCon4_mat=fCon4_mat,fDir4_vec=fDir4_vec,fRhs4_vec=fRhs4_vec)
  return(fCon4_list)
}

MoveConst <- function(varName_vec,varNum,operLimit,operLimitMs,fungible){
  varNum2 <- length(varName_vec)
  msIdDul_vec <- SplitVarName(varName_vec[(varNum+1):varNum2],'ms')
  msId_vec <- unique(msIdDul_vec)
  msNum <- length(msId_vec)
  
  fCon5_mat <- matrix(0,nrow=1,ncol=varNum2)
  fCon5_mat[1,(varNum+1):varNum2] <- 1
  
  fDir5_vec <- c('<=')
  fRhs5_vec <- c(operLimit)
  
  #### set the movements limit per margin statement if fungible=FALSE
  # the total limit is not necessary in theory, but it's better keep it until proven
  if(fungible==FALSE){
    # will be number of margin statements constraints
    fCon6_mat <- matrix(0,nrow=msNum,ncol=varNum2)
    
    colIdx_vec <- (varNum+1):varNum2
    rowIdx_vec <- match(msIdDul_vec,msId_vec)
    
    fCon6_mat[cbind(rowIdx_vec,colIdx_vec)] <- 1
    
    fDir6_vec <- rep('<=',msNum)
    fRhs6_vec <- rep(operLimitMs,msNum)
    
    fCon5_mat <- rbind(fCon5_mat,fCon6_mat)
    fDir5_vec <- c(fDir5_vec,fDir6_vec)
    fRhs5_vec <- c(fRhs5_vec,fRhs6_vec)
  }
  
  fCon5_list <- list(fCon5_mat=fCon5_mat,fDir5_vec=fDir5_vec,fRhs5_vec=fRhs5_vec)
  return(fCon5_list)
}

MoveConstInherit <- function(allocated_vec,varName_vec,varNum,operLimit,operLimitMs_vec,fungible){
  varNum2 <- length(varName_vec)
  msIdDul_vec <- SplitVarName(varName_vec[(varNum+1):varNum2],'ms')
  msId_vec <- unique(msIdDul_vec)
  msNum <- length(msId_vec)
  
  fCon5_mat <- matrix(0,nrow=1,ncol=varNum2)
  fCon5_mat[1,(varNum+1):varNum2] <- 1
  
  fDir5_vec <- c('<=')
  fRhs5_vec <- c(operLimit)
  
  #### set the movements limit per margin statement if fungible=FALSE
  # the total limit is not necessary in theory, but it's better keep it until proven
  if(fungible==FALSE){
    # will be number of margin statements constraints
    fCon6_mat <- matrix(0,nrow=msNum,ncol=varNum2)
    
    colIdx_vec <- (varNum+1):varNum2
    rowIdx_vec <- match(msIdDul_vec,msId_vec)
    
    fCon6_mat[cbind(rowIdx_vec,colIdx_vec)] <- 1
    
    fDir6_vec <- rep('<=',msNum)
    fRhs6_vec <- operLimitMs_vec
    
    fCon5_mat <- rbind(fCon5_mat,fCon6_mat)
    fDir5_vec <- c(fDir5_vec,fDir6_vec)
    fRhs5_vec <- c(fRhs5_vec,fRhs6_vec)
  }
  fCon5_mat[,which(allocated_vec==1)] <- 0
  
  fCon5_list <- list(fCon5_mat=fCon5_mat,fDir5_vec=fDir5_vec,fRhs5_vec=fRhs5_vec)
  return(fCon5_list)
}

AllocateByRank <- function(resource_vec,callId,rank_vec,callAmount,quantity_vec,minUnitValue_vec,haircut_vec,operLimit){
  ## rank_mat(scores assume using purely each asset)
  ## names(rank_mat)=resources, value: score
  
  # process
  # find the optimal resources within number of (operLimit) which are sufficient for the call
  solution_vec <- rep(0,length(resource_vec))
  if(operLimit<2){ # operLimit=1
    integralSuffQty_vec <- ceiling(callAmount/(1-haircut_vec)/minUnitValue_vec)
    suffIdx_vec <- which(quantity_vec >= integralSuffQty_vec)
    if(length(suffIdx_vec)==0){
      errormsg <- paste('ALERR2004: It is not sufficient to allocate',floor(operLimit),'assets for',callId,'!')
      stop(errormsg)
    } else{
      suffResource_vec <- resource_vec[suffIdx_vec]
      tempIdx <- which.max(rank_vec[suffIdx_vec])
      optimalResource <- suffResource_vec[tempIdx]
      optimalIdx <- which(resource_vec==optimalResource)
      quantity <- integralSuffQty_vec[optimalIdx]
      solution_vec[optimalIdx] <- quantity
    }
  }
  return(solution_vec)
}

ConstructModelObj <- function(callAmount_mat,minUnitValue_mat,haircut_mat,costBasis_mat,eli_mat,callInfo_df,
                              callId_vec,resource_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  #### calculate the cost if only the integral units of asset can be allocated
  integerCallAmount_mat <- ceiling(callAmount_mat/(1-haircut_mat)/minUnitValue_mat)*minUnitValue_mat
  
  cost_mat<-integerCallAmount_mat*costBasis_mat  # cost amount
  cost_vec <- as.vector(t(cost_mat))
  #costBasis_mat <- costBasis_mat/(1-haircut_mat)
  costBasis_vec <- as.vector(t(costBasis_mat))
  if(is.null(dim(haircut_mat))){
    assetLiquidity_vec <- (1-haircut_mat*eli_mat)^2 # define asset liquidity
    
  } else{
    assetLiquidity_vec <- apply((1-haircut_mat*eli_mat)^2,2,min) # define asset liquidity
    
  }
  liquidity_mat <- matrix(rep(assetLiquidity_vec,callNum),nrow=callNum,byrow=TRUE,dimnames=list(callId_vec,resource_vec)) 
  liquidity_vec <- as.vector(t(liquidity_mat))
  
  callCcy <- callInfo_df$currency
  operation_mat <- matrix(rep(1,resourceNum*callNum),nrow=callNum,byrow=TRUE,dimnames=list(callId_vec,resource_vec)) 
  assetId_vec <- SplitResource(resource_vec,'asset') #### parallel with resource, not unique
  for(i in 1:callNum){
    idxCcy <- which(callCcy[i]==assetId_vec)    # return the index of mc[i] currency cash in the asset list
    idx1 <- which(eli_mat[i,]!=0)             # return elegible asset idx for mc[i]
    if(length(idxCcy)==1 && is.element(idxCcy,idx1)){  # if there exist call currency cash in the inventory, and it's available
      operation_mat[i,idxCcy] <- 0
    }
  }
  operation_vec <- as.vector(t(operation_mat))
  
  normCost_mat <- cost_mat
  for(i in 1:callNum){
    if(length(unique(cost_mat[i,]))==1){
      normCost_mat[i,]<-1
    }else{
      normCost_mat[i,]<- scale(cost_mat[i,])
      normCost_mat[i,]<- normCost_mat[i,]+(-min(normCost_mat[i,])*2)
    }
  }
  normCost_vec <- as.vector(t(normCost_mat))
  
  normLiquidity_mat <- liquidity_mat
  for(i in 1:callNum){
    if(length(unique(liquidity_mat[i,]))==1){
      normLiquidity_mat[i,]<-1
    }else{
      normLiquidity_mat[i,]<- scale(liquidity_mat[i,])
      normLiquidity_mat[i,]<- normLiquidity_mat[i,]+(-min(normLiquidity_mat[i,])*2)
    }
  }
  normLiquidity_vec <- as.vector(t(normLiquidity_mat))
  
  normLiquidity_vec <- as.vector(t(normLiquidity_mat))
  normOperation_mat <- operation_mat*9+1
  normOperation_vec <- as.vector(t(normOperation_mat))
  
  objParams_list <- list(cost_vec=normCost_vec,cost_mat=normCost_mat,
                         liquidity_vec=normLiquidity_vec,liquidity_mat=normLiquidity_mat,
                         operation_vec=normOperation_vec,operation_mat=normOperation_mat)
  
  return(objParams_list)
}

DeriveOptimalAssetsV2 <- function(minUnitQuantity_mat,eli_mat,callAmount_mat,haircut_mat,minUnitValue_mat,
                                  pref_vec,objParams_list,callId_vec,resource_vec){
  callNum <- length(callId_vec); resourceNum <- length(resource_vec)
  normCost_mat <- objParams_list$cost_mat
  normLiquidity_mat <- objParams_list$liquidity_mat 
  
  optimal_mat <- normCost_mat*pref_vec[1]+normLiquidity_mat*pref_vec[2]
  colnames(optimal_mat) <- resource_vec
  rownames(optimal_mat)<-callId_vec
  
  optimalAsset_mat <- matrix(c(callId_vec,rep('', callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callId','resource')))
  
  tempMinUnitQuantity_mat <- minUnitQuantity_mat
  for(i in 1:callNum){
    idx1 <- which(eli_mat[i,]!=0)  # return elegible asset idx for mc[i]
    temp_mat <- matrix(c(optimal_mat[i,idx1],idx1),nrow=2,byrow = T) # combine the asset cost and index together
    # sort the asset per call by cost
    if(length(temp_mat[1,])==1){       # if there's only one eligible asset, no need to sort.
      sortOptimal_mat=temp_mat
    }else{
      sortOptimal_mat<-temp_mat[,order(temp_mat[1,])] # sort the cost, return the cost and asset idx in matrix
    }
    # if there are more than one assets have the same score, we cannot simply select the first one
    # because this may cause the case that there are 3 assets have the same score for 3 calls
    # if we just select the first asset, then it's possible this single asset is not sufficient to fulfill 
    # all these 3 calls, but these three assets can fulfill one of the call respectively
    
    # selecting order:
    # select the one which hasn't been selected to the previous call
    # unless, they are from the same margin statment (deal with that in OW-379)
    # Best approach, allocate the most sufficient asset to the largest call amount, deal with that later
    # better to deal with that now
    idxMinScore_vec <- sortOptimal_mat[2,which(round(sortOptimal_mat[1,],2)==round(min(sortOptimal_mat[1,]),2))]
    # if idxMinScore_vec contains only one element, don't need to sort
    if(length(idxMinScore_vec) > 1){
      optimalResource_vec <- resource_vec[idxMinScore_vec]
      
      # temp.largestAmount.asset: the least score assets score and index(>=1)
      largestAmountResource_vec <- matrix(c(tempMinUnitQuantity_mat[i,idxMinScore_vec]*minUnitValue_mat[i,idxMinScore_vec],idxMinScore_vec),nrow=2,byrow=T)
      if(length(largestAmountResource_vec[1,])>1){  
        largestAmountResource_vec <- largestAmountResource_vec[,order(largestAmountResource_vec[1,],decreasing=T)]
        # substitute in sortOptimal_mat
        sortOptimal_mat[,1:length(largestAmountResource_vec[1,])]<- largestAmountResource_vec
        colnames(sortOptimal_mat)[1:length(largestAmountResource_vec[1,])] <- colnames(largestAmountResource_vec)
      }
    }
    optimalAsset_mat[i,2] <- resource_vec[sortOptimal_mat[2,1]]
    tempMinUnitQuantity <- tempMinUnitQuantity_mat[,sortOptimal_mat[2,1]]
    tempMinUnitQuantity_mat[,sortOptimal_mat[2,1]]<- tempMinUnitQuantity-callAmount_mat[i,1]/(1-haircut_mat[i,1])/minUnitValue_mat[,sortOptimal_mat[2,1]]
    #for(m in 1:length(idxMinScore_vec)){
    #  if(!is.element(temp.optimal.asset[m],optimalAsset_mat[,2])){
    #    optimalAsset_mat[i,2] <- temp.optimal.asset[m]
    #    break
    #  }
    #}
    # if all possible assets have been selected as optimal of previous margin calls
    # then, select the first asset
    if(optimalAsset_mat[i,2]==''){
      optimalAsset_mat[i,2] <- optimalResource_vec[1]
    }
  }
  return(optimalAsset_mat)
}

DeriveOptimalAssetsV1 <- function(minUnitQuantity_mat,eli_mat,callAmount_mat,haircut_mat,minUnitValue_mat,
                                  pref_vec,objParams_list,callId_vec,resource_vec){
  callNum <- length(callId_vec); resourceNum <- length(resource_vec)
  normCost_mat <- objParams_list$cost_mat
  normLiquidity_mat <- objParams_list$liquidity_mat 
  normOperation_mat <- objParams_list$operation_mat
  
  optimal_mat <- normOperation_mat*pref_vec[3]+normLiquidity_mat*pref_vec[2]+normCost_mat*pref_vec[1]
  colnames(optimal_mat) <- resource_vec; rownames(optimal_mat)<-callId_vec
  
  optimalAsset_mat <- matrix(c(callId_vec,rep('', callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callId','assetCustacId')))
  
  tempMinUnitQuantity_mat <- minUnitQuantity_mat
  for(i in 1:callNum){
    idx1 <- which(eli_mat[i,]!=0)  # return elegible asset idx for mc[i]
    temp_mat <- matrix(c(optimal_mat[i,idx1],idx1),nrow=2,byrow = T) # combine the asset cost and index together
    # sort the asset per call by cost
    if(length(temp_mat[1,])==1){       # if there's only one eligible asset, no need to sort.
      sortOptimal_mat=temp_mat
    }else{
      sortOptimal_mat<-temp_mat[,order(temp_mat[1,])] # sort the cost, return the cost and asset idx in matrix
    }
    # if there are more than one assets have the same score, we cannot simply select the first one
    # because this may cause the case that there are 3 assets have the same score for 3 calls
    # if we just select the first asset, then it's possible this single asset is not sufficient to fulfill 
    # all these 3 calls, but these three assets can fulfill one of the call respectively
    
    # selecting order:
    # select the one which hasn't been selected to the previous call
    # unless, they are from the same margin statment (deal with that in OW-379)
    # Best approach, allocate the most sufficient asset to the largest call amount, deal with that later
    # better to deal with that now
    # round to 2 digits
    idxMinScore_vec <- sortOptimal_mat[2,which(round(sortOptimal_mat[1,],2)==round(min(sortOptimal_mat[1,]),2))]
    # if idxMinScore_vec contains only one element, don't need to sort
    if(length(idxMinScore_vec) > 1){
      optimalResource_vec <- resource_vec[idxMinScore_vec]
      
      # temp.largestAmount.asset: the least score assets score and index(>=1)
      largestAmountResource_vec <- matrix(c(tempMinUnitQuantity_mat[i,idxMinScore_vec]*minUnitValue_mat[i,idxMinScore_vec],idxMinScore_vec),nrow=2,byrow=T)
      if(length(largestAmountResource_vec[1,])>1){  
        largestAmountResource_vec <- largestAmountResource_vec[,order(largestAmountResource_vec[1,],decreasing=T)]
        # substitute in sortOptimal_mat
        sortOptimal_mat[,1:length(largestAmountResource_vec[1,])]<- largestAmountResource_vec
        colnames(sortOptimal_mat)[1:length(largestAmountResource_vec[1,])] <- colnames(largestAmountResource_vec)
      }
    }
    optimalAsset_mat[i,2] <- resource_vec[sortOptimal_mat[2,1]]
    tempMinUnitQuantity <- tempMinUnitQuantity_mat[,sortOptimal_mat[2,1]]
    tempMinUnitQuantity_mat[,sortOptimal_mat[2,1]]<- tempMinUnitQuantity-callAmount_mat[i,1]/(1-haircut_mat[i,1])/minUnitValue_mat[,sortOptimal_mat[2,1]]
    #for(m in 1:length(idxMinScore_vec)){
    #  if(!is.element(temp.optimal.asset[m],optimalAsset_mat[,2])){
    #    optimalAsset_mat[i,2] <- temp.optimal.asset[m]
    #    break
    #  }
    #}
    # if all possible assets have been selected as optimal of previous margin calls
    # then, select the first asset
    if(optimalAsset_mat[i,2]==''){
      optimalAsset_mat[i,2] <- optimalResource_vec[1]
    }
  }
  return(optimalAsset_mat)
}

VarInfo <- function(eli_vec,callInfo_df,resource_vec,callId_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  idxEli_vec <- which(eli_vec==1)
  
  # matrix store the index number, by row
  idx_mat <- matrix(1:(callNum*resourceNum),nrow=callNum,byrow = TRUE,dimnames = list(callId_vec,resource_vec))
  # matrix store the variable name("msId_mcId_assetCustId"), by row
  fullNameOri_mat <-  matrix('',nrow=callNum,ncol=resourceNum,byrow = TRUE,dimnames = list(callId_vec,resource_vec))
  
  # new dummy for "msId_assetCustId"
  newNameOri_mat <- matrix('',nrow=callNum,ncol=resourceNum,byrow = TRUE,dimnames = list(callId_vec,resource_vec))
  
  
  for(i in 1:callNum){
    msId <- callInfo_df$marginStatement[i]
    fullNameOri_mat[i,]<-PasteVarName(msId,callId_vec[i],resource_vec)
    newNameOri_mat[i,] <- PasteVarName(msId,'dummy',resource_vec)
  }
  varNameOri_vec <- t(fullNameOri_mat)[idxEli_vec]
  newNameOri_vec <- t(newNameOri_mat)[idxEli_vec]
  
  # remove duplicated variables constructed by same asset in one statement for different calls
  newNameDummy_vec <- unique(newNameOri_vec) 
  
  # use the dummies to construct the indicator
  # same number means same asset for same statement: 1,2,3,4,1,2,4,5,...
  pos_vec <- match(newNameOri_vec,newNameDummy_vec)
  
  varName_vec <- c(varNameOri_vec,newNameDummy_vec)
  
  varNum <- length(varNameOri_vec)
  varNum2 <- length(varName_vec)
  
  var_list <- list(varName_vec=varName_vec,varNum=varNum,varNum2=varNum2,pos_vec=pos_vec)
  return(var_list)
}

#### qtyFunctions #### 
UpdateQtyOriInResourceDf <- function(resource_df){
  ## will be called at the very end of the allocation
  quantity_vec <- resource_df$qtyMin*resource_df$minUnit + resource_df$qtyRes
  resource_df$qtyOri <- quantity_vec
  return(resource_df)
}

ResetQtyMinInResourceDf <- function(resource_df){
  resource_df$qtyMin <- floor(resource_df$qtyOri/resource_df$minUnit)
  return(resource_df)
}

UsedQtyFromResultList <- function(result_list,resource_vec,callId_vec){
  #### minUnitQuantity of resources used for allocation
  quantityUsed_vec <- rep(0,length(resource_vec))
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  for(i in 1:callNum){
    callId <- callId_vec[i]
    alloc_df <- result_list[[callId]]
    resourceTemp_vec <- PasteResource(alloc_df$Asset,alloc_df$CustodianAccount)
    idxInRes_vec <- na.omit(match(resourceTemp_vec,resource_vec))
    if(length(idxInRes_vec)!=0){
      idxInAlloc <- match(resource_vec[idxInRes_vec],resourceTemp_vec)
      quantityUsed_vec[idxInRes_vec] <- quantityUsed_vec[idxInRes_vec]+alloc_df$Quantity[idxInAlloc]
    }
  }
  return(quantityUsed_vec)
}

#### staticFunctions #### 
PasteFun1 <- function(x1='',x2=''){
  temp=paste(x1,x2,sep='_',collapse = '')
  return(temp)
}

PasteFun2 <- function(x){
  temp=paste(x,collapse='_')
  return(temp)
}

PasteResource <- function(assetId_vec,custodianAccount_vec){
  temp <- paste(assetId_vec,custodianAccount_vec,sep='---')
  return(temp)
}

PasteVarName <- function(msId_vec,callId_vec,resource_vec){
  temp <- paste(msId_vec,callId_vec,resource_vec,sep='___')
  return(temp)
}

SplitResource <- function(resource_vec,target){
  resource_mat <- matrix(unlist(strsplit(resource_vec,'---')),nrow=2)
  if(missing(target)){
    target <- 'all'
  }
  if(target=='asset'){
    return(resource_mat[1,])
  } else if(target=='custodianAccount'){
    return(resource_mat[2,])
  } else{
    return(resource_mat)
  }
}

SplitVarName <- function(varName_vec,target){
  varName_mat <- matrix(unlist(strsplit(varName_vec,'___')),nrow=3)
  if(missing(target)){
    target <- 'all'
  }
  if(target=='call'){
    return(varName_mat[2,])
  } else if(target=='ms'){
    return(varName_mat[1,])
  } else if(target=='resource'){
    return(varName_mat[3,])
  } else if(target=='all'){
    return(varName_mat)
  } else{
    stop('ALERR3001: Invalid varName_vec splitor!')
  }
}

renjinFix <- function(frame, name) {
  d <- data.frame(frame);
  colnames(d) <- gsub(name, "", colnames(d));
  return(d);
}

BaseMat0 <- function(callId_vec,resource_vec){
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  base_mat <- matrix(0,nrow=callNum,ncol=resourceNum, dimnames = list(callId_vec,resource_vec))
  return(base_mat)
}
