
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
      errormsg <- paste('It is not sufficient to allocate',floor(operLimit),'assets for',callId,'!')
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
  
  #costBasis_mat <- costBasis_mat/(1-haircut_mat)
  costBasis_vec <- as.vector(t(costBasis_mat))
  
  assetLiquidity_vec <- apply((1-haircut_mat*eli_mat)^2,2,min) # define asset liquidity
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
  newNameDummy_vec <- unique(newNameOri_vec)
  
  varName_vec <- c(varNameOri_vec,newNameDummy_vec)
  
  varNum <- length(varNameOri_vec)
  varNum2 <- length(varName_vec)
  
  var_list <- list(varName_vec=varName_vec,varNum=varNum,varNum2=varNum2)
  return(var_list)
}
