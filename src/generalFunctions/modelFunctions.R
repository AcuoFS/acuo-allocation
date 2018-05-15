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
  scaleFactor_vec <- quantity_vec+1
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
  leftCallAmount <- callAmount
  integralSuffQty_vec <- ceiling(callAmount/(1-haircut_vec)/minUnitValue_vec)
  suffIdx_vec <- which(quantity_vec >= integralSuffQty_vec)
  if(length(suffIdx_vec)>=1){
    suffResource_vec <- resource_vec[suffIdx_vec]
    tempIdx <- which.max(rank_vec[suffIdx_vec])
    optimalResource <- suffResource_vec[tempIdx]
    optimalIdx <- which(resource_vec==optimalResource)
    quantity <- integralSuffQty_vec[optimalIdx]
    solution_vec[optimalIdx] <- quantity
  } else{
    if(operLimit<=1){ # operLimit=1
      errormsg <- paste('ALERR2004: It is not sufficient to allocate',floor(operLimit),'assets for',callId)
      stop(errormsg)
    } else{
      for(i in 1:operLimit){
        amount_vec <- floor(quantity_vec)*(1-haircut_vec)*minUnitValue_vec
        suffIdx_vec <- which(amount_vec >= callAmount)
        if(length(suffIdx_vec)==0){
          oriIdx_vec <- which(amount_vec==amount_vec[which.max(amount_vec)])
          tempIdx <- which.max(rank_vec[oriIdx_vec])
          optimalResource <- resource_vec[oriIdx_vec[tempIdx]]
          optimalIdx <- which(resource_vec==optimalResource)
          adjAmount <- quantity_vec[optimalIdx]*minUnitValue_vec[optimalIdx]*(1-haircut_vec[optimalIdx])
          if(leftCallAmount>adjAmount){
            quantity <- quantity_vec[optimalIdx]
            solution_vec[optimalIdx] <- quantity
            quantity_vec[optimalIdx] <- 0
            amount_vec <- floor(quantity_vec)*(1-haircut_vec)*minUnitValue_vec
            leftCallAmount <- leftCallAmount-adjAmount
          } else{
            quantity <- ceiling(leftCallAmount/(1-haircut_vec[optimalIdx])/minUnitValue_vec[optimalIdx])
            solution_vec[optimalIdx] <- quantity
            quantity_vec[optimalIdx] <- quantity_vec[optimalIdx] - quantity
            amount_vec <- floor(quantity_vec)*(1-haircut_vec)*minUnitValue_vec
            leftCallAmount <- leftCallAmount-adjAmount
            break
          }
        }
      }
      if(leftCallAmount > 0){
        errormsg <- paste('ALERR2004: It is not sufficient to allocate',floor(operLimit),'assets for',callId)
        stop(errormsg)
      }
    }
  }
  return(solution_vec)
}

ConstructModelObjV1 <- function(integerCallAmount_mat,costBasis_mat,resourceLiquidity_vec, eli_mat,callInfo_df,resource_vec){
  callId_vec <- callInfo_df$id
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  #### calculate the cost
  cost_mat <- integerCallAmount_mat*costBasis_mat
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
  
  #### liquidity
  liquidity_mat <- matrix(rep(resourceLiquidity_vec,callNum),nrow=callNum,byrow=TRUE,dimnames=list(callId_vec,resource_vec)) 
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
  
  #### operation 
  operation_mat <- matrix(rep(1,resourceNum*callNum),nrow=callNum,byrow=TRUE,dimnames=list(callId_vec,resource_vec)) 
  assetId_vec <- SplitResource(resource_vec,'asset') 
  for(i in 1:callNum){
    idxCcy <- which(callInfo_df$currency[i]==assetId_vec)    # return the index of mc[i] currency cash in the asset list
    idx1 <- which(eli_mat[i,]!=0)             # return elegible asset idx for mc[i]
    if(length(idxCcy)==1 && is.element(idxCcy,idx1)){  # if there exist call currency cash in the inventory, and it's available
      operation_mat[i,idxCcy] <- 0
    }
  }
  normOperation_mat <- operation_mat*9+1
  normOperation_vec <- as.vector(t(normOperation_mat))
  
  objParams_list <- list(cost_vec=normCost_vec,cost_mat=normCost_mat,
                         liquidity_vec=normLiquidity_vec,liquidity_mat=normLiquidity_mat,
                         operation_vec=normOperation_vec,operation_mat=normOperation_mat)
  
  return(objParams_list)
}

ConstructModelObjV2 <- function(integerCallAmount_mat,costBasis_mat,resourceLiquidity_vec,callId_vec,resource_vec){
  
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  #### calculate the cost
  cost_mat <- integerCallAmount_mat*costBasis_mat
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
  
  #### liquidity
  liquidity_mat <- matrix(rep(resourceLiquidity_vec,callNum),nrow=callNum,byrow=TRUE,dimnames=list(callId_vec,resource_vec)) 
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
  
  objParams_list <- list(cost_vec=normCost_vec,cost_mat=normCost_mat,
                         liquidity_vec=normLiquidity_vec,liquidity_mat=normLiquidity_mat)
  
  return(objParams_list)
}

DeriveOptimalAssetsV2 <- function(quantity_vec,callAmount_vec,minUnitValue_vec,eli_mat,haircut_mat,
                                  costScore_mat,liquidityScore_mat,pref_vec,callId_vec,resource_vec){
  
  objCoef_mat <- CalculateObjParams(costScore_mat,liquidityScore_mat,pref_vec,"amount")
  
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  optimal_mat <- objCoef_mat
  colnames(optimal_mat) <- resource_vec
  rownames(optimal_mat) <- callId_vec
  
  optimalAsset_mat <- matrix(c(callId_vec,rep('', callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callId','resource')))
  
  callAmount_mat <- matrix(rep(callAmount_vec,resourceNum),nrow=callNum,byrow=F)
  minUnitValue_mat <- matrix(rep(minUnitValue_vec,callNum),nrow=callNum,byrow=T)
  minUnitQuantity_mat <- matrix(rep(quantity_vec,callNum),nrow=callNum,byrow=T)
  tempMinUnitQuantity_mat <- minUnitQuantity_mat
  for(i in 1:callNum){
    idx1 <- which(eli_mat[i,]!=0)  # return elegible asset idx for mc[i]
    temp_mat <- matrix(c(optimal_mat[i,idx1],idx1),nrow=2,byrow = T) # combine the asset score and index together
    # sort the asset per call by score
    if(length(temp_mat[1,])==1){       # if there's only one eligible asset, no need to sort.
      sortOptimal_mat=temp_mat
    }else{
      sortOptimal_mat<-temp_mat[,order(temp_mat[1,])] # sort the score, return the cost and asset idx in matrix
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

VarInfo <- function(idxEli_vec,callInfo_df,resource_vec){
  callId_vec <- callInfo_df$id
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
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

EliMat <- function(availAsset_df,callId_vec,resource_vec){
  # Construct eligibility matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df
  #         columns to be used: callId, assetCustacId
  # Returns:
  #   haircut matrix
  
  eli_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                    dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  eli_mat[cbind(idxCallId_vec,idxResource_vec)] <- 1
  return(eli_mat)
}

HaircutCVec2Mat <- function(haircutC_vec,availAsset_df,callId_vec,resource_vec){
  # Construct Collateral haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df(columns to be used: haircut, callId, assetCustacId)
  #
  # Returns:
  #   haircut matrix
  
  haircutC_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                         dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  haircutC_mat[cbind(idxCallId_vec,idxResource_vec)] <- haircutC_vec
  return(haircutC_mat)
}

HaircutFXVec2Mat <- function(haircutFX_vec,availAsset_df,callId_vec,resource_vec){
  # Construct FX haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df
  #         columns to be used: haircutFX, callId, assetCustacId
  # Returns:
  #   haircut matrix
  
  haircutFX_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                          dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  haircutFX_mat[cbind(idxCallId_vec,idxResource_vec)] <- haircutFX_vec
  return(haircutFX_mat)
}

HaircutVec2Mat <- function(haircut_vec,availAsset_df,callId_vec,resource_vec){
  # Construct total haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df(columns to be used: haircutC, haircutFX, callId, assetCustacId)
  #
  # Returns:
  #   haircut matrix
  
  haircut_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                        dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  haircut_mat[cbind(idxCallId_vec,idxResource_vec)] <- haircut_vec
  return(haircut_mat)
}

CostVec2Mat <- function(cost_vec,availAsset_df,callId_vec,resource_vec){
  # Construct haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df(columns to be used: all)
  #       resource_df(columns to be used: all)
  #         
  # Returns:
  #   haircut matrix
  
  cost_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                     dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  cost_mat[cbind(idxCallId_vec,idxResource_vec)] <- cost_vec
  return(cost_mat)
}

CheckAssetSufficiency <- function(eli_mat,haircut_mat,quantity_vec,minUnitValue_vec,callAmount_vec){
  # check whether the assets are sufficient 
  # per each call & all calls
  #
  # Args: 
  #
  # Returns:
  #   true or false
  suffPerCall <- all((eli_mat*(1-haircut_mat)) %*% (quantity_vec*minUnitValue_vec) > callAmount_vec)
  suffAllCall <- sum(quantity_vec*minUnitValue_vec*(1-apply(haircut_mat,2,max))) > sum(callAmount_vec)
  return(suffPerCall&suffAllCall)
}

CheckOptimalAssetSufficiency <- function(optimalAsset_mat,assetSuffQty_mat,resource_df){
  # check whether the optimal assets are sufficeint for the calls
  #
  # Returns:
  #   true or false
  selectUniqueAsset_vec <- unique(optimalAsset_mat[,2]) 
  ifSelectAssetSuff_vec <- rep(0,length(selectUniqueAsset_vec))
  
  for(i in 1:length(selectUniqueAsset_vec)){
    resource <- selectUniqueAsset_vec[i]
    idxCall_vec <- which(optimalAsset_mat[,2]==resource) 
    idxResource <- which(resource_df$id==resource)
    
    ifSelectAssetSuff_vec[i] <- 1*(sum(assetSuffQty_mat[idxCall_vec,idxResource]) < resource_df$qtyMin[idxResource])
  }
  isSuff <- sum(ifSelectAssetSuff_vec)==length(selectUniqueAsset_vec)
  return(isSuff)
}

GenerateStandardizedCostMat <- function(integerCallAmount_mat,costBasis_mat,callId_vec,resource_vec){
  
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  #### calculate the cost
  cost_mat <- integerCallAmount_mat*costBasis_mat
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
  return(normCost_mat)
}

GenerateStandardizedLiquidityMat <- function(resourceLiquidity_vec,callId_vec,resource_vec){
  #### liquidity
  callNum <- length(callId_vec)
  liquidity_mat <- matrix(rep(resourceLiquidity_vec,callNum),nrow=callNum,byrow=TRUE,dimnames=list(callId_vec,resource_vec)) 
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
  return(liquidity_mat)
}

AllocateUnderSufficientOptimalAssets <- function(optimalAsset_mat,assetSuffQty_mat,callId_vec,resource_vec){
  
  result_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),dimnames=list(callId_vec,resource_vec))
  for(k in 1:length(callId_vec)){
    idx <- which(optimalAsset_mat[,1]==callId_vec[k])
    resource <- optimalAsset_mat[idx,2]
    idxResource <- which(resource_vec==resource)
    result_mat[k,idxResource] <- assetSuffQty_mat[idx,idxResource]
  }
  solverStatus <- -1
  solverObjValue <- -1
  return(result_mat)
}

AllocateUnderInsufficientOptimalAssets <- function(costScore_mat,liquidityScore_mat,pref_vec,eli_mat,
                                                   callInfo_df,resource_vec,resource_df,
                                                   minMoveValue,operLimit,operLimitMs_vec,fungible,timeLimit,
                                                   allocated_list,initAllocation_list){
  idxEli_vec <- which(t(eli_mat)==1)         # eligible index
  callNum <- length(callInfo_df$id)
  callAmount_mat <- matrix(rep(callInfo_df$callAmount,resourceNum),nrow=callNum,byrow=F)
  callAmount_vec <- as.vector(t(callAmount_mat))[idxEli_vec]
  
  quantity_mat <- matrix(rep(resource_df$qtyMin,callNum),nrow=callNum,byrow=T)
  quantity_vec <- as.vector(t(quantity_mat))[idxEli_vec]
  
  minUnitValue_mat <- matrix(rep(resource_df$minUnitValue, callNum),nrow=callNum,byrow=T)
  minUnitValue_vec <- as.vector(t(minUnitValue_mat))[idxEli_vec]
  
  #### Construct Variable Names ######
  varInfo_list <- VarInfo(idxEli_vec,callInfo_df,resource_vec)
  
  varName_vec <- varInfo_list$varName_vec
  varNum <- varInfo_list$varNum
  varNum2 <- varInfo_list$varNum2
  pos_vec <- varInfo_list$pos_vec
  
  #### Build the Optimization Model #######
  
  #### OBJECTIVE FUNCTION
  objCoef_mat <- CalculateObjParams(costScore_mat,liquidityScore_mat,pref_vec,"unit",minUnitValue_vec)
  fObj_vec <- c(t(objCoef_mat)[idxEli_vec],rep(0,varNum2-varNum))
  
  names(fObj_vec) <- varName_vec
  
  #### CONSTRAINTS
  
  # deduct some units to avoid overflow in the rounding later
  oriQuantity_vec <- quantity_vec
  quantity_vec <- quantity_vec - max(1,(callNum-1))
  
  fCon2_list <- QtyConst(varName_vec,varNum,resource_vec,resource_df$qtyMin)
  fCon3_list <- MarginConst(varName_vec,varNum,minUnitValue_vec,haircut_vec,callInfo_df$id,callInfo_df$callAmount)
  if(ifNewAlloc){
    fCon4_list <- DummyConst(varName_vec,varNum,quantity_vec,callAmount_vec,minUnitValue_vec)
    fCon5_list <- MoveConst(varName_vec,varNum,operLimit,operLimitMs_vec,fungible)
  } else{
    allocated_vec <- ResultList2Vec(allocated_list,callId_vec,minUnit_vec,varName_vec,varNum,pos_vec)
    allocatedDummy_vec <- allocated_vec[(varNum+1):varNum2]
    fCon4_list <- DummyConstInherit(allocatedDummy_vec,varName_vec,varNum,quantity_vec,callAmount_vec,minUnitValue_vec)
    fCon5_list <- MoveConstInherit(allocatedDummy_vec,varName_vec,varNum,operLimit,operLimitMs_vec,fungible)
  }
  
  #### Build the Optimization Model END ########
  
  #### Solver Inputs Start #####################
  # minimum movement quantity of each asset
  minMoveQty_vec <- ceiling(minMoveValue/minUnitValue_vec)
  minMoveQty_vec <- pmin(minMoveQty_vec,quantity_vec)
  
  if(length(callAmount_vec[which(minMoveValue > callAmount_vec/(1-haircut_vec))])!=0){
    idxTemp <- which(minMoveValue > callAmount_vec/(1-haircut_vec))
    callEli_vec <- callAmount_vec/(1-haircut_vec)
    minMoveQty_vec[idxTemp] <- ceiling(callEli_vec[idxTemp]/minUnitValue_vec[idxTemp])
  }
  
  lpObj_vec <- fObj_vec
  lpCon_mat <- rbind(fCon2_list$fCon2_mat,fCon3_list$fCon3_mat,fCon4_list$fCon4_mat,fCon5_list$fCon5_mat)
  lpDir_vec <- c(fCon2_list$fDir2_vec,fCon3_list$fDir3_vec,fCon4_list$fDir4_vec,fCon5_list$fDir5_vec)
  lpRhs_vec <- c(fCon2_list$fRhs2_vec,fCon3_list$fRhs3_vec,fCon4_list$fRhs4_vec,fCon5_list$fRhs5_vec)
  
  
  lpKind_vec <- rep('semi-continuous',varNum2)
  lpType_vec <- rep('real',varNum2)
  #lpType_vec[which(minUnitValue_vec>=1)] <- 'integer'
  lpType_vec[(varNum+1):varNum2] <- 'integer'
  lpLowerBound_vec <- c(minMoveQty_vec,rep(0,varNum2-varNum))
  
  varNameResource_vec <- SplitVarName(varName_vec,'resource') # resource in varName
  for(k in 1:resourceNum){
    resourceTemp <- resource_vec[k]
    idxTemp_vec <- which(varNameResource_vec==resourceTemp)
    lowerSumTemp <- sum(lpLowerBound_vec[idxTemp_vec])
    if(lowerSumTemp > resource_df$qtyMin[k]){
      lpLowerBound_vec[idxTemp_vec] <- 0
    }
  }
  
  lpUpperBound_vec <- c(quantity_vec,rep(1,varNum2-varNum))
  lpBranchMode_vec <- c(rep('auto',varNum),rep('auto',varNum2-varNum))
  
  #### Control options
  lpPresolve <- ifelse(callNum<=10,'none','knapsack')
  lpTimeout <- timeLimit
  
  #### INITIAL GUESS BASIS 
  lpGuessBasis_vec <- rep(0,varNum2)
  if(!missing(initAllocation_list)){
    # the initial guess must be a feasible point
    lpGuessBasis_vec<-ResultList2Vec(initAllocation_list,callId_vec,minUnit_vec,varName_vec,varNum,pos_vec)
  }
  #### Solver Inputs END ###################
  
  #### Solve the Model Start ###############
  #### call lpSolve solver
  solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                                   lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                                   lpGuessBasis_vec=lpGuessBasis_vec, 
                                   presolve=lpPresolve,timeout=lpTimeout)
  #### solver outputs
  solverStatus<- solverOutput_list$resultStatus
  solverSolution_vec <- solverOutput_list$solverSolution_vec
  solverObjValue <- solverOutput_list$solverObjValue
  #### Solve the Model END #################
  
  
  #### Exception Start ####
  errStatus <- c(2,5,6,7,10,13)
  if(solverStatus==2){
    errormsg <- paste("ALERR2005: The model constructed by margin calls",paste(callId_vec,collapse = " "),"is infeasible")
    stop(errormsg)
  } else if(is.element(solverStatus,errStatus)){
    if(callNum==1){
      rank_vec <- objParams_list$cost_mat*pref_vec[1]+objParams_list$liquidity_mat*pref_vec[2]
      callAmount <- callInfo_df$callAmount
      solverSolution_vec <- AllocateByRank(resource_vec[idxEli_vec],callInfo_df$id,rank_vec,callAmount,quantity_vec[idxEli_vec],minUnitValue_vec[idxEli_vec],haircut_vec[idxEli_vec],operLimit)
    } else{ # Solver time out
      #### choose the best alternative
      solverSolution_vec <- lpGuessBasis_vec
    }
  }
  #### Exception END ######
  
  quantity_vec <- oriQuantity_vec
  
  #### Adjust & Convert the Solver Result Start ######
  solverSolution_vec <- AdjustResultVec(solverSolution_vec,varNum,varName_vec,fCon4_list$fCon4_mat,
                                        callAmount_vec,quantity_vec,minUnitValue_vec)
  
  result_mat <- ResultVec2Mat(solverSolution_vec,callId_vec,resource_vec,idxEli_vec,varNum)
  #### Adjust & Convert the Solver Result END ######## 
  
  return(result_mat)
}

PrintCallAllocatedAmount <- function(callAmount_vec,callId_vec,callSelect_list){
  # print the call amount and the allocated amount in a matrix
  # no return
  callAllocated_mat<- matrix(c(callInfo_df$callAmount,rep(0, callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callAmount','allocatedAmount')))
  for(i in 1:callNum){
    callAllocated_mat[i,2] <- sum(callSelect_list[[callId_vec[i]]]$`NetAmount(USD)`)
  }
  print(callAllocated_mat)
}

CalculateObjParams <- function(cost,liquidity,pref_vec,unit,minUnitValue){
  # calculate the weighted objectives parameters
  # per 1 unit or per 1 dollar value of a resource
  # 
  # Args:
  #   unit: the unit of the decision variable
  #         "quantity" means per 1 unit of a resource; "amount" means per 1 dollar value of a resource
  #
  pref_vec <- pref_vec/sum(pref_vec[1:2]) # Recalculate the parameters weight setting
  
  if(unit == "quantity"){
    weightedScore <- pref_vec[1]*cost*minUnitValue + pref_vec[2]*liquidity*minUnitValue
  } else if(unit == "amount"){
    weightedScore <- pref_vec[1]*cost + pref_vec[2]*liquidity
  }
  return(weightedScore)
}
