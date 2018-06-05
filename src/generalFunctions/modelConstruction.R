
DeriveMinMoveQty <- function(minMoveValue,quantity_vec,minUnitValue_vec,callAmount_vec,haircut_vec){
  # Derive the minimum quantity of a resource for a single movement by minimum amount of a movement
  # 
  # minMoveValue: the minimum amount of a resource (in dollar) you can use for a call
  #
  # minMoveQty: the minimum quantity of a resource you can use for a call
  #   minMoveQty = minMoveValue/minUnitValue, 
  # Unless 
  #   1. the quantity of the resource is less than minMoveQty 
  #      minMoveQty = resource quantity
  #   2. the quantity of the resource needed for a call is less than minMoveQty
  #      minMoveQty = resource quantity needed for the call
  #
  # Returns:
  #   A vector of the resource minimum movement quantity
  
  # Derive minimum movement quantity by minimum movement value
  minMoveQty_vec <- ceiling(minMoveValue/minUnitValue_vec)
  
  # 1. In case resource quantity is less than minMoveQty
  minMoveQty_vec <- pmin(minMoveQty_vec,quantity_vec)
  
  # 2. In case quantity needed for a call amount is less than minMoveQty
  if(length(callAmount_vec[which(minMoveValue > callAmount_vec/(1-haircut_vec))])!=0){
    idxTemp <- which(minMoveValue > callAmount_vec/(1-haircut_vec))
    callEli_vec <- callAmount_vec/(1-haircut_vec)
    minMoveQty_vec[idxTemp] <- ceiling(callEli_vec[idxTemp]/minUnitValue_vec[idxTemp])
  }
  return(minMoveQty_vec)
}

DeriveLowerBound <- function(minMoveValue,varName_vec,quantity_vec,minUnitValue_vec,callAmount_vec,haircut_vec){
  # Derive the lower bound of the decision variables
  #   lower bound for quantity variables = minMoveQty
  #   lower bound for dummy variables = 0
  #
  # *minMoveQty: minimum movement quantity of a resource to a call
  #
  # Returns
  #   A vector of decision variables lower bound
  
  qtyVarNum <- GetQtyVarNum(varName_vec)
  
  ## Derive minimum movement quantity by minimum movement value
  minMoveQty_vec <- DeriveMinMoveQty(minMoveValue,quantity_vec,minUnitValue_vec,callAmount_vec,haircut_vec)
  
  ## Initialize lower bound vector
  lowerBound_vec <- c(minMoveQty_vec,rep(0,length(varName_vec)-qtyVarNum))
  
  ## Extract the resource id from decision variables' names
  varNameResource_vec <- SplitVarName(varName_vec,'resource')[1:qtyVarNum]
  
  return(lowerBound_vec)
}

QtyConst <- function(varName_vec,qtyVarNum,resource_vec,quantity_vec){
  # Quantity constraints, constraint number = resource number
  #   The quantity to allocate of a resource <= the quantity of the resource
  #
  # *Coefficient matrix:
  #   row number is the number of constraints; col number is the number of decision variables
  # 
  # Returns:
  #   A list contains constraint's coefficient matrix, direction vector, and right hand side vector 
  
  ## Extract the resource id from decision variables' names
  varNameResource_vec <- SplitVarName(varName_vec,'resource')[1:qtyVarNum]
  
  ## Coefficient matrix
  coef_mat <- matrix(0,nrow=length(resource_vec),ncol=length(varName_vec))
  # assign 1: can use an example to illustrate
  coef_mat[cbind(match(varNameResource_vec,resource_vec),1:qtyVarNum)] <- 1
  
  ## dirction vector, right hand side value vector
  dir_vec <- rep('<=',length(resource_vec))
  rhs_vec <- quantity_vec
  
  con_list <- list(coef_mat=coef_mat,dir_vec=dir_vec,rhs_vec=rhs_vec)
  
  return(con_list)
}

MarginConst <- function(varName_vec,qtyVarNum,minUnitValue_vec,haircut_vec,callId_vec,callAmount_vec){
  # Margin constraints, constraint number = call number
  #   The amount allocated to a call(SUM(minUnitValue*haircut*variables)) >= the call amount
  #
  # *Coefficient matrix:
  #   row number is the number of constraints; col number is the number of decision variables
  # 
  # Returns:
  #   A list contains constraint's coefficient matrix, direction vector, and right hand side vector 
  
  ## Extract the call id from decision variables' names
  varNameCall_vec <- SplitVarName(varName_vec,'call')[1:qtyVarNum]
  
  ## Coefficient matrix
  coef_mat <- matrix(0,nrow=length(callId_vec),ncol=length(varName_vec))
  coef_mat[cbind(match(varNameCall_vec,callId_vec),1:qtyVarNum)] <- minUnitValue_vec*(1-haircut_vec)
  
  ## dirction vector, right hand side value vector
  dir_vec <- rep('>=',length(callId_vec))
  rhs_vec <- callAmount_vec
  
  con_list <- list(coef_mat=coef_mat,dir_vec=dir_vec,rhs_vec=rhs_vec)
  return(con_list)
}

DummyConst <- function(varName_vec,qtyVarNum,quantity_vec){
  # Dummy decision variables constraints, constraint number = qtyVarNum * 2
  #   1. x1 - M * d1 <= 0, M is a large number
  #   2. x1 - d1 >= m , m is a small number
  #
  # *Coefficient matrix:
  #   row number is the number of constraints; col number is the number of decision variables
  # 
  # Returns:
  #   A list contains constraint's coefficient matrix, direction vector, and right hand side vector 
  
  varNum2 <- length(varName_vec)
  newName_mat <- SplitVarName(varName_vec[1:qtyVarNum],'all')
  newName_vec <- PasteVarName(newName_mat[1,],rep('dummy',qtyVarNum),newName_mat[3,])
  newNameDummy_vec <- varName_vec[(qtyVarNum+1):varNum2]
  
  coef_mat1 <- matrix(0,nrow=(varNum2-qtyVarNum),ncol=varNum2)
  coef_mat1[cbind(match(newName_vec,newNameDummy_vec),1:qtyVarNum)] <- 1
  scaleFactor_vec <- quantity_vec+1
  scaleFactor_vec <- scaleFactor_vec[match(newNameDummy_vec,newName_vec)]
  coef_mat1[cbind(1:(varNum2-qtyVarNum),(qtyVarNum+1):varNum2)] <- -scaleFactor_vec
  
  dir_vec1 <- rep('<=',varNum2-qtyVarNum)
  rhs_vec1 <- rep(0,varNum2-qtyVarNum)
  
  coef_mat2 <- matrix(0,nrow=varNum2-qtyVarNum,ncol=varNum2)
  coef_mat2[cbind(match(newName_vec,newNameDummy_vec),1:qtyVarNum)] <- 1
  coef_mat2[cbind(1:(varNum2-qtyVarNum),(qtyVarNum+1):varNum2)] <- -1
  
  dir_vec2 <- rep('>=',varNum2-qtyVarNum)
  rhs_vec2 <- rep(-0.1,varNum2-qtyVarNum)
  
  coef_mat <- rbind(coef_mat1,coef_mat2)
  dir_vec <- c(dir_vec1,dir_vec2)
  rhs_vec <- c(rhs_vec1,rhs_vec2)
  
  con_list <- list(coef_mat=coef_mat,dir_vec=dir_vec,rhs_vec=rhs_vec)
  return(con_list)
}

DummyConstInherit <- function(allocated_vec,varName_vec,qtyVarNum,quantity_vec){
  # To be refactored
  # used when there are some assets selected to the calls
  varNum2 <- length(varName_vec)
  newName_mat <- SplitVarName(varName_vec[1:qtyVarNum],'all')
  newName_vec <- PasteVarName(newName_mat[1,],rep('dummy',qtyVarNum),newName_mat[3,])
  newNameDummy_vec <- varName_vec[(qtyVarNum+1):varNum2]
  
  fCon4_mat <- matrix(0,nrow=(varNum2-qtyVarNum),ncol=varNum2)
  
  colIdx1_vec <- 1:qtyVarNum
  rowIdx1_vec <- match(newName_vec,newNameDummy_vec)
  colIdx2_vec <- (qtyVarNum+1):varNum2
  rowIdx2_vec <- 1:(varNum2-qtyVarNum)
  
  fCon4_mat[cbind(rowIdx1_vec,colIdx1_vec)] <- 1
  scaleFactor_vec <- quantity_vec+1
  scaleFactor_vec <- scaleFactor_vec[match(newNameDummy_vec,newName_vec)]
  fCon4_mat[cbind(rowIdx2_vec,colIdx2_vec)] <- -scaleFactor_vec
  
  fDir4_vec <- rep('<=',varNum2-qtyVarNum)
  fDir4_vec[which(allocated_vec==1)] <- '>='  ## new added 18 May
  fRhs4_vec <- rep(0,varNum2-qtyVarNum)
  
  fCon5_mat <- matrix(0,nrow=varNum2-qtyVarNum,ncol=varNum2)
  fCon5_mat[cbind(rowIdx1_vec,colIdx1_vec)] <- 1
  fCon5_mat[cbind(rowIdx2_vec,colIdx2_vec)] <- -1
  
  fDir5_vec <- rep('>=',varNum2-qtyVarNum)
  fRhs5_vec <- rep(-0.1,varNum2-qtyVarNum)
  
  fCon4_mat <- rbind(fCon4_mat,fCon5_mat)
  fDir4_vec <- c(fDir4_vec,fDir5_vec)
  fRhs4_vec <- c(fRhs4_vec,fRhs5_vec)
  
  fCon4_list <- list(fCon4_mat=fCon4_mat,fDir4_vec=fDir4_vec,fRhs4_vec=fRhs4_vec)
  return(fCon4_list)
}

MovementConst <- function(varName_vec,qtyVarNum,operLimitMs,fungible){
  varNum2 <- length(varName_vec)
  msIdDul_vec <- SplitVarName(varName_vec[(qtyVarNum+1):varNum2],'ms')
  msId_vec <- unique(msIdDul_vec)
  msNum <- length(msId_vec)
  
  coef_mat1 <- matrix(0,nrow=1,ncol=varNum2)
  coef_mat1[1,(qtyVarNum+1):varNum2] <- 1
  
  dir_vec1 <- c('<=')
  rhs_vec1 <- c(operLimitMs*msNum)
  
  #### set the movements limit per margin statement if fungible=FALSE
  # the total limit is not necessary in theory, but it's better keep it until proven
  if(fungible==FALSE){
    # will be number of margin statements constraints
    coef_mat2 <- matrix(0,nrow=msNum,ncol=varNum2)
    
    colIdx_vec <- (qtyVarNum+1):varNum2
    rowIdx_vec <- match(msIdDul_vec,msId_vec)
    
    coef_mat2[cbind(rowIdx_vec,colIdx_vec)] <- 1
    
    dir_vec2 <- rep('<=',msNum)
    rhs_vec2 <- rep(operLimitMs,msNum)
    
    coef_mat <- rbind(coef_mat1,coef_mat2)
    dir_vec <- c(dir_vec1,dir_vec2)
    rhs_vec <- c(rhs_vec1,rhs_vec2)
  }
  
  con_list <- list(coef_mat=coef_mat,dir_vec=dir_vec,rhs_vec=rhs_vec)
  return(con_list)
}

MoveConstInherit <- function(allocated_vec,varName_vec,qtyVarNum,operLimitMs,fungible){
  # To be refactored
  # used when there are some assets selected to the calls
  varNum2 <- length(varName_vec)
  msIdDul_vec <- SplitVarName(varName_vec[(qtyVarNum+1):varNum2],'ms')
  msId_vec <- unique(msIdDul_vec)
  msNum <- length(msId_vec)
  
  fCon5_mat <- matrix(0,nrow=1,ncol=varNum2)
  fCon5_mat[1,(qtyVarNum+1):varNum2] <- 1
  
  fDir5_vec <- c('<=')
  fRhs5_vec <- c(operLimitMs*msNum)
  
  #### set the movements limit per margin statement if fungible=FALSE
  # the total limit is not necessary in theory, but it's better keep it until proven
  if(fungible==FALSE){
    # will be number of margin statements constraints
    fCon6_mat <- matrix(0,nrow=msNum,ncol=varNum2)
    
    colIdx_vec <- (qtyVarNum+1):varNum2
    rowIdx_vec <- match(msIdDul_vec,msId_vec)
    
    fCon6_mat[cbind(rowIdx_vec,colIdx_vec)] <- 1
    
    fDir6_vec <- rep('<=',msNum)
    fRhs6_vec <- rep(operLimitMs,msNum)
    
    fCon5_mat <- rbind(fCon5_mat,fCon6_mat)
    fDir5_vec <- c(fDir5_vec,fDir6_vec)
    fRhs5_vec <- c(fRhs5_vec,fRhs6_vec)
  }
  fCon5_mat[,which(allocated_vec==1)] <- 0
  
  fCon5_list <- list(fCon5_mat=fCon5_mat,fDir5_vec=fDir5_vec,fRhs5_vec=fRhs5_vec)
  return(fCon5_list)
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
  return(optimalAsset_mat[,2])
}

DeriveOptimalAssetsV1 <- function(minUnitQuantity_mat,eli_mat,callAmount_mat,haircut_mat,minUnitValue_mat,
                                  pref_vec,objParams_list,callId_vec,resource_vec){
  callNum <- length(callId_vec); resourceNum <- length(resource_vec)
  normCost_mat <- objParams_list$cost_mat
  normLiquidity_mat <- objParams_list$liquidity_mat 
  normOperation_mat <- objParams_list$operation_mat
  
  optimal_mat <- normOperation_mat*pref_vec[3]+normLiquidity_mat*pref_vec[2]+normCost_mat*pref_vec[1]
  colnames(optimal_mat) <- resource_vec; rownames(optimal_mat)<-callId_vec
  
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

DeriveVarName <- function(callInfo_df, availAsset_df){
  # Construct the decision variable names
  # 
  # Quantity decision variable
  #   Definition: the minimum unit quantity of a resource allocated to a call in a statement.
  #   Name format: "msId___mcId___resourceId"
  # Dummy decision variables:
  #   Definition: 1 or 0 denote whether a resource is allocated to a statement or not.
  #   Name format: "msId___dummy___resourceId"
  # Relationship: 
  #   d1: ms1___dummy___resource1, x1: ms1___mc1___resource1, x2: ms1___mc2___resource1 (assume ms1 contains only mc1 and mc2)
  #     d1 = 1, if x1 + x2 > 0;
  #     d1 = 0, if x1 + x2 = 0.
  #
  # Returns:
  #   A vector of decision variables' names
  
  # Quantity decision variable names
  quantityName_vec <- vector()
  
  # Dummy decision variable names
  dummyName_vec <- vector()
  
  for(i in 1:dim(availAsset_df)[1]){
    resource <- availAsset_df$resource[i]
    callId <- availAsset_df$callId[i]
    msId <- callInfo_df$marginStatement[match(callId, callInfo_df$id)]
    quantityName_vec[i] <- PasteVarName(msId,callId,resource)
    dummyName_vec[i] <- PasteVarName(msId,'dummy',resource)
  }
  
  # remove duplicated variables constructed by same asset in one statement for different calls
  distinctDummyName_vec <- unique(dummyName_vec) 
  
  ## Construct dummy variables' position 
  # example
  # QuantityName      = c(ms1___mc1___R1, ms1___mc2___R1,ms2___mc3___R1)
  # DummyName         = c(ms1___dummy___R1,ms1___dummy___R1,ms2___dummy___R1)
  # DistinctDummyName = c(ms1___dummy___R1,ms2___dummy___R1)
  
  varName_vec <- c(quantityName_vec,distinctDummyName_vec)
  
  return(varName_vec)
}

GetQtyVarNum <- function(varName_vec){
  qtyVarNum <- which(SplitVarName(varName_vec,"call")=="dummy")[1] - 1
  return(qtyVarNum)
}

EliMat <- function(CRMap_df,callId_vec,resource_vec){
  # Construct eligibility matrix with call ids and resource ids as two dimensions
  #
  # Args:
  #   CRMap_df: callId, resourceId availability mapping
  #
  # Returns:
  #   haircut matrix
  
  eli_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                    dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(CRMap_df$callId,callId_vec)
  idxResource_vec <- match(CRMap_df$resource,resource_vec)
  
  eli_mat[cbind(idxCallId_vec,idxResource_vec)] <- 1
  return(eli_mat)
}

HaircutCVec2Mat <- function(haircutC_vec,availAsset_df,callId_vec,resource_vec){
  # Construct Collateral haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df(columns to be used: haircut, callId, resource)
  #
  # Returns:
  #   haircut matrix
  
  haircutC_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                         dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$resource,resource_vec)
  
  haircutC_mat[cbind(idxCallId_vec,idxResource_vec)] <- haircutC_vec
  return(haircutC_mat)
}

HaircutFXVec2Mat <- function(haircutFX_vec,availAsset_df,callId_vec,resource_vec){
  # Construct FX haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df
  #         columns to be used: callId, resource
  # Returns:
  #   haircut matrix
  
  haircutFX_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                          dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$resource,resource_vec)
  
  haircutFX_mat[cbind(idxCallId_vec,idxResource_vec)] <- haircutFX_vec
  return(haircutFX_mat)
}

HaircutVec2Mat <- function(haircut_vec,availAsset_df,callId_vec,resource_vec){
  # Construct total haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df(columns to be used: haircutC, haircutFX, callId, resource)
  #
  # Returns:
  #   haircut matrix
  
  haircut_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                        dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$resource,resource_vec)
  
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
  idxResource_vec <- match(availAsset_df$resource,resource_vec)
  
  cost_mat[cbind(idxCallId_vec,idxResource_vec)] <- cost_vec
  return(cost_mat)
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

