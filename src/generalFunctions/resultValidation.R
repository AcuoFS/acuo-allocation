

#### Check Functions #######
CheckSolverResult <- function(solution_vec,result_mat,varName_vec,resourceQty_vec,callAmount_vec,minUnitValue_mat,haircut_mat,
                              lpLowerBound_vec,lpUpperBound_vec,operLimitMs,fungible,callInfo_df){
  # Check whether solver solution meets each type of constraints 
  #
  # No returns

  #### Checkings #########
  ## 1. Lower Bound Checking
  CheckLowerBound(solution_vec,lpLowerBound_vec)
  
  ## 2. Upper Bound Checking
  CheckUpperBound(solution_vec,lpUpperBound_vec)
  
  ## 3. Quantity Constraints Checking
  CheckQuantityConstraint(result_mat,resourceQty_vec)
  
  ## 4. Margin Constraints Checking
  CheckMarginConstraint(result_mat,minUnitValue_mat,haircut_mat,callAmount_vec)
  
  ## 5. Dummy Constraints Checking
  CheckDummyConstraint(solution_vec,varName_vec)
  
  ## 6. Movement Constraints Checking
  CheckMovementConstraint(result_mat,operLimitMs,fungible,callInfo_df)
  
}

CheckLowerBound <- function(solution_vec,lpLowerBound_vec){
  # For a semi decision variable 
  # if it >= lower bound or == 0, then satisfy
  satisfy <- all(solution_vec >= lpLowerBound_vec | solution_vec == 0)
  if(!satisfy){
    stop("Lower bound condition checking failed")
  }
}

CheckUpperBound <- function(solution_vec,lpUpperBound_vec){
  satisfy <- all(solution_vec <= lpUpperBound_vec)
  if(!satisfy){
    stop("Upper bound condition checking failed")
  }
}

CheckQuantityConstraint <- function(allocatedQty_mat,resourceQty_vec){
  quantityUsed_vec <- apply(allocatedQty_mat,2,sum)
  satisfy <- all(quantityUsed_vec <= resourceQty_vec)
  if(!satisfy){
    stop("Quantity limit condition checking failed")
  }
}

CheckMarginConstraint <- function(allocatedQty_mat,minUnitValue_mat,haircut_mat,callAmount_vec){
  allocatedAmount_mat <- allocatedQty_mat*minUnitValue_mat*(1-haircut_mat)
  marginAllocated_vec <- apply(allocatedAmount_mat,1,sum)
  satisfy <- all(marginAllocated_vec >= callAmount_vec)
  if(!satisfy){
    stop("Margin completion condition checking failed")
  }
}

CheckDummyConstraint <- function(solution_vec,varName_vec){
  ## Quantity variables info
  qtyVarNum <- GetQtyVarNum(varName_vec)
  qtyVar_vec <- solution_vec[1:qtyVarNum]
  msInQtyVar_vec <- SplitVarName(varName_vec[1:qtyVarNum],"ms")
  resourceInQtyVar_vec <- SplitVarName(varName_vec[1:qtyVarNum],"resource")
  
  ## Iterate dummy variables and check values
  # a better solution to be developed without iteration
  dummyVar_vec <- solution_vec[(qtyVarNum+1):length(varName_vec)]
  dummyVarName_vec <- varName_vec[(qtyVarNum+1):length(varName_vec)]
  for(i in 1:(length(varName_vec)-qtyVarNum)){
    msId <- SplitVarName(dummyVarName_vec[i],"ms")
    resource <- SplitVarName(dummyVarName_vec[i],"resource")
    # find the corresonding quantity decision variables
    idx_vec <- which(msInQtyVar_vec == msId & resourceInQtyVar_vec == resource)
    satisfy <- (dummyVar_vec[i] == (sum(qtyVar_vec[idx_vec]) > 0))
    if(!satisfy){
      stop("Dummy relationship condition checking failed")
    }
  }
}

CheckMovementConstraint <- function(result_mat,operLimitMs,fungible,callInfo_df){
  
  msId_vec <- unique(callInfo_df$marginStatement)
  movementForMs_vec <- rep(0,length(msId_vec))
  for(i in 1:length(msId_vec)){
    idx_vec <- which(callInfo_df$marginStatement == msId_vec[i])
    thisResult_mat <- result_mat[idx_vec,]
    if(length(idx_vec)==1){
      movementForMs_vec[i] <- sum(thisResult_mat & 1)
    }else{
      movementForMs_vec[i] <- sum(apply(thisResult_mat & matrix(1,nrow=dim(thisResult_mat)[1], ncol=dim(thisResult_mat)[2]),2,max))
      
    }
    
  }
  ## Check movement limit of all statements
  satisfy1 <- sum(movementForMs_vec) <= operLimitMs*length(callInfo_df$marginStatement)
  if(!satisfy1){
    stop("Movement limit of all statements condition failed")
  }
  
  ## Check movement limit per statement in fungible scenario
  satisfy2 <- all(movementForMs_vec <= operLimitMs)
  if((!fungible) & (!satisfy2)){
    stop("Movement limit per statement condition failed")
  }
}

#### Adjust Functions #####
RoundUpQuantityVariable <- function(solution_vec,varName_vec){
  # Round up any quantity decision variable x to the nearest integer
  # unless x is "very close" to the floor integer then round down x to the nearest integer.
  #
  # "very close" definition:
  #   x - floor(x) <= small number(e.g. 1e-5)
  #
  # Returns:
  #   The updated solution vector
  
  qtyVarNum <- GetQtyVarNum(varName_vec)
  qtyVar_vec <- solution_vec[1:qtyVarNum]
  qtyVarFloor_vec <- floor(qtyVar_vec)
  
  ## Define A Small Number
  small <- 1e-5
  
  ## Round up quantity variables larger than the small number
  roundUpIdx <- which((qtyVar_vec - qtyVarFloor_vec) > small)
  qtyVar_vec[roundUpIdx] <- ceiling(qtyVar_vec[roundUpIdx])
  
  ## Round down quantity variables equal or than the small number
  roundDownIdx <- which((qtyVar_vec - qtyVarFloor_vec) <= small)
  qtyVar_vec[roundDownIdx] <- floor(qtyVar_vec[roundDownIdx])
  
  solution_vec[1:qtyVarNum] <- qtyVar_vec
  
  return(solution_vec)
}

UpdateDummyVariable <- function(solution_vec,varName_vec){
  # Derive dummy decision variables by quantity decision variables
  # 
  # Returns:
  #   The updated solution vector
  
  ## Quantity variables info
  qtyVarNum <- GetQtyVarNum(varName_vec)
  qtyVar_vec <- solution_vec[1:qtyVarNum]
  msInQtyVar_vec <- SplitVarName(varName_vec[1:qtyVarNum],"ms")
  resourceInQtyVar_vec <- SplitVarName(varName_vec[1:qtyVarNum],"resource")
  
  ## Iterate dummy variables and assign values
  dummyVar_vec <- solution_vec[(qtyVarNum+1):length(varName_vec)]
  dummyVarName_vec <- varName_vec[(qtyVarNum+1):length(varName_vec)]
  for(i in 1:(length(varName_vec)-qtyVarNum)){
    msId <- SplitVarName(dummyVarName_vec[i],"ms")
    resource <- SplitVarName(dummyVarName_vec[i],"resource")
    # find the corresonding quantity decision variables
    idx_vec <- which(msInQtyVar_vec == msId & resourceInQtyVar_vec == resource)
    dummyVar_vec[i] <- (sum(qtyVar_vec[idx_vec]) > 0)
  }
  
  ## Adjusted solution
  solution_vec[(qtyVarNum+1):length(varName_vec)] <- dummyVar_vec
  
  return(solution_vec)
}

AdjustSolverResult <- function(result_mat,quantityTotal_vec,callAmount_vec,haircut_mat,minUnitValue_mat,eli_mat){
  ## Adjustment for Constraint Violations
  result_mat <- AdjustNonNegativeViolation(result_mat)
  result_mat <- AdjustQuantityLimitViolation(result_mat,quantityTotal_vec,callAmount_vec,haircut_mat,minUnitValue_mat,eli_mat)
  result_mat <- AdjustCallRequirementViolation(result_mat,quantityTotal_vec,minUnitValue_mat,haircut_mat,callAmount_vec,eli_mat)
  return(result_mat)
}


AdjustNonNegativeViolation <- function(result_mat){
  callNum <- dim(result_mat)[1]
  for(k in 1:callNum){
    idxNeg_vec <- which(result_mat[k,]<0)
    if(length(idxNeg_vec)>=1){
      result_mat[k,idxNeg_vec] <- 0 # set to 0 first, then check the other two criteria
      warning("Adjusted negative values to 0")
    }
  }
  return(result_mat)
}

AdjustQuantityLimitViolation <- function(result_mat,quantityTotal_vec,callAmount_vec,haircut_mat,minUnitValue_mat,eli_mat){
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
  return(result_mat)
}

AdjustCallRequirementViolation <- function(result_mat,quantityTotal_vec,minUnitValue_mat,haircut_mat,callAmount_vec,eli_mat){
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
  return(result_mat)
}
