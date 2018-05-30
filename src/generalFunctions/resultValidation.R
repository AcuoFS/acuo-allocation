
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
