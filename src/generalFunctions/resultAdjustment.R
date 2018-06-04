
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

AdjustSolverResult <- function(allocatedQty_mat,resourceQty_vec,callAmount_vec,haircut_mat,minUnitValue_mat,eli_mat){
  # Adjust the allocation result under 3 conditions
  #
  # Args:
  #   allocatedQty_mat: current allocation matrix, allocatedQty_mat[i,j] denotes the minUnit quantity of resource j allocated to call i
  #   resourceQty_vec: minUnit quantity of each resource
  # 
  # Returns:
  #   The adjusted allocation matrix
  allocatedQty_mat <- AdjustNonNegativeViolation(allocatedQty_mat)
  allocatedQty_mat <- AdjustQuantityLimitViolation(allocatedQty_mat,resourceQty_vec,callAmount_vec,haircut_mat,minUnitValue_mat,eli_mat)
  allocatedQty_mat <- AdjustCallRequirementViolation(allocatedQty_mat,resourceQty_vec,minUnitValue_mat,haircut_mat,callAmount_vec,eli_mat)
  return(allocatedQty_mat)
}

AdjustNonNegativeViolation <- function(allocatedQty_mat){
  # Assign 0 to negative elements in allocation matrix
  #
  # Args:
  #   allocatedQty_mat[i,j] < 0 means the quantity of resource j allocated to call i is negative
  #
  # Returns:
  #   the adjusted allocation matrix; 
  #   and warning messages depends on how many adjustments have been done
  
  callId_vec <- rownames(allocatedQty_mat)
  resource_vec <- colnames(allocatedQty_mat)
  for(i in 1:length(callId_vec)){
    idxNeg_vec <- which(allocatedQty_mat[i,] < 0)
    if(length(idxNeg_vec) >= 1){
      allocatedQty_mat[i,idxNeg_vec] <- 0
      for(w in idxNeg_vec){
        warning(paste("Adjusted negative quantity use of resource",resource_vec[w],"in margin call",callId_vec[i],"to 0"))
      }
    }
  }
  return(allocatedQty_mat)
}

AdjustQuantityLimitViolation <- function(allocatedQty_mat,resourceQty_vec,callAmount_vec,haircut_mat,minUnitValue_mat,eli_mat){
  # Adjust the allocation result to meet quantity constraints
  # if the quantity used of any resource exceeds the limit
  # 
  # Variables:
  #   idxExcess_vec: the indexes of the resources violate the quantity limit
  resource_vec <- colnames(allocatedQty_mat)
  callId_vec <- rownames(allocatedQty_mat)
  quantityUsed_vec <- apply(allocatedQty_mat,2,sum)
  idxExcess_vec <- which(quantityUsed_vec > resourceQty_vec)
  if(length(idxExcess_vec) > 0){
    ## Iterate the resources
    for(i in idxExcess_vec){
      ## Construct a 2-row matrix: thisAlloc_mat
      #		First row is the call ids which this resource allocated to; 
      #		Second row is the minUnit quantity used for each allocation to the call.
      #		Order by quantity, ascending
      # which(allocatedQty_mat[,i] > 0): find the call indexes that this resource allocated to
      # allocatedQty_mat[which(allocatedQty_mat[,i]>0),i]: the quantity allocated to the calls
      
      thisAlloc_mat <- matrix(c(which(allocatedQty_mat[,i]>0),allocatedQty_mat[which(allocatedQty_mat[,i]>0),i]),nrow=2,byrow=T)
      if(length(thisAlloc_mat[1,])>1){
        thisAlloc_mat <- thisAlloc_mat[,order(thisAlloc_mat[2,])]
      }
      
      ## Iterate the calls
      for(k in 1:length(thisAlloc_mat[1,])){
        idxCall <- thisAlloc_mat[1,k]
        # the new quantity of this resource to this call
        newQuantity <- DetermineNewQuantityToUseOfExcessResourceForCall(quantityUsedForCall = thisAlloc_mat[2,k],
                                                                        quantityTotal = resourceQty_vec[i],
                                                                        quantityUsed = quantityUsed_vec[i])
        ## Calculate the missing call amount
        # 1.missingAmount <= 0, which means after deducting the exceeded quantity of the resource from the allocation,
        #   the allocated amount is still able to fulfill the call requirement
        # 2. missingAmount > 0, then we need to find other resources to fulfill the call
        #   From the operation efficiency perspective, we first check whether there exists one sufficient resource in the current allocation.
        # (1) if there is, then add more units of this allocated resource
        # (2) if not, then choose another unallocated resource to fulfill the missing amount
        
        missingAmount <- CalculateMissingCallAmount(allocatedQty_mat,idxCall,callAmount_vec[idxCall],i,newQuantity,minUnitValue_mat,haircut_mat)
        
        if(missingAmount <= 0){
          allocatedQty_mat[idxCall,i] <- newQuantity
          break
        } else{
          allocatedQty_mat[idxCall,i] <- newQuantity
          allocatedQty_mat <- FulfillMissingCallAmount(allocatedQty_mat,idxCall,missingAmount,callAmount,thisAlloc_mat,resourceQty_vec,quantityUsed_vec,
                                                 minUnitValue_mat,haircut_mat,eli_mat)
        }
        
        ## Update thisAlloc_mat, quantityUsed_vec
        if(newQuantity==thisAlloc_mat[2,k]){
          thisAlloc_mat <- thisAlloc_mat[,-k]
        } else{
          thisAlloc_mat[2,k] <- newQuantity
        }
        temp <- apply(allocatedQty_mat,2,sum)
        idxNewReource <- which(temp - quantityUsed_vec > 0)
        quantityUsed_vec <- temp
        
        ## warning
        warning(paste("Adjusted excessive quantity use of resource",resource_vec[i],"in margin call",callId_vec[idxCall],"to",newQuantity,
                      "and replace using another resource",resource_vec[idxNewReource]))
      }
    }
  }
  return(allocatedQty_mat)
}

AdjustCallRequirementViolation <- function(allocatedQty_mat,resourceQty_vec,minUnitValue_mat,haircut_mat,callAmount_vec,eli_mat){
  # Adjust the allocation result to meet the margin call requirement
  # if the margin call is not fully fulfilled
  #
  # Variables:
  #   idxCallMissing_vec: indexes of the calls which are not fully fulfilled
  resource_vec <- colnames(allocatedQty_mat)
  callId_vec <- rownames(allocatedQty_mat)
  callFulfilled_vec <- CalculateAllocatedCallAmount(allocatedQty_mat,minUnitValue_mat,haircut_mat,1:dim(allocatedQty_mat)[1])
  callMissingAmount_vec <- callAmount_vec - callFulfilled_vec
  idxCallMissing_vec <- which(callMissingAmount_vec > 0)
  
  if(length(idxCallMissing_vec) >= 1){
    
    for(idxCall in idxCallMissing_vec){
      
      thisAlloc_mat <- matrix(c(which(allocatedQty_mat[idxCall,]>0),allocatedQty_mat[idxCall,which(allocatedQty_mat[idxCall,]>0)]),nrow=2,byrow=T)
      
      missingAmount <- callMissingAmount_vec[idxCall]
      
      quantityUsed_vec <- apply(allocatedQty_mat,2,sum)
      allocatedQty_mat <- FulfillMissingCallAmount(allocatedQty_mat,idxCall,missingAmount,callAmount,thisAlloc_mat,resourceQty_vec,quantityUsed_vec,
                                             minUnitValue_mat,haircut_mat,eli_mat)
      
      temp <- apply(allocatedQty_mat,2,sum)
      idxNewReource <- which(temp - quantityUsed_vec > 0)
      quantityUsed_vec <- temp
      
      ## warning
      warning(paste("Fulfilled the incompleted the call",callId_vec[idxCall],
                    "with resource",resource_vec[idxNewReource]))
    }
  }
  return(allocatedQty_mat)
}

CalculateAllocatedCallAmount <- function(allocatedQty_mat,minUnitValue_mat,haircut_mat,callIdx){
  allocatedAmount_mat <- allocatedQty_mat*minUnitValue_mat*(1-haircut_mat)

  allocatedCallAmount <- apply(allocatedAmount_mat,1,sum)[callIdx]

  return(allocatedCallAmount)  
}

DetermineNewQuantityToUseOfExcessResourceForCall <- function(quantityUsedForCall,quantityTotal,quantityUsed){
  # Calculate the new quantity of a resource to allocate to a call in the case that the resource is overused:
  #   quantityUsed > quantityTotal
  # If the excess quantity is larger than the quantity used in the call, then we remove this resource from the allocation in this call
  # If not, we deduct the excess quantity of this resource from the allocation in this call
  # 
  # Args:
  #   quantityUsedForCall: the quantity of the resource already allocated to the call
  #
  # Returns:
  #   the new quanity of the resource to allocate to the call
  
  if(quantityUsedForCall < quantityUsed - quantityTotal){
    # current allocated quantity < excess quanity
    # which indicates that even we adjust the quantity used in this call to 0,
    # there's still excess usage of this resource
    newQuantity <- 0
  } else{
    quantityToDeduct <- quantityUsed - quantityTotal
    newQuantity <- quantityUsedForCall - quantityToDeduct
  }
  return(newQuantity)
}

CalculateMissingCallAmount <- function(allocatedQty_mat,idxCall,callAmount,idxOldResource,oldResourceNewQuantity,minUnitValue_mat,haircut_mat){
  # Calculate the call amount absent after changing an allocated resource quantity, which means if the new quantity is
  # less than the old quantity, missing amount will be larger than 0
  #
  # Args:
  #   allocatedQty_mat: current allocation matrix, allocatedQty_mat[i,j] denotes the minUnit quantity of resource j allocated to call i
  #   idxCall: the margin call index in allocatedQty_mat
  #   callAmount: the amount of this call requirement
  #   idxOldResource: the resource index in allocatedQty_mat
  #   oldResourceNewQuantity: the new quantity of this resource to use
  #
  # Returns:
  #   the missing amount
  newAllocatedQty_mat <- allocatedQty_mat
  newAllocatedQty_mat[idxCall,idxOldResource] <- oldResourceNewQuantity
  allocatedAmountAfterDeduct <- CalculateAllocatedCallAmount(newAllocatedQty_mat,minUnitValue_mat,haircut_mat,idxCall)
  missingAmount <- callAmount - allocatedAmountAfterDeduct
  return(missingAmount)
}

FulfillMissingCallAmount <- function(allocatedQty_mat,idxCall,missingAmount,callAmount,thisAlloc_mat,resourceQty_vec,quantityUsed_vec,
                                     minUnitValue_mat,haircut_mat,eli_mat){
  # Adjust the quantity of resources allocated to the call which is partially fulfilled
  #
  # Args:
  #   allocatedQty_mat: current allocation matrix, allocatedQty_mat[i,j] denotes the minUnit quantity of resource j allocated to call i
  #   idxCall: the margin call index in allocatedQty_mat
  #   missingAmount: the part of this call requirement hasn't been fulfilled
  #   callAmount: the amount of this call requirement
  #   thisAlloc_mat: current allocation of this resource, thisAlloc_mat[1,i] and thisAlloc_mat[2,i] denote the 
  #                  call index this resource allocated to and the minUnit quanity allocated
  #   resourceQty_vec: a vector of the total minUnit quantity of each resource
  #   quantityUsed_vec: a vector of the used minUnit quantity of each resource
  #   minUnitValue_mat,haircut_mat,eli_mat
  #
  # Variables:
  #   missingQuantity: the quantity needed to meet a missing amount of a call
  # 	idxSuff_vec: the resource indexes which are sufficient to fulfill the missingAmount 
  #		idxSuffAlloc_vec: the allocated resource indexes which are sufficient to fulfill the missingAmount
  #   newResourceQuantity: the new allocation quantity of the resource to substitute the old resource
  #
  # Returns:
  #   new allocation result matrix
  
  missingQuantity_vec <- CalculateIntegralUnit(missingAmount,minUnitValue_mat[idxCall,],1-haircut_mat[idxCall,])
  idxSuff_vec <- intersect(which(resourceQty_vec - quantityUsed_vec >= missingQuantity_vec),which(eli_mat[idxCall,]==1))
  if(length(idxSuff_vec) > 0){
    idxSuffAlloc_vec <- intersect(which(allocatedQty_mat[idxCall,] > 0),idxSuff_vec)
    if(length(idxSuffAlloc_vec) >= 1){
      idxNewResource <- idxSuffAlloc_vec[1]
    }else{
      idxNewResource <- idxSuff_vec[1]
    }
    newResourceQuantity <- missingQuantity_vec[idxNewResource] + allocatedQty_mat[idxCall,idxNewResource]
    allocatedQty_mat[idxCall,idxNewResource] <- newResourceQuantity
    return(allocatedQty_mat)
  } else{
    # which means none of the asset itself is enough to to fulfill the left amount of the margin call
    # This should be a very extreme case, and it's more complicated to develop for this case
    # so, I will leave here blank, once I'm done the rest part I'll return to check
    # Also, the exception handling will be a long-run development, and it will be raised once we have exception
    
    # None of resources have sufficient quantity left to fulfill the missing call amount
    # In this scenario, we will exit the manual adjustment
    stop(Paste("Exit the adjustment due to insufficient for assets call",rownames(allocatedQty_mat)[idxCall]))
  }
}
