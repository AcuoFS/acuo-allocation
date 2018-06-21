
AllocateByRank <- function(costScore_mat,liquidityScore_mat,pref_vec,callInfo_df,resource_df,availAsset_df,
                           operLimitMs,fungible){
  # Allocate by the ranking of the resources for each statement, higher ranking resource will be allocated first.
  #
  # Iterate the margin statements
  #   if only one call in the statement, invoke AllocateFirstCall()
  #   if two calls in the statement, invoke AllocateFirstCall() and AllocateAnotherCall()
  #
  # Returns:
  #   A matrix of the allocated quantity by each call and each resource
  
  
  ##### Static Variables ####
  score_mat <- CalculateObjParams(costScore_mat,liquidityScore_mat,pref_vec,"amount")
  eli_mat <- EliMat(availAsset_df[c('callId','resource')],callInfo_df$id,resource_df$id)
  haircut_mat <- HaircutVec2Mat(availAsset_df,callInfo_df$id,resource_df$id)
  
  #### Dynamic Variables ####
  # Store the Latest Quantity of Each Resource
  resourceQty_vec <- resource_df$qtyMin
  # Initiate Output
  result_mat <- matrix(0,nrow = length(callInfo_df$id),ncol = length(resource_df$id),
                       dimnames = list(callInfo_df$id,resource_df$id))
  
  #### Iterate Margin Statements #########
  # for each margin statement, find the best operLimitMs or less number of resources
  # Process
  #   1. Apply different strategies by the number of calls in the statement
  #   2. Update the resource quantity left
  # sacrifice 1: consider one statement at a time
  # sacrifice 2: in fungible scenario, has to set limit on statement level
  # sacrifice 3: assume fungible == FALSE
  
  for(msId in unique(callInfo_df$marginStatement)){
    idxCall <- which(callInfo_df$marginStatement == msId)
    callInThisMs <- callInfo_df$id[idxCall]
    
    if(length(callInThisMs) == 1){
      callAmount <- callInfo_df$callAmount[idxCall]
      result_mat <- AllocateFirstCall(result_mat,score_mat[idxCall,],operLimitMs,idxCall,callAmount,resource_df$id,resourceQty_vec,resource_df$minUnitValue,
                                haircut_mat[idxCall,],eli_mat[idxCall,])
      
    } else if(length(callInThisMs) == 2){

      callAmountInMs <- callInfo_df$callAmount[idxCall]
      idxCallLarger <- idxCall[which.max(callAmountInMs)]
      idxCallSmaller <- idxCall[-which.max(callAmountInMs)]
      
      callAmountLarger <- callInfo_df$callAmount[idxCallLarger]
      callAmountSmaller <- callInfo_df$callAmount[idxCallSmaller]
      
      result_mat <- AllocateLargerCallFirst(idxCallLarger,idxCallSmaller,callAmountLarger,callAmountSmaller,
                              result_mat,score_mat,operLimitMs,resource_df$id,resourceQty_vec,resource_df$minUnitValue,
                              haircut_mat,eli_mat)
    }
    ## Update the Latest Resource Quantity Left
    resourceQty_vec <- resource_df$qtyMin - apply(result_mat,2,sum)
  }
  return(result_mat)
}

AllocateFirstCall <- function(result_mat,score_vec,movementLimit,idxCall,callAmount,resource_vec,resourceQty_vec,minUnitValue_vec,
                              haircut_vec,eli_vec){
  idx_vec <- which(eli_vec == 1)
  movementLeft <- movementLimit
  
  list <- AllocateWithinMovements(result_mat,idxCall,callAmount,movementLeft,resource_vec,score_vec,resourceQty_vec,
                                  haircut_vec,minUnitValue_vec,idx_vec,vector())
  if(list$leftCallAmount > 0){
    errormsg <- paste('ALERR2004: It is not sufficient to allocate',movementLeft,'assets for',rownames(result_mat)[idxCall])
    stop(errormsg)
  }
  return(list$result_mat)
}

AllocateAnotherCall <- function(result_mat,score_vec,movementLimit,idxCall,idxCall0,callAmount,resource_vec,resourceQty_vec,minUnitValue_vec,
                                haircut_vec,eli_vec){
  # Allocate another call in the statement
  #
  # Args:
  #   idxCall: the index of this call to be allocated
  #   idxCall0: the index of the first call in the statement
  #
  # Varibles:
  #   idxAllocated_vec: indexes of eligible resources which are allocated to the first call
  #   idx_vec: indexes of the resources that are usable for this call
  #
  # Returns: 
  #   result matrix
  
  idxAllocated_vec <- which(result_mat[idxCall0,] > 0 & eli_vec == 1)
  
  if(movementLimit == 0){
    idx_vec <- idxAllocated_vec
  } else if(movementLimit > 0){
    idx_vec <- which(eli_vec == 1)
  }
  
  list <- AllocateWithinMovements(result_mat,idxCall,callAmount,movementLimit,resource_vec,score_vec,resourceQty_vec,
                                  haircut_vec,minUnitValue_vec,idx_vec,idxAllocated_vec)
  
  if(list$leftCallAmount > 0){
    errormsg <- paste('ALERR2004: It is not sufficient to allocate',movementLimit,'assets for',rownames(result_mat)[idxCall])
    stop(errormsg)
  }
  return(list$result_mat)
}

AllocateLargerCallFirst <- function(idxCallLarger,idxCallSmaller,callAmountLarger,callAmountSmaller,
                                    result_mat,score_mat,operLimitMs,resource_vec,resourceQty_vec,minUnitValue_vec,haircut_mat,eli_mat){
  # the strategy:
  #   - first allocate  the call with larger call amount, allow operLimitMs movements;
  #   checking: if call is not fully fulfilled, return an error
  #   - update result, quantity, and movement left;
  #   - then allocate the other call
  #       if movement is used up, then we can only select the resources that are allocated to the first call;
  #   checking: if call is not fully fulfilled, return an error
  
  # allocate the larger call
  result_mat <- AllocateFirstCall(result_mat,score_mat[idxCallLarger,],operLimitMs,idxCallLarger,callAmountLarger,resource_vec,resourceQty_vec,minUnitValue_vec,
                                    haircut_mat[idxCallLarger,],eli_mat[idxCallLarger,])

  # update      
  idxUsedLarger <- which(result_mat[idxCallLarger,] > 0)
  operLimitLeft <- operLimitMs - length(idxUsedLarger)
  resourceQty_vec[idxUsedLarger] <- resourceQty_vec[idxUsedLarger] - result_mat[idxCallLarger,idxUsedLarger]
  
  # allocate the smaller call
  result_mat <- AllocateAnotherCall(result_mat,score_mat[idxCallSmaller,],operLimitLeft,idxCallSmaller,idxCallLarger,callAmountSmaller,resource_vec,resourceQty_vec,minUnitValue_vec,
                                       haircut_mat[idxCallSmaller,],eli_mat[idxCallSmaller,])
  
  return(result_mat)
}

AllocateWithinMovements <- function(result_mat,idxCall,leftCallAmount,movementLeft,resource_vec,score_vec,resourceQty_vec,
                                    haircut_vec,minUnitValue_vec,idx_vec,idx0Move_vec){
  # Allocate a call within a movement limit
  # In each iteration, find the optimal resource and fulfill the left call amount 
  # until the call is fully fulfilled (leftCallAmount equals to 0)
  # or the movement limit is reached
  # Note that using the same resources allocated to the previous call won't take up movement
  #
  # Args:
  #   movementLeft: the movement left to allocate
  #   idx_vec: the indexes of resources usable
  #   idx0Move_vec: the indexes of resources which will take up 0 movement (a subset of idx_vec)
  #
  # Returns:
  #   A list contains the result matrix and the call amount hasn't been fulfilled
  
  while((length(idx_vec) > 0 & movementLeft > 0) | (length(idx0Move_vec) > 0 & movementLeft == 0)){

    optimalResource <- DetermineOptimalResourceByRank(resource_vec[idx_vec],score_vec[idx_vec],resourceQty_vec[idx_vec],haircut_vec[idx_vec],minUnitValue_vec[idx_vec],
                                                      dominator = "score")
    
    idxResource <- match(optimalResource,resource_vec)
    
    idx_vec <- idx_vec[-match(idxResource,idx_vec)]
    if(idxResource %in% idx0Move_vec){
      idx0Move_vec <- idx0Move_vec[-match(idxResource,idx0Move_vec)]
    } else{
      movementLeft <- movementLeft - 1
    }
    
    qtyRequired <- CalculateIntegralUnit(leftCallAmount,minUnitValue_vec[idxResource],1-haircut_vec[idxResource])
    
    if(resourceQty_vec[idxResource] >= qtyRequired){
      quantity <- qtyRequired
      leftCallAmount <- 0
      result_mat[idxCall,idxResource] <- quantity
      break
    } else{
      quantity <- resourceQty_vec[idxResource]
      leftCallAmount <- leftCallAmount - quantity*minUnitValue_vec[idxResource]*(1-haircut_vec[idxResource])
      result_mat[idxCall,idxResource] <- quantity
    }
  }
  return(list(result_mat=result_mat,leftCallAmount=leftCallAmount))
}

DetermineOptimalResourceByRank <- function(resource_vec,score_vec,resourceQty_vec,haircut_vec,minUnitValue_vec,
                                           dominator){
  # There are two factors will affect the ranking of a resource: 
  #   1. resource score: lower score, higher ranking
  #   2. resource amount: larger amount, higher ranking
  # Note that the amount will change with the resource usage, so the ranking will change all the time 

  if(dominator == "amount"){
    optimalResource <- SelectResourceAmountFirst(resource_vec,score_vec,resourceQty_vec,haircut_vec,minUnitValue_vec)
  } else if(dominator == "score"){
    optimalResource <- SelectResourceScoreFirst(resource_vec,score_vec,resourceQty_vec,haircut_vec,minUnitValue_vec)
  } else{
    stop("Please input a valid dominator!")
  }
  return(optimalResource)
}

SelectResourceAmountFirst <- function(resource_vec,score_vec,resourceQty_vec,haircut_vec,minUnitValue_vec){
  # select the resource with largest amount and then highest ranking
  resourceAmount_vec <- floor(resourceQty_vec)*(1-haircut_vec)*minUnitValue_vec
  
  idxLargestAmount_vec <- which(resourceAmount_vec == resourceAmount_vec[which.max(resourceAmount_vec)])
  idxHighestRank <- which.min(score_vec[idxLargestAmount_vec])
  idxResource <- idxLargestAmount_vec[idxHighestRank]
  optimalResource <- resource_vec[idxResource]
  return(optimalResource)
}

SelectResourceScoreFirst <- function(resource_vec,score_vec,resourceQty_vec,haircut_vec,minUnitValue_vec){
  # select the resource with highest ranking and then largest amount
  resourceAmount_vec <- floor(resourceQty_vec)*(1-haircut_vec)*minUnitValue_vec
  
  idxHighestRank_vec <- which(score_vec == score_vec[which.min(score_vec)])
  idxLargestAmount <- which.max(resourceAmount_vec[idxHighestRank_vec])
  
  idxResource <- idxHighestRank_vec[idxLargestAmount]
  optimalResource <- resource_vec[idxResource]
  return(optimalResource)
}
