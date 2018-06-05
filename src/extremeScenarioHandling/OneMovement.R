##############################################################
## Title: Functions to handle the scenario where movement limit for a statement is 1
## Purpose: When there are two calls in a statement, and the movement limit is 1 for this statement,
##          it's difficult for the solver to find a feasible solution especially with pre-solve setting
##          because only a few eligible resources are sufficient to meet the call requirement. 
##          So we need to eliminate the infeasible possibilities passed to the solver.
##          To do so, we can exclude the unwanted resources from the available asset pool for the call(s).
## Strategy: Remove the rows with the unwanted resource-call mapping from availAsset_df
## Author: Silin Yuan
## Date: 2018/05/08
##############################################################

HandleStatementMovementLimitIsOne <- function(availAsset_df,callInfo_df,resource_df){
  # Deal with the scenario where movement limit for a margin statement is 1 by manipulating availAsset_df
  #
  # Args:  
  #   availAsset_df: margin calls' available resources info   
  #     columns will be used: all
  #   callInfo_df: margin call info
  #     columns will be used: all
  #   resource_df: resources info
  #     columns will be used: all
  #
  # Returns:  
  #   The new availAsset_df after processing. 
  msId_vec <- unique(callInfo_df$marginStatement)
  for(msId in msId_vec){
    callInThisMs_vec <- callInfo_df$id[which(callInfo_df$marginStatement==msId)]
    if(length(callInThisMs_vec)==2){
      ## remove the resources that are not sufficient to individually fulfill the total call amount in this MS
      availAsset_df <- RemoveResourcesNotSufficientForBothCallsFromAvailAsset(availAsset_df,callInfo_df,resource_df,callInThisMs_vec,msId)
    }
  }

  return(availAsset_df)
}

RemoveResourcesNotSufficientForBothCallsFromAvailAsset <- function(availAsset_df,callInfo_df,resource_df,twoCallIds,msId){
  # Remove the rows where the resource is not sufficient to individually fulfill the total call amount of both calls in this margin statement from availAsset_df
  #
  # Args:  
  #   availAsset_df: margin calls' available resources info   
  #     columns will be used: all
  #   callInfo_df: margin call info
  #     columns will be used: all
  #   resource_df: resources info
  #     columns will be used: all
  #   twoCallIds: the two margin calls 
  #   msId: the margin statement of the two margin calls 
  # 
  # Returns:  
  #   The new availAsset_df after removing the rows. 
  
  ## 1. remove resources not eligible for both calls
  # after this step, the resources in availAsset_df for both calls will be the same
  availAsset_df <- RemoveResourcesNotEligibleForBothCallsFromAvailAsset(availAsset_df,twoCallIds,msId)
  
  ## 2. remove the insufficient assets
  twoCallAmounts <- callInfo_df$callAmount[match(twoCallIds,callInfo_df$id)]
  suffResource_vec <- FindSufficientResourcesForBothCalls(availAsset_df,resource_df,twoCallIds,twoCallAmounts)
  if(length(suffResource_vec)>0){
    rmIdx_vec <- which(availAsset_df$callId %in% twoCallIds && !(availAsset_df$resource %in% suffResource_vec)) 
    
    availAsset_df <- RemoveRowsInAvailAsset(availAsset_df,rmIdx_vec)
    return(availAsset_df)
  } else{
    errormsg <- paste('ALERR2002: There is no single asset sufficient for both',twoCallIds,'in',msId)
    stop(errormsg)
  }
}

RemoveResourcesNotEligibleForBothCallsFromAvailAsset <- function(availAsset_df,twoCallIds,msId){
  # Remove the rows where the resource is not eligible for both calls in this margin statement from availAsset_df
  #
  # Args:  
  #   availAsset_df: margin calls' available resources info   
  #     columns will be used: callId, resource
  #   twoCallIds: the two margin calls 
  #   msId: the margin statement of the two margin calls 
  # 
  # Returns:  
  #   The new availAsset_df after removing the rows. 
  
  # eligible resources indexes for call1 and call2 in availAsset_df
  idxEliCall1_vec <- which(availAsset_df$callId==twoCallIds[1]) 
  idxEliCall2_vec <- which(availAsset_df$callId==twoCallIds[2]) 
  
  # the corresponding resource ids and common resource ids
  resourceEliCall1_vec <- availAsset_df$resource[idxEliCall1_vec]
  resourceEliCall2_vec <- availAsset_df$resource[idxEliCall2_vec]
  commonResource_vec <- intersect(resourceEliCall1_vec,resourceEliCall2_vec) 
  
  # remove the rows with those resoures
  if(length(commonResource_vec) > 0){ 
    # the indexes of other assets for call1 and call2
    rmIdx1_vec <- idxEliCall1_vec[-match(commonResource_vec,resourceEliCall1_vec)]
    rmIdx2_vec <- idxEliCall2_vec[-match(commonResource_vec,resourceEliCall2_vec)]
    
    availAsset_df <- RemoveRowsInAvailAsset(availAsset_df,c(rmIdx1_vec,rmIdx2_vec))
    return(availAsset_df)
  } else{ 
    errormsg <- paste('ALERR2002: There is no common assets eligible for both',paste(twoCallIds),'in',msId)
    stop(errormsg)
  }
}

FindSufficientResourcesForBothCalls <- function(availAsset_df,resource_df,twoCallIds,twoCallAmounts){
  # Find sufficient resources for two margin calls
  #
  # Args:  
  #   availAsset_df: margin calls' available resources info   
  #     columns will be used: callId, haircut, FXHaircut
  #   resource_df: resources info
  #     columns will be used: id, minUnitValue, qtyMin
  #   twoCallIds: the two margin calls 
  #   twoCallAmounts: call amount for the two margin calls 
  # 
  # Returns:  
  #   The sufficient resource ids.  
  
  # eligible resources indexes for call1 and call2 in availAsset_df
  idxEliCall1_vec <- which(availAsset_df$callId==twoCallIds[1]) 
  idxEliCall2_vec <- which(availAsset_df$callId==twoCallIds[2]) 
  commonResource_vec <- availAsset_df$resource[idxEliCall1_vec]
  
  # the corresponding resource ids for call1 and call2 and common resource ids
  idxResource_vec <- match(commonResource_vec,resource_df$id)
  minUnitValue_vec <- resource_df$minUnitValue[idxResource_vec]
  quantity_vec <- resource_df$qtyMin[idxResource_vec]
  
  # the haircut of the resources for call1 and call2 
  haircut1_vec <- availAsset_df$haircut[idxEliCall1_vec]+availAsset_df$FXHaircut[idxEliCall1_vec]
  haircut2_vec <- availAsset_df$haircut[idxEliCall2_vec]+availAsset_df$FXHaircut[idxEliCall2_vec]
  
  # the integral sufficient quantity of common resources to fulfill call1 and call2
  integralSuffQty1_vec <- CalculateIntegralUnit(amount = twoCallAmounts[1],
                                                valuePerUnit = minUnitValue_vec,
                                                discount = 1- haircut1_vec)
  integralSuffQty2_vec <- CalculateIntegralUnit(amount = twoCallAmounts[2],
                                                valuePerUnit = minUnitValue_vec,
                                                discount = 1- haircut1_vec)
  integralSuffQty_vec <- integralSuffQty1_vec+integralSuffQty2_vec
  
  # sufficient resource ids
  suffIdx_vec <- which(quantity_vec >= integralSuffQty_vec) # index of sufficient resources in idxResource_vec
  suffResource_vec <- resource_df$id[idxResource_vec[suffIdx_vec]]
  
  return(suffResource_vec)
}

