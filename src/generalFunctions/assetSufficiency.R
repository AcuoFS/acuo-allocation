EstimateAssetSufficiency <- function(availAsset_df,callInfo_df,resource_df){
  # check whether the assets are sufficient per each call and for all calls
  #   sufficient per call: amount(post haircut) of available assets for a call is larger than the call amount
  #   sufficient for all calls: total amount(post haircut*) of available assets is larger than the total call amount
  #             *If an asset is available for several calls, take the highest one to calculate sufficiency
  #              to ensure the accuracy of sufficiency instead of the accuracy of insufficiency.
  #              There's a very low possibility that our calculation shows insufficient but the asset inventory is actually sufficient. 
  #              Though we can still run the algo to see whether it's indeed insufficient, the inventory is in a very low level already, 
  #              so giving back the insufficiency warning at this point is helpful for users to realize their inventory level
  #              so that they can increase their available assets in time to avoid future insufficiency. 
  #
  # Returns:
  #   true - asset inventory is sufficient
  #   false - asset inventory is likely to be insufficient
  
  ## Eligibility Matrix
  eli_mat <- EliMat(availAsset_df[c('callId','resource')],callInfo_df$id,resource_df$id)
  ## Haircut Matrix
  haircut_mat <- HaircutMat(availAsset_df,callInfo_df$id,resource_df$id)
  #### Check Whether Assets Are Sufficient Per Each Call #####
  suffPerCall <- all((eli_mat*(1-haircut_mat)) %*% (resource_df$qtyMin*resource_df$minUnitValue) > callInfo_df$callAmount)
  
  #### Check Whether Assets Are Sufficient for All Calls #####
  # use the asset's highest haircut among calls for calculation
  suffAllCall <- sum(resource_df$qtyMin*resource_df$minUnitValue*(1-apply(haircut_mat,2,max))) > sum(callInfo_df$callAmount)
  
  return(suffPerCall & suffAllCall)
}


CheckOptimalAssetSufficiency <- function(optimalResource_vec,callInfo_df,availAsset_df,resource_df){
  # Check whether the optimal resources are sufficeint for the calls
  #
  # Args:
  #   optimalResource_vec: IDs of optimal resources, in call id order
  #
  # Returns:
  #   true or false
  
  ## Haircut Matrix
  haircut_mat <- HaircutMat(availAsset_df,callInfo_df$id,resource_df$id)
  ## Sufficient Resource Units for Calls Matrix
  resourceSuffQty_mat <- CalculateIntegralUnit(amount = rep(callInfo_df$callAmount,length(resource_df$id)),
                                               valuePerUnit = matrix(rep(resource_df$minUnitValue, length(callInfo_df$id)),nrow=length(callInfo_df$id),byrow=T),
                                               discount = 1-haircut_mat)
  ## Distinct Optimal Resources
  uniqueResource_vec <- unique(optimalResource_vec)
  ## Sufficiency Vector for Distinct Resources : 1 - sufficient; 0 - insufficient
  isResourceSuff_vec <- rep(0,length(uniqueResource_vec))
  
  for(i in 1:length(uniqueResource_vec)){
    resource <- uniqueResource_vec[i]
    idxCall_vec <- which(optimalResource_vec==resource) 
    idxResource <- which(resource_df$id==resource)
    
    isResourceSuff_vec[i] <- 1*(sum(resourceSuffQty_mat[idxCall_vec,idxResource]) < resource_df$qtyMin[idxResource])
  }
  isSuff <- sum(isResourceSuff_vec)==length(uniqueResource_vec)
  return(isSuff)
}

