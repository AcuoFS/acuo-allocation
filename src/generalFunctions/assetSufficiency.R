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
  eli_mat <- EliMat(availAsset_df,callInfo_df$id,resource_df$id)
  ## Haircut Matrix
  haircut_mat <- HaircutVec2Mat(haircut_vec = availAsset_df$haircut + availAsset_df$FXHaircut,
                                availAsset_df,callInfo_df$id,resource_df$id)
  #### Check Whether Assets Are Sufficient Per Each Call #####
  suffPerCall <- all((eli_mat*(1-haircut_mat)) %*% (quantity_vec*minUnitValue_vec) > callInfo_df$callAmount)
  
  #### Check Whether Assets Are Sufficient for All Calls #####
  # use the asset's highest haircut among calls for calculation
  suffAllCall <- sum(quantity_vec*minUnitValue_vec*(1-apply(haircut_mat,2,max))) > sum(callInfo_df$callAmount)
  
  return(suffPerCall & suffAllCall)
}
