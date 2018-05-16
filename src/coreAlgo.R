
CoreAlgoV2 <- function(callInfo_df,availAsset_df,resource_df,
                       pref_vec,operLimitMs,fungible,
                       ifNewAlloc,initAllocation_list,allocated_list,
                       minMoveValue,timeLimit){
  # Estimate overall asset sufficiency
  # Derive optimal assets based on the weighted objectives parameters
  # Determine the method to solve the problem based on the sufficiency of optimal assets
  
  callInfo_df <- renjinFix(callInfo_df, "callInfo.")
  resource_df <- renjinFix(resource_df, "resource.")  
  
  #### Handle Extreme Scenarios ##################
  ## 1. movement limit for one or several margin statements is 1
  if(operLimitMs==1){
    availAsset_df <- HandleStatementMovementLimitIsOne(availAsset_df,callInfo_df,resource_df)
  }
  
  #### Assign Default Values ####
  if(missing(timeLimit)){
    timeLimit <- 13
  }
  if(missing(minMoveValue)){
    minMoveValue <- 1000
  }
  #### Derive Eligibility Matrix ###################
  eli_mat <- EliMat(availAsset_df,callInfo_df$id,resource_df$id)
  #### Derive Haircut Matrix ###################
  haircut_mat <- HaircutVec2Mat(haircut_vec = availAsset_df$haircut + availAsset_df$FXHaircut,
                                availAsset_df,callInfo_df$id,resource_df$id)
  #### Derive Cost Matrix ###################
  costBasis_mat <- CostVec2Mat(cost_vec = DefineCost(availAsset_df,resource_df),
                               availAsset_df,callInfo_df$id,resource_df$id)
  #### Check Asset Pool Sufficiency #####
  AssetsAreSufficient <- CheckAssetSufficiency(eli_mat,haircut_mat,resource_df$qtyMin,resource_df$minUnitValue,callInfo_df$callAmount)
  if(!AssetsAreSufficient){
    stop('ALERR2003: Asset inventory is insufficient')
  }
  
  #### Generate Standardized Cost and Liquidity #######
  minUnitValue_mat <- matrix(rep(resource_df$minUnitValue, length(callInfo_df$id)),nrow=length(callInfo_df$id),byrow=T)
  resourceSuffQty_mat <- CalculateIntegralUnit(amount = rep(callInfo_df$callAmount,length(resource_df$id)),
                                               minUnitValue_mat,1-haircut_mat)
  costScore_mat <- GenerateStandardizedCostMat(integerCallAmount_mat = minUnitValue_mat * resourceSuffQty_mat,
                                               costBasis_mat,callInfo_df$id,resource_df$id)
  liquidityScore_mat <- GenerateStandardizedLiquidityMat(resourceLiquidity = DefineLiquidity(availAsset_df,resource_df),
                                                         callInfo_df$id,resource_df$id)
  
  #### Derive the Optimal Assets and Check Sufficiency #######
  optimalAsset_mat <- DeriveOptimalAssetsV2(resource_df$qtyMin,callInfo_df$callAmount,resource_df$minUnitValue,eli_mat,haircut_mat,
                                            costScore_mat,liquidityScore_mat,pref_vec,callInfo_df$id,resource_df$id)
  
  optimalAssetsAreSufficient <- CheckOptimalAssetSufficiency(optimalAsset_mat,resourceSuffQty_mat,resource_df)
  
  #### Allocate ###############
  if(optimalAssetsAreSufficient){
    result_mat <- AllocateUnderSufficientOptimalAssets(optimalAsset_mat,resourceSuffQty_mat,callInfo_df$id,resource_df$id)
  } else {
    result_mat <- AllocateUnderInsufficientOptimalAssets(costScore_mat,liquidityScore_mat,pref_vec,eli_mat,
                                                    callInfo_df,resource_df$id,resource_df,
                                                    minMoveValue,operLimitMs,fungible,timeLimit,
                                                    allocated_list,initAllocation_list)
  }
  
  #### Valiate and Update Allocation Result ########
  checkResult <- CheckResultVec(result_mat,quantityTotal_vec=resource_df$qtyMin,callInfo_df$id,callInfo_df$callAmount,resource_df$minUnitValue,haircut_mat,eli_mat)
  result_mat <- checkResult$result_mat
  ## return ?
  resource_df$qtyMin <- checkResult$quantityTotal_vec
  
  #### Convert the Result from Matrix to List ####
  haircutC_mat <- HaircutCVec2Mat(availAsset_df$haircut,availAsset_df,callInfo_df$id,resource_df$id)
  haircutFX_mat <- HaircutFXVec2Mat(availAsset_df$FXHaircut,availAsset_df,callInfo_df$id,resource_df$id)
  result_list <- ResultMat2List(result_mat,callInfo_df$id,resource_df$id,callInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df)
  
  return(result_list)
}