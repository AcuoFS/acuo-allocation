
CoreAlgoV2 <- function(configurations,callInfo_df,availAsset_df,resource_df,
                       pref_vec,operLimitMs,fungible,
                       ifNewAlloc,initAllocation_mat,allocated_list,
                       minMoveValue,timeLimit){
  # Handle extreme scenarios
  # Calculated the objectives parameters
  # Derive optimal assets based on the weighted objectives parameters
  # Derive the sufficiency of the optimal assets
  # Determine and call the method to solve the problem based on the sufficiency of optimal assets
  # Valiate the solution
  
  ## Remove unwanted characters from column names of the dataframes(not sure if still needed)
  callInfo_df <- renjinFix(callInfo_df, "callInfo.")
  resource_df <- renjinFix(resource_df, "resource.")  
  
  #### Assign Default Values ####
  if(missing(timeLimit)){
    timeLimit <- 13
  }
  if(missing(minMoveValue)){
    minMoveValue <- 1000
  }
  
  #### Handle Extreme Scenarios ##################
  ## 1. movement limit for one or several margin statements is 1
  if(operLimitMs==1){
    availAsset_df <- HandleStatementMovementLimitIsOne(availAsset_df,callInfo_df,resource_df)
    resource_df <- RemoveResourceNotInAvailAsset(availAsset_df,resource_df)
  }
  
  #### Derive Eligibility and Haircut Matrix ###################
  eli_mat <- EliMat(availAsset_df[c('callId','resource')],callInfo_df$id,resource_df$id)
  haircut_mat <- HaircutMat(availAsset_df,callInfo_df$id,resource_df$id)

  #### Generate Standardized Cost and Liquidity #######
  costScore_mat <- GenerateStandardizedCostMat(cost_mat = CostVec2Mat(cost_vec = DefineCost(availAsset_df,resource_df),
                                                                      availAsset_df,callInfo_df$id,resource_df$id),
                                               callInfo_df$id,resource_df$id)
  liquidityScore_mat <- GenerateStandardizedLiquidityMat(resourceLiquidity = DefineLiquidity(availAsset_df,resource_df),
                                                         callInfo_df$id,resource_df$id)
  
  #### Derive the Optimal Assets and Check Sufficiency #######  
  optimalResource_vec <- DeriveOptimalAssetsV2(resource_df$id,resource_df$qtyMin,callInfo_df$id,callInfo_df$callAmount,resource_df$minUnitValue,eli_mat,haircut_mat,
                                            costScore_mat,liquidityScore_mat,pref_vec)
  
  optimalResourcesAreSufficient <- CheckOptimalAssetSufficiency(optimalResource_vec,callInfo_df,availAsset_df,resource_df)
  #optimalResourcesAreSufficient <- F
  
  #### Allocate ###############
  if(optimalResourcesAreSufficient){
    result_mat <- AllocateUnderSufficientOptimalAssets(optimalResource_vec,callInfo_df,availAsset_df,resource_df)
  } else {
    result_mat <- AllocateUnderInsufficientOptimalAssets(configurations,costScore_mat,liquidityScore_mat,pref_vec,
                                                         callInfo_df,resource_df,availAsset_df,
                                                         minMoveValue,operLimitMs,fungible,timeLimit,
                                                         ifNewAlloc,allocated_list,initAllocation_mat)
  }

  #### Calculate Objective Value ########
  minUnitValue_mat <- matrix(rep(resource_df$minUnitValue, length(callInfo_df$id)),nrow=length(callInfo_df$id),byrow = T)
  objValue <- sum((pref_vec[1]*costScore_mat + pref_vec[2]*liquidityScore_mat)*result_mat*minUnitValue_mat*eli_mat)
  
  return(list(result_mat=result_mat,objValue=objValue))
}

renjinFix <- function(frame, name) {
  d <- data.frame(frame);
  colnames(d) <- gsub(name, "", colnames(d));
  return(d);
}
