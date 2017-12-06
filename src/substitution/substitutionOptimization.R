SubstitutionOptimization <- function(algoVersion,availAsset_df,assetInfo_df,resource_df,callInfo_df,
                                     subCollateral_df,
                                     pref_vec,operLimit,operLimitMs_vec,fungible){
  subResourceId <- PasteResource(subCollateral_df$asset,subCollateral_df$custodianAccount)
  
  # substitution amount needs to be adjusted by haircut and fxRate
  subAmount <- subCollateral_df$quantity*subCollateral_df$unitValue*(1-subCollateral_df$haircut)/subCollateral_df$fxRate
  
  
  subCallId <- subCollateral_df$call
  
  # consider the settled collaterals for this margin call
  # 1. allocate an asset that is in the settled collateral list will be considered operational efficient
  
  # remove the asset from availAsset_df which is being substituted
  # don't consider the scenario that the old asset is no longer available
  # because if so, we can't retrieve the asset information by the current cypher queries
  idx <- which(availAsset_df$assetCustacId==subResourceId)
  if(length(idx)>=1){
    subAvailAsest_df <- availAsset_df[which(availAsset_df$assetCustacId==subResourceId),]
    availAsset_df <- availAsset_df[-which(availAsset_df$assetCustacId==subResourceId),]
  }
  # remove resource from resource_df and resource_vec
  idx <- which(resource_df$id==subResourceId)
  if(length(idx)>=1){
    subResource_df <- resource_df[which(resource_df$id==subResourceId),]
    resource_df <- resource_df[-which(resource_df$id==subResourceId),]
  }
  resource_vec <- resource_df$id
  
  callInfo_df$callAmount <- subAmount
  
  # new allocation
  newResult <- CallAllocation(algoVersion,scenario=1,subCallId,resource_vec,
                            callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                            ifNewAlloc=T,list())
  # compare with the current allocation
  currentResult <- CallAllocation(algoVersion,scenario=1,subCallId,subResourceId,
                            callInfo_df,subAvailAsest_df,subResource_df,pref_vec,1,1,fungible,
                            ifNewAlloc=T,list())
  # define the factors we want to compare
  # 1. cost (potentially the double funding cost, consider in the future)
  dailyCostSaving <- currentResult$resultAnalysis$dailyCost - newResult$resultAnalysis$dailyCost
  monthlyCostSaving <- currentResult$resultAnalysis$monthlyCost - newResult$resultAnalysis$monthlyCost
  
  
  return(list(newResult=newResult,currentResult=currentResult,dailyCostSavin=dailyCostSaving,monthlyCostSaving=monthlyCostSaving))
}