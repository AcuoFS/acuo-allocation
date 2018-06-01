
CalculateIntegralUnit <- function(amount,valuePerUnit,discount){
  # All args can be a single element or a vector
  intUnit <- ceiling(amount/(valuePerUnit*discount))
  return(intUnit)
}

RemoveRowsInAvailAsset <- function(availAsset_df,rmIdx_vec){
  if(length(rmIdx_vec)>0){
    availAsset_df <- availAsset_df[-rmIdx_vec,]
  }
  return(availAsset_df)
}
