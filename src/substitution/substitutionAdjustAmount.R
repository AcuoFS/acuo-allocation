
SubstitutionAdjustAmount <- function(newResource,newResourceAmount,amountSelect,resource_df,availAsset_df){

  if(amountSelect=="Amount"){
    # adjust the amount of new asset by minUnitValue
    newResourceMinQuantity <- ceiling(newResourceAmount/resource_df$minUnitValue)
    newResourceRequireAmount <- newResourceMinQuantity * resource_df$minUnitValue
    newResourceAdjAmount <- newResourceRequireAmount*(1-availAsset_df$haircut-availAsset_df$FXHaircut)
      
  } else if(amountSelect=="AdjAmount"){
    # adjust the amount of new asset by minUnitValue
    newResourceMinQuantity <- ceiling(newResourceAmount/(1-availAsset_df$haircut-availAsset_df$FXHaircut)/resource_df$minUnitValue)
    newResourceRequireAmount <- newResourceMinQuantity * resource_df$minUnitValue
    newResourceAdjAmount <- newResourceRequireAmount*(1-availAsset_df$haircut-availAsset_df$FXHaircut)
      
  }

  return(list(amount=newResourceRequireAmount, adjAmount=newResourceAdjAmount))
}





