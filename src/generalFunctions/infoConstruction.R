#### infoFunctions #### 
ResourceInfoAndAvailAsset <- function(assetInfo_df,availAsset_df){
  # order by call id 
  availAsset_df <- availAsset_df[order(availAsset_df[,1],availAsset_df[,2]),]
  # remove assets with negative amount
  availAsset_df$quantity[which(availAsset_df$quantity<0)] <- 0 # avoid negative amount
  # construct the resourceId
  availAsset_df$assetCustacId <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
  resource_vec <- unique(availAsset_df$assetCustacId)
  
  ## construct resource_df
  assetId_vec <- SplitResource(resource_vec,'asset')
  custodianAccount_vec <- SplitResource(resource_vec,'custodianAccount')
  # keep 11 columns
  idx1_vec <- match(c('id', 'name', 'unitValue', 'minUnit', 'minUnitValue','currency','yield', 'FXRate','oriFXRate'),names(assetInfo_df))
  resource_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),idx1_vec]
  
  ## add 5 columns: resource id, custodianAccount id, quantity, minQty, qtyRes
  resource_df <- cbind(id=resource_vec,resource_df,custodianAccount_vec)
  idx2_vec <- match(resource_vec, availAsset_df$assetCustacId)
  venue_vec <- availAsset_df$venue[idx2_vec] 
  qtyOri_vec <- availAsset_df$quantity[idx2_vec]
  qtyMin_vec <- floor(qtyOri_vec/resource_df$minUnit) # interal minUnit quantity
  qtyRes_vec <- qtyOri_vec - qtyMin_vec*resource_df$minUnit # quantity left after integral minQty
  
  resource_df <- cbind(resource_df[,1:3],qtyOri_vec,qtyMin_vec,qtyRes_vec,resource_df[,4:11],venue_vec)
  resource_df$id <- as.character(resource_df$id)
  
  names(resource_df) <- c('id','assetId','assetName','qtyOri','qtyMin','qtyRes','unitValue', 'minUnit','minUnitValue','currency','yield','FXRate','oriFXRate',
                          'custodianAccount','venue')
  
  ## clean up availAsset_df
  ## keep 8 columns
  idx_vec <- match(c("callId","assetCustacId","internalCost", "opptCost", "haircut","FXHaircut","externalCost","interestRate"),names(availAsset_df))
  newAvailAsset_df <- availAsset_df[,idx_vec]
  
  # add 1 column for the simplicity of calculation later: yield
  newAvailAsset_df$yield <- resource_df$yield[match(newAvailAsset_df$assetCustacId,resource_df$id)]
  
  return(list(resource_df=resource_df,availAsset_df=newAvailAsset_df))
}

UnifyFxBaseUsdInAssetInfo <- function(assetInfo_df){
  # add assetInfo_df$oriFXRate to store the original fx rate
  # change assetInfo_df$oriFXRate to 1 USD can change how much foreign currency
  
  ## Add oriFXRate, change FXRate
  assetInfo_df$oriFXRate <- assetInfo_df$FXRate
  if(!is.null(assetInfo_df$from)&&!is.null(assetInfo_df$to)){
    idxTo <- which(assetInfo_df$to=="USD")
    assetInfo_df$FXRate[idxTo] <- 1/assetInfo_df$FXRate[idxTo]
  }
  return(assetInfo_df)
}

AddMinUnitValueInBaseCcyToAssetInfo <- function(assetInfo_df){
  # add assetInfo_df$minUnitValue to store the minimum unit value in base ccy
  
  ## Add minUnitValue
  assetInfo_df$minUnitValue <- assetInfo_df$minUnit*assetInfo_df$unitValue/assetInfo_df$FXRate
  return(assetInfo_df)
}

UnifyFxBaseUsdInCallInfo <- function(callInfo_df){
  # add callInfo_df$oriFXRate to store the original fx rate
  # change callInfo_df$oriFXRate to 1 USD can change how much foreign currency
  
  ## Add oriFXRate, change FXRate
  callInfo_df$oriFXRate <- callInfo_df$FXRate
  if(!is.null(callInfo_df$from)&&!is.null(callInfo_df$to)){
    idxTo <- which(callInfo_df$to=="USD")
    callInfo_df$FXRate[idxTo] <- 1/callInfo_df$FXRate[idxTo]
  }
  return(callInfo_df)
}

ConvertCallAmountToBaseCcyInCallInfo <- function(callInfo_df){
  # add callInfo_df$oriCallAmount to store the original call amount in local call currency
  # change callInfo_df$callAmount to call amount in base ccy
  
  ## Add oriCallAmount, change callAmount
  callInfo_df$oriCallAmount <- callInfo_df$callAmount
  callInfo_df$callAmount <- callInfo_df$callAmount/callInfo_df$FXRate 
  return(callInfo_df)
}
