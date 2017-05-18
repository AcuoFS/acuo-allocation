
ResourceInfo <- function(resource_vec,assetInfo_df,availAsset_df){
  ## better retrieve from DB
  ## keep useful columns from assetInfo
  ## asset id, name, currency, unitValue, minUnit, minUnitValue, FXRate
  assetId_vec <- SplitResource(resource_vec,'asset')
  custodianAccount_vec <- SplitResource(resource_vec,'custodianAccount')
  idx1_vec <- match(c('id', 'name', 'unitValue', 'minUnit', 'minUnitValue','currency', 'FXRate'),names(assetInfo_df))
  resource_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),idx1_vec]

  ## add resource id, custodianAccount id, quantity, minQty, qtyRes
  resource_df <- cbind(id=resource_vec,resource_df,custodianAccount_vec)
  idx2_vec <- match(resource_vec, availAsset_df$assetCustacId)
  venue_vec <- availAsset_df$venue[idx2_vec] 
  qtyOri_vec <- availAsset_df$quantity[idx2_vec]
  qtyMin_vec <- floor(qtyOri_vec/resource_df$minUnit) # interal minUnit quantity
  qtyRes_vec <- qtyOri_vec - qtyMin_vec*resource_df$minUnit # quantity left after integral minQty
  
  resource_df <- cbind(resource_df[,1:3],qtyOri_vec,qtyMin_vec,qtyRes_vec,resource_df[,4:9],venue_vec)
  
  names(resource_df) <- c('id','assetId','assetName','qtyOri','qtyMin','qtyRes','unitValue', 'minUnit','minUnitValue','currency','FXRate',
                              'custodianAccount','venue')
  
  return(resource_df)
}

AvailAsset <- function(availAsset_df){
  ## keep useful columns
  ## "callId","assetCustacId","internalCost", "opptCost", "yield", "haircut","FXHaircut","externalCost","interestRate"
  idx_vec <- match(c("callId","assetCustacId","internalCost", "opptCost", "yield", "haircut","FXHaircut","externalCost","interestRate"),names(availAsset_df))
  new_df <- availAsset_df[,idx_vec]
  
  return(new_df)
}

AssetByCallInfo <- function(callId_vec,resource_vec,availAsset_df){
  
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  availAsset_df <- availAsset_df[order(availAsset_df$callId),] # order the availAsset_df by callId_vec
  
  base_mat <- matrix(0,nrow=callNum,ncol=resourceNum, dimnames = list(callId_vec,resource_vec))
  eli_mat <- base_mat
  haircut_mat <- base_mat
  cost_mat <- base_mat
  
  # fill in matrixes with the data from availAsset_df
  idxTempCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxTempResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
 
  eli_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- 1
  haircut_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$haircut+availAsset_df$FXHaircut
  cost_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-(availAsset_df$interestRate+availAsset_df$yield)

  # convert the matrix format data to vector format
  # thinking of keeping only eligible parts
  eli_vec <- as.vector(t(eli_mat))
  haircut_vec <- as.vector(t(haircut_mat))
  cost_vec <- as.vector(t(cost_mat))
  
  output_list <- list(base_mat=base_mat,eli_mat=eli_mat,haircut_mat=haircut_mat,cost_mat=cost_mat)
  return (output_list)
}

