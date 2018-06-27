
HaircutCVec2Mat <- function(haircutC_vec,availAsset_df,callId_vec,resource_vec){
  # Construct Collateral haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df(columns to be used: haircut, callId, resource)
  #
  # Returns:
  #   haircut matrix
  
  haircutC_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                         dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$resource,resource_vec)
  
  haircutC_mat[cbind(idxCallId_vec,idxResource_vec)] <- haircutC_vec
  return(haircutC_mat)
}

HaircutFXVec2Mat <- function(haircutFX_vec,availAsset_df,callId_vec,resource_vec){
  # Construct FX haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df
  #         columns to be used: callId, resource
  # Returns:
  #   haircut matrix
  
  haircutFX_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                          dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$resource,resource_vec)
  
  haircutFX_mat[cbind(idxCallId_vec,idxResource_vec)] <- haircutFX_vec
  return(haircutFX_mat)
}

HaircutMat <- function(availAsset_df,callId_vec,resource_vec){
  # Construct total haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df(columns to be used: haircutC, haircutFX, callId, resource)
  #
  # Returns:
  #   haircut matrix
  
  haircut_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                        dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$resource,resource_vec)
  
  haircut_vec <- availAsset_df$haircut + availAsset_df$FXHaircut
  haircut_mat[cbind(idxCallId_vec,idxResource_vec)] <- haircut_vec
  return(haircut_mat)
}
