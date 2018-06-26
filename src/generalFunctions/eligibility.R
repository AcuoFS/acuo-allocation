EliMat <- function(CRMap_df,callId_vec,resource_vec){
  # Construct eligibility matrix with call ids and resource ids as two dimensions
  #
  # Args:
  #   CRMap_df: callId, resourceId availability mapping
  #
  # Returns:
  #   haircut matrix
  
  eli_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                    dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(CRMap_df$callId,callId_vec)
  idxResource_vec <- match(CRMap_df$resource,resource_vec)
  
  eli_mat[cbind(idxCallId_vec,idxResource_vec)] <- 1
  return(eli_mat)
}
