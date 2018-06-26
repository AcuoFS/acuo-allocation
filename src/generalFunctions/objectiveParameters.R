
GenerateStandardizedCostMat <- function(cost_mat,callId_vec,resource_vec){
  #### cost
  callNum <- length(callId_vec)
  
  normCost_mat <- cost_mat
  for(i in 1:callNum){
    if(length(unique(cost_mat[i,]))==1){
      normCost_mat[i,]<-1
    }else{
      normCost_mat[i,]<- scale(cost_mat[i,])
      normCost_mat[i,]<- normCost_mat[i,]+(-min(normCost_mat[i,])*2)
    }
  }
  return(normCost_mat)
}

CostVec2Mat <- function(cost_vec,availAsset_df,callId_vec,resource_vec){
  # Construct haircut matrix with call ids and resource ids as two dimensions
  #
  # Args: availAsset_df(columns to be used: all)
  #       resource_df(columns to be used: all)
  #         
  # Returns:
  #   haircut matrix
  
  cost_mat <- matrix(0,nrow=length(callId_vec),ncol=length(resource_vec),
                     dimnames = list(callId_vec,resource_vec))
  
  idxCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxResource_vec <- match(availAsset_df$resource,resource_vec)
  
  cost_mat[cbind(idxCallId_vec,idxResource_vec)] <- cost_vec
  return(cost_mat)
}

GenerateStandardizedLiquidityMat <- function(resourceLiquidity_vec,callId_vec,resource_vec){
  #### liquidity
  callNum <- length(callId_vec)
  liquidity_mat <- matrix(rep(resourceLiquidity_vec,callNum),nrow=callNum,byrow=TRUE,dimnames=list(callId_vec,resource_vec)) 
  normLiquidity_mat <- liquidity_mat
  for(i in 1:callNum){
    if(length(unique(liquidity_mat[i,]))==1){
      normLiquidity_mat[i,]<-1
    }else{
      normLiquidity_mat[i,]<- scale(liquidity_mat[i,])
      normLiquidity_mat[i,]<- normLiquidity_mat[i,]+(-min(normLiquidity_mat[i,])*2)
    }
  }
  return(normLiquidity_mat)
}


CalculateObjParams <- function(cost,liquidity,pref_vec,unit,minUnitValue){
  # calculate the weighted objectives parameters
  # per 1 unit or per 1 dollar value of a resource
  # 
  # Args:
  #   unit: the unit of the decision variable
  #         "quantity" means per 1 unit of a resource; "amount" means per 1 dollar value of a resource
  #
  pref_vec <- pref_vec/sum(pref_vec[1:2]) # Recalculate the parameters weight setting
  
  if(unit == "quantity"){
    weightedScore <- pref_vec[1]*cost*minUnitValue + pref_vec[2]*liquidity*minUnitValue
  } else if(unit == "amount"){
    weightedScore <- pref_vec[1]*cost + pref_vec[2]*liquidity
  }
  return(weightedScore)
}
