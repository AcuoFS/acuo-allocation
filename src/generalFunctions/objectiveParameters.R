
GenerateStandardizedCostMat <- function(integerCallAmount_mat,costBasis_mat,callId_vec,resource_vec){
  
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  #### calculate the cost
  cost_mat <- integerCallAmount_mat*costBasis_mat
  normCost_mat <- cost_mat
  for(i in 1:callNum){
    if(length(unique(cost_mat[i,]))==1){
      normCost_mat[i,]<-1
    }else{
      normCost_mat[i,]<- scale(cost_mat[i,])
      normCost_mat[i,]<- normCost_mat[i,]+(-min(normCost_mat[i,])*2)
    }
  }
  normCost_vec <- as.vector(t(normCost_mat))
  return(normCost_mat)
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
  normLiquidity_vec <- as.vector(t(normLiquidity_mat))
  return(liquidity_mat)
}
