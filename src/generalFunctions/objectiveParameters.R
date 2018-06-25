
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
  return(normLiquidity_vec)
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
