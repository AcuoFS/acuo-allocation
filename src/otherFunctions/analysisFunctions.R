
LiquidFun <- function(quantity_vec,quantityTotal_vec,liquidity_vec,minUnitValue_vec){
  numerator <- sum(quantity_vec*liquidity_vec*minUnitValue_vec)
  denominator <- sum(quantityTotal_vec*liquidity_vec*minUnitValue_vec)
  ratio <- numerator/denominator
  return(ratio)
}

CostFun <- function(amount_vec,cost_vec){
  cost <- sum(amount_vec*cost_vec)
  return(cost)
}

OperationFun <- function(result,callInfo_df,method){
  movements <- 0
  if(method=='matrix'){
    result_mat <- result
    resultDummy_mat <- 1*(result_mat&1)
    msDul_vec <- callInfo_df$marginStatement
    msId_vec <- unique(msDul_vec)
    
    if(length(result_mat[1,])==1){
      for(m in 1:length(msId_vec)){
        idxTemp_vec <- which(msDul_vec==msId_vec[m])
        if(length(idxTemp_vec)==1){
          movements <- movements+sum(resultDummy_mat[idxTemp_vec])
        } else{
          movements <- movements+max(resultDummy_mat[idxTemp_vec])
        }
      }
    } else{
      for(m in 1:length(msId_vec)){
        idxTemp_vec <- which(msDul_vec==msId_vec[m])
        if(length(idxTemp_vec)==1){
          movements <- movements+sum(resultDummy_mat[idxTemp_vec,])
        } else{
          movements <- movements+sum(apply(resultDummy_mat[idxTemp_vec,],2,max))
        }
      }
    }
  } else if(method=='msList'){
    msOutput_list <- result
    msId_vec <- unique(callInfo_df$marginStatement)
    msNum <- length(msId_vec)
    for(i in 1:msNum){
      msId <- msId_vec[i]
      msAlloc_df <- msOutput_list[[msId]]
      resources <- unique(PasteResource(msAlloc_df$Asset,msAlloc_df$CustodianAccout))
      movements <- movements + length(resources)
    }
  } else if(method=='callList'){
    callOutput_list <- result
    callId_vec <- callInfo_df$id
    msId_vec <- unique(callInfo_df$marginStatement)
    callNum <- length(callId_vec)
    msNum <- length(msId_vec)
    for(i in 1:msNum){
      msId <- msId_vec[i]
      callIds <- callInfo_df$id[which(callInfo_df$marginStatement==msId)]
      
      callId <- callIds[1]
      msAlloc_df <- callOutput_list[[callId]]
      if(length(callIds>1)){
        # inside one margin statement
        for(m in 2:length(callIds)){
          callId <- callIds[m]
          callAlloc_df <- callOutput_list[[callId]]
          msAlloc_df <- rbind(callAlloc_df,msAlloc_df)
        }
      }
      resources <- unique(PasteResource(msAlloc_df$Asset,msAlloc_df$CustodianAccout))
      movements <- movements + length(resources)
    }
    
  } else{
    stop('Please input a valid method!')
  }
  
  return(movements)
}
