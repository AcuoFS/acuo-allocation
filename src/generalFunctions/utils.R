
CalculateIntegralUnit <- function(amount,valuePerUnit,discount){
  # All args can be a single element or a vector
  intUnit <- ceiling(amount/(valuePerUnit*discount))
  return(intUnit)
}

UsedQtyFromResultList <- function(result_list,resource_vec,callId_vec){
  #### minUnitQuantity of resources used for allocation
  quantityUsed_vec <- rep(0,length(resource_vec))
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  
  for(i in 1:callNum){
    callId <- callId_vec[i]
    alloc_df <- result_list[[callId]]
    resourceTemp_vec <- PasteResource(alloc_df$Asset,alloc_df$CustodianAccount)
    idxInRes_vec <- na.omit(match(resourceTemp_vec,resource_vec))
    if(length(idxInRes_vec)!=0){
      idxInAlloc <- match(resource_vec[idxInRes_vec],resourceTemp_vec)
      quantityUsed_vec[idxInRes_vec] <- quantityUsed_vec[idxInRes_vec]+alloc_df$Quantity[idxInAlloc]
    }
  }
  return(quantityUsed_vec)
}

PrintCallAllocatedAmount <- function(callAmount_vec,callId_vec,callSelect_list){
  # print the call amount and the allocated amount in a matrix
  # no return
  callAllocated_mat<- matrix(c(callInfo_df$callAmount,rep(0, callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callAmount','allocatedAmount')))
  for(i in 1:callNum){
    callAllocated_mat[i,2] <- sum(callSelect_list[[callId_vec[i]]]$`NetAmount(USD)`)
  }
  print(callAllocated_mat)
}
