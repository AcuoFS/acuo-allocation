library("RNeo4j")

modelInput = function(callId,order='assetId'){
  
  source('src/availAssetByCall.R')
  source('src/assetInfoById.R')
  
  result <- availAssetByCall(callId,order)
  
  assetId <- unique(result$assetId)
  asset.num <- length(assetId)
  call.num <- length(callId)
  
  assetInfo <- assetInfoById(assetId)
  assetInfo <- assetInfo[match(assetId,assetInfo$id),]
  
  ###############################################
  # elegibility matrix: 1-eligible, 0-ineligible
  # haircut matrix: haircut+FX haircut
  # quantity matrix
  # value matrix: value/FX rate
  # cost matrix: internal+external+opptunity-yield(interestRate)
  # call amount vector
  ############################################
  

  base.mat <- matrix(0,nrow=call.num,ncol=asset.num, dimnames = list(callId,assetId))
  
  ele.mat <- base.mat
  haircut.mat <- base.mat
  cost.mat <- base.mat
  quantity.mat <- base.mat
  value.mat<- base.mat
  call.mat <- base.mat
  
  # fill in matrixes with the data from result
  call.mat[callId,] <- as.numeric(unique(cbind(result$callId,result$callAmount))[,2])
  quantity.mat[,]<- matrix(rep(as.numeric(unique(cbind(result$assetId,result$quantity))[,2]),call.num),nrow=call.num,byrow=T)
  value.mat[,]<-matrix(rep(as.numeric(unique(cbind(result$assetId,result$value/result$FXRate))[,2]),call.num),nrow=call.num,byrow=T)
  
  ele.mat[cbind(result$callId,result$assetId)]<-1
  haircut.mat[cbind(result$callId,result$assetId)]<- result$haircut+result$FXHaircut
  cost.mat[cbind(result$callId,result$assetId)]<- result$internalCost+result$externalCost+result$opptCost-result$interestRate

  
  keep.row <- which(apply(ele.mat,1,sum)!=0)   # keep the rows with eligible assets
  keep.col <- which(apply(ele.mat,2,sum)!=0)   # keep the cols with eligible assets
  
  ele.mat <- ele.mat[keep.row,keep.col]
  haircut.mat <- haircut.mat[keep.row,keep.col]
  cost.mat <- cost.mat[keep.row,keep.col]
  quantity.mat <- quantity.mat[keep.row,keep.col]
  value.mat <- value.mat[keep.row,keep.col]
  
  output.list <- list(assetId=assetId,assetInfo=assetInfo,
                      ele.mat=ele.mat,haircut.mat=haircut.mat,
                      cost.mat = cost.mat,
                      quantity.mat=quantity.mat, value.mat=value.mat,
                      call.mat = call.mat
                      )
  
  return (output.list)
}
options("scipen"=100, "digits"=4) # show decimal number

