library("RNeo4j")

allocationInputData = function(callId,clientId,order='assetId'){
  source('src/functionsOfDBRequestByExecutingCypher.R')
  
  result <- availAssetByCallIdAndClientId(callId,clientId,order)
  
  assetId <- unique(result$assetId)
  asset.num <- length(assetId)
  call.num <- length(callId)
  
  assetInfo <- assetInfoByAssetId(assetId)
  assetInfo <- assetInfo[match(assetId,assetInfo$id),]
  
  callInfo <- callInfoByCallId(callId)
  callInfo <- callInfo[match(callId,callInfo$id),]
  
  ###############################################
  # eligibility matrix: 1-eligible, 0-ineligible
  # haircut matrix: haircut+FX haircut
  # quantity matrix
  # value matrix: value/FX rate
  # cost matrix: internal+external+opptunity-yield(interestRate)
  # call amount matrix: duplicate the column
  # minUnit matrix: minUnit[i,j]=x, asset j for margin call i has a minimum denomination x,
  #     which means we can only allocate the integral multiples quantity of A_j to MC_i.
  #     To start with, we use 1 for non-cash securities; 0.0001 for cash, apply to all margin calls.
  ############################################
  

  base.mat <- matrix(0,nrow=call.num,ncol=asset.num, dimnames = list(callId,assetId))
  
  eli.mat <- base.mat
  haircut.mat <- base.mat
  cost.mat <- base.mat
  quantity.mat <- base.mat
  value.mat<- base.mat
  call.mat <- base.mat
  minUnit.mat <- base.mat  
  
  # fill in matrixes with the data from result
  call.mat[callId,] <- matrix(rep(callInfo$callAmount,asset.num),nrow=call.num,byrow=F)
  quantity.mat[,]<- matrix(rep(as.numeric(unique(cbind(result$assetId,result$quantity))[,2]),call.num),nrow=call.num,byrow=T)
  value.mat[,]<-matrix(rep(as.numeric(unique(cbind(result$assetId,result$value/result$FXRate))[,2]),call.num),nrow=call.num,byrow=T)
  
  eli.mat[cbind(result$callId,result$assetId)]<-1
  haircut.mat[cbind(result$callId,result$assetId)]<- result$haircut+result$FXHaircut
  cost.mat[cbind(result$callId,result$assetId)]<- result$internalCost+result$externalCost+result$opptCost-result$interestRate
  
  minUnit.mat[] <- 1
  minUnit.mat[,which(assetInfo$isdaType=='Cash')]<- 0.0001
  
#  keep.row <- which(apply(eli.mat,1,sum)!=0)   # keep the rows with eligible assets
#  keep.col <- which(apply(eli.mat,2,sum)!=0)   # keep the cols with eligible assets
  
#  eli.mat <- eli.mat[keep.row,keep.col]
#  haircut.mat <- haircut.mat[keep.row,keep.col]
#  cost.mat <- cost.mat[keep.row,keep.col]
#  quantity.mat <- quantity.mat[keep.row,keep.col]
#  value.mat <- value.mat[keep.row,keep.col]
  
  
  eli.vec <- as.vector(t(eli.mat))
  haircut.vec <- as.vector(t(haircut.mat))
  cost.vec <- as.vector(t(cost.mat))
  quantity.vec <- as.vector(t(quantity.mat))
  value.vec <- as.vector(t(value.mat))
  minUnit.vec <- as.vector(t(minUnit.mat))
  
  output.list <- list(assetId=assetId,assetInfo=assetInfo,callInfo=callInfo,
                      eli.mat=eli.mat, eli.vec = eli.vec,
                      haircut.mat=haircut.mat, haircut.vec=haircut.vec,
                      cost.mat = cost.mat, cost.vec = cost.vec,
                      quantity.mat=quantity.mat, quantity.vec=quantity.vec,
                      value.mat=value.mat,value.vec=value.vec,
                      minUnit.mat=minUnit.mat, minUnit.vec=minUnit.vec,
                      call.mat = call.mat
                      )
  
  return (output.list)
}

