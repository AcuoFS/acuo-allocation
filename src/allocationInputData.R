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
  
  custodianAccount <- result$CustodianAccount[match(assetId,result$assetId)]
  
  ###############################################
  # eligibility matrix: 1-eligible, 0-ineligible
  # haircut matrix: haircut+FX haircut
  # quantity matrix
  # unitValue matrix: unitValue/FX rate
  # cost matrix: internal+external+opptunity-yield(interestRate)
  # call amount matrix: duplicate the column
  # minUnit matrix: minUnit[i,j]=x, asset j for margin call i has a minimum denomination x,
  #     which means we can only allocate the integral multiples quantity of A_j to MC_i.
  #     To start with, we use (i>=1) for non-cash securities; 0.01 for cash, apply to all margin calls.
  ############################################
  
  base.mat <- matrix(0,nrow=call.num,ncol=asset.num, dimnames = list(callId,assetId))
  
  eli.mat <- base.mat
  haircut.mat <- base.mat
  cost.mat <- base.mat
  quantity.mat <- base.mat
  minUnitQuantity.mat <- base.mat
  call.mat <- base.mat
  
  unitValue.mat<- base.mat
  minUnit.mat <- base.mat  
  minUnitValue.mat <- base.mat
  
  
  # fill in matrixes with the data from result
  call.mat[callId,] <- matrix(rep(callInfo$callAmount,asset.num),nrow=call.num,byrow=F)
  quantity.mat[,]<- matrix(rep(as.numeric(unique(cbind(result$assetId,result$quantity))[,2]),call.num),nrow=call.num,byrow=T)
  
  eli.mat[cbind(result$callId,result$assetId)]<-1
  haircut.mat[cbind(result$callId,result$assetId)]<- result$haircut+result$FXHaircut
  cost.mat[cbind(result$callId,result$assetId)]<- result$internalCost+result$externalCost+result$opptCost-result$interestRate
  
  unitValue.mat[cbind(result$callId,result$assetId)] <- result$unitValue/result$FXRate
  minUnit.mat[,]<- matrix(rep(assetInfo$minUnit,call.num),nrow=call.num,byrow=TRUE)
  minUnitValue.mat[cbind(result$callId,result$assetId)]<- result$minUnitValue/result$FXRate
  
  minUnitQuantity.mat[,]<- floor(quantity.mat/minUnit.mat) # round to the nearest integer smaller
  
  
  # convert the matrix format data to vector format
  eli.vec <- as.vector(t(eli.mat))
  haircut.vec <- as.vector(t(haircut.mat))
  cost.vec <- as.vector(t(cost.mat))
  quantity.vec <- as.vector(t(quantity.mat))
  minUnitQuantity.vec <- as.vector(t(minUnitQuantity.mat))
  unitValue.vec <- as.vector(t(unitValue.mat))
  minUnit.vec <- as.vector(t(minUnit.mat))
  minUnitValue.vec <- as.vector(t(minUnitValue.mat))
  
  output.list <- list(assetId=assetId,assetInfo=assetInfo,callInfo=callInfo,custodianAccount=custodianAccount,
                      eli.mat=eli.mat, eli.vec = eli.vec,
                      haircut.mat=haircut.mat, haircut.vec=haircut.vec,
                      cost.mat = cost.mat, cost.vec = cost.vec,
                      quantity.mat=quantity.mat, quantity.vec=quantity.vec,
                      minUnitQuantity.mat=minUnitQuantity.mat,minUnitQuantity.vec=minUnitQuantity.vec,
                      unitValue.mat=unitValue.mat,unitValue.vec=unitValue.vec,
                      minUnit.mat=minUnit.mat, minUnit.vec=minUnit.vec,
                      minUnitValue.mat=minUnitValue.mat,minUnitValue.vec= minUnitValue.vec,
                      call.mat = call.mat
                      )
  
  return (output.list)
}

