
ResourceInfo <- function(resource_vec,assetInfo_df,availAsset_df){
  ## better retrieve from DB
  ## keep useful columns from assetInfo
  ## asset id, name, currency, unitValue, minUnit, minUnitValue, FXRate
  assetId_vec <- SplitResource(resource_vec,'asset')
  custodianAccount_vec <- SplitResource(resource_vec,'custodianAccount')
  idx1_vec <- match(c('id', 'name', 'unitValue', 'minUnit', 'minUnitValue','currency', 'FXRate'),names(assetInfo_df))
  resourceInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),idx1_vec]

  ## add resource id, custodianAccount id, quantity
  resourceInfo_df <- cbind(id=resource_vec,resourceInfo_df,custodianAccount_vec)
  idx2_vec <- match(resource_vec, availAsset_df$assetCustacId)
  venue_vec <- availAsset_df$venue[idx2_vec] 
  quantity_vec <- availAsset_df$quantity[idx2_vec]
  resourceInfo_df <- cbind(resourceInfo_df[,1:3],quantity_vec, resourceInfo_df[,4:9],venue_vec)
  
  names(resourceInfo_df) <- c('id','assetId','assetName','quantity','unitValue', 'minUnit','minUnitValue','currency','FXRate',
                              'custodianAccount','venue')
  
  return(resourceInfo_df)
}

AvailAsset <- function(availAsset_df){
  ## keep useful columns
  ## "callId","assetCustacId","internalCost", "opptCost", "yield", "haircut","FXHaircut","externalCost","interestRate"
  idx_vec <- match(c("callId","assetCustacId","internalCost", "opptCost", "yield", "haircut","FXHaircut","externalCost","interestRate"),names(availAsset_df))
  new_df <- availAsset_df[,idx_vec]
  
  return(new_df)
}

AllocationInputDataTest <- function(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,resourceInfo_df){
  
  ### new identifer ####
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  callInfo_df$currency[which(is.na(callInfo_df$currency))] <- 'ZZZ' 
  availAsset_df <- availAsset_df[order(availAsset_df$callId),] # order the availAsset_df by callId_vec
  
  custodianAccount <- matrix(unlist(strsplit(resource_vec,'-')),nrow=2)[2,]
  venue <- availAsset_df$venue[match(resource_vec,availAsset_df$assetCustacId)]
  
    
  base_mat <- matrix(0,nrow=callNum,ncol=resourceNum, dimnames = list(callId_vec,resource_vec))
  eli_mat <- base_mat
  haircut_mat <- base_mat
  cost_mat <- base_mat
  quantity_mat <- base_mat
  minUnitQuantity_mat <- base_mat
  callAmount_mat <- base_mat
  
  unitValue_mat<- base_mat
  minUnit_mat <- base_mat  
  minUnitValue_mat <- base_mat
  FXRate_mat <- base_mat
  
  # fill in matrixes with the data from availAsset_df
  
  callAmount_mat[]<- matrix(rep(callInfo_df$callAmount,resourceNum),nrow=callNum,byrow=F)
                            
  idxTempCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxTempResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  quantity_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$quantity
  eli_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- 1
  haircut_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$haircut+availAsset_df$FXHaircut
  cost_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-(availAsset_df$interestRate+availAsset_df$yield)
  
  #### restructure!
  assetId_vec <- SplitResource(resource_vec,'asset') #### parallel with resource, not unique
  resourceInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]
  
  #unitValue_mat[] <- matrix(rep(resourceInfo_df$unitValue,callNum),nrow=callNum,byrow=TRUE)
  
  unitValue_mat[] <- matrix(rep(resourceInfo_df$unitValue/resourceInfo_df$FXRate,callNum),nrow=callNum,byrow=TRUE)
  minUnit_mat[]<- matrix(rep(resourceInfo_df$minUnit,callNum),nrow=callNum,byrow=TRUE)
  FXRate_mat[]<- matrix(rep(resourceInfo_df$FXRate,callNum),nrow=callNum,byrow=TRUE)
  minUnitValue_mat[] <- minUnit_mat*unitValue_mat
  minUnitQuantity_mat[]<- floor(quantity_mat/minUnit_mat) # round down to the nearest integer
  
  # convert the matrix format data to vector format
  # thinking of keeping only eligible parts
  eli_vec <- as.vector(t(eli_mat))
  haircut_vec <- as.vector(t(haircut_mat))
  cost_vec <- as.vector(t(cost_mat))
  quantity_vec <- as.vector(t(quantity_mat))
  minUnitQuantity_vec <- as.vector(t(minUnitQuantity_mat))
  unitValue_vec <- as.vector(t(unitValue_mat))
  minUnit_vec <- as.vector(t(minUnit_mat))
  FXRate_vec <- as.vector(t(FXRate_mat))
  minUnitValue_vec <- as.vector(t(minUnitValue_mat))
  callAmount_vec <- as.vector(t(callAmount_mat))
  
  output_list <- list(resource_vec=resource_vec,callId_vec=callId_vec,assetInfo_df=assetInfo_df,callInfo_df=callInfo_df,
                      custodianAccount=custodianAccount,venue=venue,
                      base_mat=base_mat,
                      FXRate_mat=FXRate_mat,FXRate_vec=FXRate_vec,
                      eli_mat=eli_mat, eli_vec = eli_vec,
                      haircut_mat=haircut_mat, haircut_vec=haircut_vec,
                      cost_mat = cost_mat, cost_vec = cost_vec,
                      quantity_mat=quantity_mat, quantity_vec=quantity_vec,
                      minUnitQuantity_mat=minUnitQuantity_mat,minUnitQuantity_vec=minUnitQuantity_vec,
                      unitValue_mat=unitValue_mat,unitValue_vec=unitValue_vec,
                      minUnit_mat=minUnit_mat, minUnit_vec=minUnit_vec,
                      minUnitValue_mat=minUnitValue_mat,minUnitValue_vec= minUnitValue_vec,
                      callAmount_mat = callAmount_mat,callAmount_vec=callAmount_vec
  )
  return (output_list)
}


AllocationInputData <- function(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df){
  
  ### new identifer ####
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  callInfo_df$currency[which(is.na(callInfo_df$currency))] <- 'ZZZ' 
  availAsset_df <- availAsset_df[order(availAsset_df$callId),] # order the availAsset_df by callId_vec
  custodianAccount <- matrix(unlist(strsplit(resource_vec,'-')),nrow=2)[2,]
  venue <- availAsset_df$venue[match(resource_vec,availAsset_df$assetCustacId)]
  
  ###############################################
  # eligibility matrix: 1-eligible, 0-ineligible
  # haircut matrix: haircut+FX haircut
  # tempQuantity_vec matrix
  # unitValue matrix: unitValue/FX rate
  # cost matrix: internal+external+opptunity-yield(interestRate)
  # call amount matrix: duplicate the column
  # minUnit matrix: minUnit[i,j]=x, asset j for margin call i has a minimum denomination x,
  
  #     which means we can only allocate the integral multiples tempQuantity_vec of A_j to MC_i.
  #     To start with, we use (i>=1) for non-cash securities; 0.01 for cash, apply to all margin calls.
  ############################################
  
  base_mat <- matrix(0,nrow=callNum,ncol=resourceNum, dimnames = list(callId_vec,resource_vec))
  eli_mat <- base_mat
  haircut_mat <- base_mat
  cost_mat <- base_mat
  quantity_mat <- base_mat
  minUnitQuantity_mat <- base_mat
  callAmount_mat <- base_mat
  
  unitValue_mat<- base_mat
  minUnit_mat <- base_mat  
  minUnitValue_mat <- base_mat
  FXRate_mat <- base_mat
  
  # fill in matrixes with the data from availAsset_df
  
  callAmount_mat[]<- matrix(rep(callInfo_df$callAmount,resourceNum),nrow=callNum,byrow=F)
  idxTempCallId_vec <- match(availAsset_df$callId,callId_vec)
  
  #resource_vec <- availAsset_df$assetCustacId
  idxTempResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  quantity_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$quantity
  eli_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- 1
  haircut_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$haircut+availAsset_df$FXHaircut
  cost_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-(availAsset_df$interestRate+availAsset_df$yield)
  
  #### restructure!
  assetId_vec <- SplitResource(resource_vec,'asset') #### parallel with resource, not unique
  resourceInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]
  
  #unitValue_mat[] <- matrix(rep(resourceInfo_df$unitValue,callNum),nrow=callNum,byrow=TRUE)
  
  unitValue_mat[] <- matrix(rep(resourceInfo_df$unitValue/resourceInfo_df$FXRate,callNum),nrow=callNum,byrow=TRUE)
  minUnit_mat[]<- matrix(rep(resourceInfo_df$minUnit,callNum),nrow=callNum,byrow=TRUE)
  FXRate_mat[]<- matrix(rep(resourceInfo_df$FXRate,callNum),nrow=callNum,byrow=TRUE)
  minUnitValue_mat[] <- minUnit_mat*unitValue_mat
  minUnitQuantity_mat[]<- floor(quantity_mat/minUnit_mat) # round down to the nearest integer
  
  # convert the matrix format data to vector format
  # thinking of keeping only eligible parts
  eli_vec <- as.vector(t(eli_mat))
  haircut_vec <- as.vector(t(haircut_mat))
  cost_vec <- as.vector(t(cost_mat))
  quantity_vec <- as.vector(t(quantity_mat))
  minUnitQuantity_vec <- as.vector(t(minUnitQuantity_mat))
  unitValue_vec <- as.vector(t(unitValue_mat))
  minUnit_vec <- as.vector(t(minUnit_mat))
  FXRate_vec <- as.vector(t(FXRate_mat))
  minUnitValue_vec <- as.vector(t(minUnitValue_mat))
  callAmount_vec <- as.vector(t(callAmount_mat))
  
  output_list <- list(resource_vec=resource_vec,callId_vec=callId_vec,assetInfo_df=assetInfo_df,callInfo_df=callInfo_df,
                      custodianAccount=custodianAccount,venue=venue,
                      base_mat=base_mat,
                      FXRate_mat=FXRate_mat,FXRate_vec=FXRate_vec,
                      eli_mat=eli_mat, eli_vec = eli_vec,
                      haircut_mat=haircut_mat, haircut_vec=haircut_vec,
                      cost_mat = cost_mat, cost_vec = cost_vec,
                      quantity_mat=quantity_mat, quantity_vec=quantity_vec,
                      minUnitQuantity_mat=minUnitQuantity_mat,minUnitQuantity_vec=minUnitQuantity_vec,
                      unitValue_mat=unitValue_mat,unitValue_vec=unitValue_vec,
                      minUnit_mat=minUnit_mat, minUnit_vec=minUnit_vec,
                      minUnitValue_mat=minUnitValue_mat,minUnitValue_vec= minUnitValue_vec,
                      callAmount_mat = callAmount_mat,callAmount_vec=callAmount_vec
  )
  return (output_list)
}

