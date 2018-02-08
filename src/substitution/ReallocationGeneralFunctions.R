#### OTHER FUNCTIONS ########
# function to replace all settled collaterals that are not optimal within a agreement-callType
ReplaceWithinAgreementAndCallType <- function(optimal_mat,callIdx,resource_vec,newSettledAmount_mat,newSettledQuantity_mat,resource_df,haircut_mat,operLimitAg){
  
  replacedNum <- 0
  
  # indexes of assets with the smallest score
  newIdx_vec <- which(optimal_mat[callIdx,]==min(optimal_mat[callIdx,]))
  resourceTemp <- resource_vec[newIdx_vec]
  idxAmountTemp <- which(newSettledAmount_mat[callIdx,]>0)
  
  # different scenarios
  # Scenario 1, all settled collaterals are optimal resources
  # then no reallocation is needed
  
  # Scenario 2, some or none of settled collaterals are optimal resources
  # check for the settled collaterals that are not optimal
  
  idxIntersect <- intersect(resource_vec[idxAmountTemp],resourceTemp)
  
  if(length(idxIntersect) < length(idxAmountTemp)){ # scenario 2 
    # find the ones that are not optimal 
    replaceIdx_vec <- idxAmountTemp[which(is.na(match(resource_vec[idxAmountTemp],resourceTemp)))]
    
    # replace with the agreement with the movement constraint
    # need to choose the most beneficial replacements within limit
    # only choose the first operLimitAg to replace
    if(length(replaceIdx_vec)>operLimitAg){
      replaceIdx_vec <- replaceIdx_vec[1:operLimitAg]
    }
    
    for(idx in replaceIdx_vec){
      resourceSelect <- FindResourceToReplace(optimal_mat[callIdx,],newIdx_vec,idx,
                                              resource_df,resource_vec)
      # execute the subsitution in the matrix
      idxResourceSelect <- which(resource_vec==resourceSelect)
      resourceAmount <- newSettledAmount_mat[callIdx,idxResourceSelect]
      # post haircut amount
      resourceAmountCall <- resourceAmount*(1-haircut_mat[callIdx,idxResourceSelect])
      
      # check the sufficiency of the optimal resources
      optimalResourceAmount_vec <- resource_df$minUnitValue[newIdx_vec]*resource_df$qtyMin[match(resource_vec[newIdx_vec],resource_df$id)]
      # post haircut amount 
      optimalResourceAmountCall_vec <- optimalResourceAmount_vec*(1-haircut_mat[callIdx,newIdx_vec])
      idxTemp <- newIdx_vec[which(optimalResourceAmountCall_vec > resourceAmountCall)]
      if(length(idxTemp)==0){
        # insufficient
        # check other possibilities? the other resourceSelect
        # if no, then 1. skip OR 2. replace partial
        # do nothing for now
      } else {
        idxNew <- idxTemp[1] # choose the first one
        
        # replace
        # update the newSettledAmount_mat and resource_df
        qtyMinNew <- IntegralUnitQuantity(resourceAmountCall,haircut_mat[callIdx,idxNew],resource_df$minUnitValue[idxNew])
        
        amountNew <- IntegralUnitAmount(resourceAmountCall,haircut_mat[callIdx,idxNew],resource_df$minUnitValue[idxNew])
        
        # update newSettledAmount_mat
        newSettledAmount_mat[callIdx,idxNew] <- amountNew
        newSettledAmount_mat[callIdx,idxResourceSelect] <- 0
        
        # update newSettledQuantity_mat
        newSettledQuantity_mat[callIdx,idxNew] <- qtyMinNew
        newSettledQuantity_mat[callIdx,idxResourceSelect] <- 0
        
        # update resource_df
        # update $qtyMin 
        resource_df$qtyMin[idxNew] <- resource_df$qtyMin[idxNew] - qtyMinNew
        resource_df$qtyMin[idxResourceSelect] <- resource_df$qtyMin[idxResourceSelect] + newSettledQuantity_mat[callIdx,idxResourceSelect]
        replacedNum <- replacedNum+1
      }
    }
  }
  return(list(newSettledAmount_mat=newSettledAmount_mat,newSettledQuantity_mat=newSettledQuantity_mat,resource_df=resource_df,replacedNum=replacedNum))
}

# function to calculate integral minumal units quantity
IntegralUnitQuantity <- function(amount,haircut,minUnitValue){
  integralUnitQuantity <- ceiling(amount/(1-haircut)/minUnitValue)
  return(integralUnitQuantity)
}

# function to calculate integral units amount
IntegralUnitAmount <- function(amount,haircut,minUnitValue){
  integralUnitAmount <- (1-haircut)*minUnitValue*ceiling(amount/(1-haircut)/minUnitValue)
  return(integralUnitAmount)
}

# function to select the asset to be substitute
FindResourceToReplace <- function(optimal_vec,idxResourceTemp,idxAmountTemp,resource_df,resource_vec){
  
  # find the highest score from the existing resources
  # if more than one, then choose the one with largest amount settled to start
  idxSelect <- idxAmountTemp[which(optimal_vec[idxAmountTemp]==max(optimal_vec[idxAmountTemp]))]
  amountSelect <- resource_df$minUnitValue[idxSelect]*resource_df$qtyMin[match(resource_vec[idxSelect],resource_df$id)]
  if(length(idxSelect)==1){
    resourceSelect <- resource_vec[idxSelect]
  } else if(length(idxSelect)>1){
    resourceSelect <- resource_vec[idxSelect[which.max(amountSelect)]]
  }
  return(resourceSelect)
}

# function to construct settled collateral amount into an allocation matrix
SettledAmountVec2Mat <- function(settledCollaterals,resource_vec,callId_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  settledAmount_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
  settledAmount_mat[cbind(match(settledCollaterals$newCallId,callId_vec),match(settledCollaterals$resource,resource_vec))] <- settledCollaterals$amount
  return(settledAmount_mat)
}

# function to construct settled collateral quantity into an allocation matrix
SettledQuantityVec2Mat <- function(settledCollaterals,resource_vec,callId_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  settledQuantity_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
  settledQuantity_mat[cbind(match(settledCollaterals$newCallId,callId_vec),match(settledCollaterals$resource,resource_vec))] <- settledCollaterals$quantity
  return(settledQuantity_mat)
}

# function to aggregate margin calls by agreement and callType
AggregateCallByAgreementAndCallType <- function(settledCollaterals,availAsset_df,callInfo_df){
  # differentiate initial calls and variation calls 
  # because they have different eligibility and haircut rules
  
  ## newSettledCollaterals
  newSettledCollaterals <- aggregate(cbind(quantity,amount)~agreement+marginType+asset+custodianAccount,data=settledCollaterals,sum)
  newSettledCollaterals$newCallId <- PasteResource(newSettledCollaterals$agreement,newSettledCollaterals$marginType)
  newSettledCollaterals$currency <- settledCollaterals$currency[match(newSettledCollaterals$asset,settledCollaterals$asset)]
  
  ## newAvailAsset_df
  newAvailAsset_df <- availAsset_df
  # change callId_vec to (agreementId---marginType)
  agreements <- settledCollaterals$agreement[match(callId_vec,settledCollaterals$call)]
  marginTypes <- settledCollaterals$marginType[match(callId_vec,settledCollaterals$call)]
  agreementCallType_vec <- PasteResource(agreements,marginTypes)
  
  # add newId column to newAvailAsset_df
  oriCallId_vec <- newAvailAsset_df$callId
  newAvailAsset_df$callId <- agreementCallType_vec[match(newAvailAsset_df$callId,callId_vec)]
  newAvailAsset_df <- unique(newAvailAsset_df)
  
  idxKeep_vec <- which(!duplicated(newAvailAsset_df))
  newAvailAsset_df$oriCallId <- oriCallId_vec[idxKeep_vec]
  
  ## newCallInfo_df
  newCallInfo_df <- callInfo_df
  # add agreement id to newCallInfo_df
  newCallInfo_df$newId <- agreementCallType_vec
  newCallInfo_df$agreement <- agreements
  # aggregate
  temp_df <- aggregate(callAmount~newId,newCallInfo_df,sum)
  # mock the other data
  idxKeep_vec <- which(!duplicated(newCallInfo_df$newId))
  newCallInfo_df <- newCallInfo_df[idxKeep_vec,]
  newCallInfo_df$callAmount <-temp_df$callAmount[match(newCallInfo_df$newId,temp_df$newId)]
  
  
  return(list(newSettledCollaterals=newSettledCollaterals,newAvailAsset_df=newAvailAsset_df,newCallInfo_df=newCallInfo_df))
}

# function to aggregate margin calls by asset, agreement and callType
AggregateCallByAssetAgreementAndCallType <- function(settledCollaterals,availAsset_df){
  # differentiate initial calls and variation calls 
  # because they have different eligibility and haircut rules
  callId_vec <- settledCollaterals$call
  ## newSettledCollaterals
  newSettledCollaterals <- aggregate(cbind(quantity,amount)~resource+agreement+marginType+asset+custodianAccount,data=settledCollaterals,sum)
  newSettledCollaterals$newCallId <- PasteNewCallId(newSettledCollaterals$resource,newSettledCollaterals$agreement,newSettledCollaterals$marginType,type="resource-_-agreement-_-callType")
  newSettledCollaterals$currency <- settledCollaterals$currency[match(newSettledCollaterals$asset,settledCollaterals$asset)]
    
  ## newAvailAsset_df
  newAvailAsset_df <- availAsset_df
  # change callId_vec to (agreementId---marginType)
  resources <- settledCollaterals$resource[match(callId_vec,settledCollaterals$call)]
  agreements <- settledCollaterals$agreement[match(callId_vec,settledCollaterals$call)]
  marginTypes <- settledCollaterals$marginType[match(callId_vec,settledCollaterals$call)]
  agreementCallType_vec <-PasteNewCallId(resources,agreements,marginTypes,type="resource-_-agreement-_-callType")
  
  
  # add newId column to newAvailAsset_df
  oriCallId_vec <- newAvailAsset_df$callId
  newAvailAsset_df$callId <- agreementCallType_vec[match(newAvailAsset_df$callId,callId_vec)]
  newAvailAsset_df <- unique(newAvailAsset_df)
  
  idxKeep_vec <- which(!duplicated(newAvailAsset_df))
  newAvailAsset_df$oriCallId <- oriCallId_vec[idxKeep_vec]
  
  ## newCallInfo_df
  newCallInfo_df <- settledCollaterals
  colnames(newCallInfo_df)[which(colnames(newCallInfo_df)=="amount")] <- "callAmount"
  
  # add agreement id to newCallInfo_df
  newCallInfo_df$newId <- agreementCallType_vec
  newCallInfo_df$agreement <- agreements
  # aggregate
  temp_df <- aggregate(callAmount~newId,newCallInfo_df,sum)
  # mock the other data
  idxKeep_vec <- which(!duplicated(newCallInfo_df$newId))
  newCallInfo_df <- newCallInfo_df[idxKeep_vec,]
  newCallInfo_df$callAmount <-temp_df$callAmount[match(newCallInfo_df$newId,temp_df$newId)]
  
  
  return(list(newSettledCollaterals=newSettledCollaterals,newAvailAsset_df=newAvailAsset_df,newCallInfo_df=newCallInfo_df))
}

# function to construct id: asset-_-agreement-_-callType
PasteNewCallId <- function(assetId_vec,agreement_vec,callType_vec,type){
  if(type=="agreement-_-callType"){
    temp <- paste(agreement_vec,callType_vec,sep='-_-')
  } else if(type=="resource-_-agreement-_-callType"){
    temp <- paste(agreement_vec,callType_vec,sep='-_-')
    temp <- paste(assetId_vec,temp,sep='-_-')
  }
  return(temp)
}

# function to split id: asset-_-agreement-_-callType
SplitNewCallId <- function(newCallId_vec,target,type){
  if(type=="agreement-_-callType"){
    newCallId_mat <- matrix(unlist(strsplit(newCallId_vec,'-_-')),nrow=2)
    if(target=="agreement"){
      temp <- newCallId_mat[1,]
    } else if(target=="callType"){
      temp <- newCallId_mat[2,]
    }
  } else if(type=="resource-_-agreement-_-callType"){
    newCallId_mat <- matrix(unlist(strsplit(newCallId_vec,'-_-')),nrow=3)
    if(target=="resource"){
      temp <- newCallId_mat[1,]
    } else if(target=="agreement"){
      temp <- newCallId_mat[2,]
    } else if(target=="callType"){
      temp <- newCallId_mat[3,]
    }
  }
  return(temp)
}

# function to construct the decision variable info
VarInfoAgreement <- function(eli_vec,callInfo_df,resource_vec,callId_vec){
  callNum <- length(callId_vec)
  resourceNum <- length(resource_vec)
  idxEli_vec <- which(eli_vec==1)
  
  # matrix store the index number, by row
  idx_mat <- matrix(1:(callNum*resourceNum),nrow=callNum,byrow = TRUE,dimnames = list(callId_vec,resource_vec))
  # matrix store the variable name("msId_mcId_assetCustId"), by row
  fullNameOri_mat <-  matrix('',nrow=callNum,ncol=resourceNum,byrow = TRUE,dimnames = list(callId_vec,resource_vec))
  
  # new dummy for "msId_assetCustId"
  newNameOri_mat <- matrix('',nrow=callNum,ncol=resourceNum,byrow = TRUE,dimnames = list(callId_vec,resource_vec))
  
  
  for(i in 1:callNum){
    agreement <- callInfo_df$agreement[i]
    fullNameOri_mat[i,]<-PasteVarName(agreement,callId_vec[i],resource_vec)
    newNameOri_mat[i,] <- PasteVarName(agreement,'dummy',resource_vec)
  }
  varNameOri_vec <- t(fullNameOri_mat)[idxEli_vec]
  newNameOri_vec <- t(newNameOri_mat)[idxEli_vec]
  
  # remove duplicated variables constructed by same asset in one statement for different calls
  newNameDummy_vec <- unique(newNameOri_vec) 
  
  # use the dummies to construct the indicator
  # same number means same asset for same statement: 1,2,3,4,1,2,4,5,...
  pos_vec <- match(newNameOri_vec,newNameDummy_vec)
  
  varName_vec <- c(varNameOri_vec,newNameDummy_vec)
  
  varNum <- length(varNameOri_vec)
  varNum2 <- length(varName_vec)
  
  var_list <- list(varName_vec=varName_vec,varNum=varNum,varNum2=varNum2,pos_vec=pos_vec)
  return(var_list)
}

# function to calculate total movements
OperationFunAgreement <- function(result,callInfo_df,method){
  movements <- 0
  if(method=='matrix'){
    result_mat <- result
    resultDummy_mat <- 1*(result_mat&1)
    agreementDul_vec <- callInfo_df$agreement
    agreement_vec <- unique(agreementDul_vec)
    
    if(length(result_mat[1,])==1){
      for(m in 1:length(agreement_vec)){
        idxTemp_vec <- which(agreementDul_vec==agreement_vec[m])
        if(length(idxTemp_vec)==1){
          movements <- movements+sum(resultDummy_mat[idxTemp_vec])
        } else{
          movements <- movements+max(resultDummy_mat[idxTemp_vec])
        }
      }
    } else{
      for(m in 1:length(agreement_vec)){
        idxTemp_vec <- which(agreementDul_vec==agreement_vec[m])
        if(length(idxTemp_vec)==1){
          movements <- movements+sum(resultDummy_mat[idxTemp_vec,])
        } else{
          movements <- movements+sum(apply(resultDummy_mat[idxTemp_vec,],2,max))
        }
      }
    }
  } else if(method=='msList'){
    agreementOutput_list <- result
    agreement_vec <- unique(callInfo_df$agreement)
    agreementNum <- length(agreement_vec)
    for(i in 1:agreementNum){
      msId <- agreement_vec[i]
      agreementAlloc_df <- agreementOutput_list[[agreement]]
      resources <- unique(PasteResource(agreementAlloc_df$Asset,agreementAlloc_df$CustodianAccout))
      movements <- movements + length(resources)
    }
  } else{
    stop('ALERR3005: Invalid OperationFun input method!')
  }
  
  return(movements)
}

# function to construct important matrix
AssetByCallInfoAgreement <- function(callId_vec,resource_vec,availAsset_df,newSettledCollaterals){
  
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  availAsset_df <- availAsset_df[order(availAsset_df$callId),] # order the availAsset_df by callId_vec
  
  base_mat <- matrix(0,nrow=callNum,ncol=resourceNum, dimnames = list(callId_vec,resource_vec))
  eli_mat <- base_mat
  haircut_mat <- base_mat
  haircutC_mat <- base_mat
  haircutFX_mat <- base_mat
  cost_mat <- base_mat
  
  # fill in matrixes with the data from availAsset_df
  idxTempCallId_vec <- match(availAsset_df$callId,callId_vec)
  idxTempResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  eli_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- 1
  haircutC_mat[cbind(idxTempCallId_vec,idxTempResource_vec)] <- availAsset_df$haircut
  haircutFX_mat[cbind(idxTempCallId_vec,idxTempResource_vec)] <- availAsset_df$FXHaircut
  haircut_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$haircut+availAsset_df$FXHaircut
  
  # cost calculation: difference between old and new, then to minimize
  baseResourceOrder_vec <- match(SplitNewCallId(availAsset_df$callId,target="resource",type="resource-_-agreement-_-callType"),availAsset_df$assetCustacId)
  baseAvailAsset <- availAsset_df[baseResourceOrder_vec,]
  
  cost_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- CostDefinition(baseAvailAsset)-CostDefinition(availAsset_df)
  
  
  # convert the matrix format data to vector format
  # thinking of keeping only eligible parts
  eli_vec <- as.vector(t(eli_mat))
  haircut_vec <- as.vector(t(haircut_mat))
  cost_vec <- as.vector(t(cost_mat))
  
  output_list <- list(base_mat=base_mat,eli_mat=eli_mat,haircut_mat=haircut_mat,haircutC_mat=haircutC_mat,haircutFX_mat=haircutFX_mat,cost_mat=cost_mat)
  return (output_list)
}

# function to group agreement-callType by agreement
GroupNewIdByAgreement <- function(agreementCallTypeLimit,agreementLimit,newCallInfo_df,agreementCallType_vec){
  
  groupNewCallId_list <- list()
  # if the total call numbers is equal or less than limitTotal, only one group
  if(length(newCallInfo_df[,1])<=agreementCallTypeLimit){
    groupNewCallId_list[[1]] <- agreementCallType_vec
  } else if(length(unique(newCallInfo_df$agreement))<=agreementLimit){
    groupNewCallId_list[[1]] <- agreementCallType_vec
  } else{
    groupAgreement_list <- list()
    agreement_vec <- unique(newCallInfo_df$agreement)
    agreementGroupNum <- ceiling(length(agreement_vec)/agreementLimit)
    
    for(i in 1:(agreementGroupNum-1)){
      tempCurrent <- agreementLimit*(i-1)
      tempAgreement_vec <- agreement_vec[(tempCurrent+1):(tempCurrent+agreementLimit)]
      tempNewCall_vec <- newCallInfo_df$newId[which((newCallInfo_df$agreement) %in% tempAgreement_vec)]
      groupAgreement_list[[i]]<- tempAgreement_vec
      groupNewCallId_list[[i]]<- tempNewCall_vec
    }
    tempCurrent <- agreementLimit*(agreementGroupNum-1)
    tempAgreement_vec <- na.omit(agreement_vec[(tempCurrent+1):(tempCurrent+agreementLimit)])
    tempNewCall_vec <- newCallInfo_df$newId[which((newCallInfo_df$agreement) %in% tempAgreement_vec)]
    groupAgreement_list[[agreementGroupNum]]<- tempAgreement_vec
    groupNewCallId_list[[agreementGroupNum]]<- tempNewCall_vec
  }
  return(groupNewCallId_list)
}


ResultMat2ListAgreement <- function(result_mat,newCallId_vec,resource_vec,callInfo_df, haircutC_mat,haircutFX_mat,cost_mat,resourceInfo_df,
                                    newCallSelect_list,agreementSelect_list){
  
  newCallNum <- length(newCallId_vec)
  
  #### construct the result
  for(i in 1:newCallNum){      
    # resource and result_mat columns have the same order 
    # the allocated indexes: 
    idx_vec <- which(result_mat[i,]!=0) 
    if(length(idx_vec)==0){
      errormsg <- paste("ALEER3004: There's no asset allocated to margin call",newCallId_vec[i])
      stop(errormsg)
    }
    
    alloc_df <- ConstructAllocDfAgreement(resourceInfo_df[idx_vec,], callInfo_df[i,], haircutC_mat[i,idx_vec], haircutFX_mat[i,idx_vec],result_mat[i,idx_vec],cost_mat[i,idx_vec])
    
    #### UPDATE THE ASSET QUANTITY START ########
    resourceInfo_df$quantity[idx_vec] <- resourceInfo_df$quantity[idx_vec]-alloc_df$Quantity #selectAssetQuantity_vec
    
    #### UPDATE THE ASSET QUANTITY END ##########
    #### Update newCallSelect_list Start ####
    newCallSelect_list[[newCallId_vec[i]]] <- alloc_df
    #### Update newCallSelect_list END ######
    
    #### Update agreementSelect_list Start ######
    agreement <- callInfo_df$agreement[i]
    if(is.null(agreementSelect_list[[agreement]])){
      agreementSelect_list[[agreement]] <- alloc_df
    } else{
      tempAlloc_df <- agreementSelect_list[[agreement]]
      alloc_df <- rbind(alloc_df,tempAlloc_df)
      rownames(alloc_df)<- 1:length(alloc_df[,1])
      agreementSelect_list[[agreement]] <- alloc_df
    }
    #### Update agreementSelect_list END ########
  }
  result_list <- list(newCallSelect_list=newCallSelect_list,agreementSelect_list=agreementSelect_list)
  return(result_list)
}

#### convertFunctions #### 
ConstructAllocDfAgreement <- function(resourceInfo_df,newCallInfo_df,haircutC_vec,haircutFX_vec,minUnitQuantity_vec,cost_vec ){
  
  oriCallId_vec <- newCallInfo_df$id
  newCallInfo_df$id <- newCallInfo_df$newId
  
  assetId_vec <- resourceInfo_df$assetId
  assetCustodianAccount_vec <- resourceInfo_df$custodianAccount
  assetVenue_vec <- resourceInfo_df$venue
  assetName_vec <- resourceInfo_df$assetName
  assetHaircut_vec <- haircutC_vec+haircutFX_vec
  assetHaircutC_vec <- haircutC_vec
  ## add fx haircut
  assetHaircutFX_vec <- haircutFX_vec
  assetCostFactor_vec <- cost_vec
  assetCurrency_vec <- resourceInfo_df$currency
  assetMinUnitQuantity_vec <- minUnitQuantity_vec
  assetQuantity_vec <- minUnitQuantity_vec*resourceInfo_df$minUnit
  marginType_vec <- rep(newCallInfo_df$marginType,length(cost_vec))
  agreement_vec <- rep(newCallInfo_df$agreement,length(cost_vec))
  newCall_vec <- rep(newCallInfo_df$id,length(cost_vec))
  
  assetFX_vec <- resourceInfo_df$FXRate
  #oriAssetFX_vec <- resourceInfo_df$oriFXRate
  assetUnitValue_vec <- resourceInfo_df$unitValue/assetFX_vec
  
  assetAmountUSD_vec <- assetQuantity_vec*assetUnitValue_vec
  assetNetAmountUSD_vec <- assetAmountUSD_vec*(1-assetHaircut_vec)
  
  assetCost_vec <- assetCostFactor_vec*assetAmountUSD_vec
  
  assetAmount_vec <- assetAmountUSD_vec*assetFX_vec
  assetNetAmount_vec <- assetNetAmountUSD_vec*assetFX_vec
  
  alloc_df <- data.frame(assetId_vec,assetName_vec,assetNetAmount_vec,assetNetAmountUSD_vec,assetFX_vec,assetHaircut_vec,assetHaircutC_vec,assetHaircutFX_vec,assetAmount_vec,assetAmountUSD_vec,
                         assetCurrency_vec,assetQuantity_vec,assetCustodianAccount_vec,assetVenue_vec,marginType_vec,agreement_vec,newCall_vec,assetCostFactor_vec,assetCost_vec)
  colnames(alloc_df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Hc','Hfx','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType',
                         'agreement','agreementCallType','CostFactor','Cost')
  rownames(alloc_df)<- 1:length(alloc_df[,1])
  return(alloc_df)
}

