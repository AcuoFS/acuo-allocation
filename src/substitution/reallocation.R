
Reallocation <- function(settledCollaterals,availAsset_df,callInfo_df,resource_df,pref_vec,operLimit,operLimitAg_vec,fungible){
  
  callId_vec <- callInfo_df$id
  resource_vec <- resource_df$id
  
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  
  pref_vec <- pref_vec/sum(pref_vec[1:2])
  
  #### parameters ############
  
  
  aggregateInfo_list <- AggregateCallByAgreementAndCallType(settledCollaterals,availAsset_df,callInfo_df)
  
  newSettledCollaterals <- aggregateInfo_list$newSettledCollaterals
  newAvailAsset_df <- aggregateInfo_list$newAvailAsset_df
  newCallInfo_df <- aggregateInfo_list$newCallInfo_df
  
  agreementCallType_vec <- newCallInfo_df$newId
  agreementCallTypeNum <- length(agreementCallType_vec)
  agreement_vec <- unique(newSettledCollaterals$agreement)
  agreementNum <- length(agreement_vec)
  
  availInfo_list <- AssetByCallInfo(agreementCallType_vec,resource_vec,newAvailAsset_df)
  
  callAmount_mat <- matrix(rep(newCallInfo_df$callAmount,resourceNum),nrow=agreementCallTypeNum,byrow=F)
  unitValue_mat<- matrix(rep(resource_df$unitValue/resource_df$FXRate, agreementCallTypeNum),nrow=agreementCallTypeNum,byrow=T)
  minUnit_mat <- matrix(rep(resource_df$minUnit,agreementCallTypeNum),nrow=agreementCallTypeNum,byrow=T)
  minUnitValue_mat <- unitValue_mat*minUnit_mat
  haircutC_mat<-availInfo_list$haircutC_mat
  haircutFX_mat<-availInfo_list$haircutFX_mat
  haircut_mat<-availInfo_list$haircut_mat
  costBasis_mat <- availInfo_list$cost_mat 
  eli_mat <- availInfo_list$eli_mat
  quantity_mat <- matrix(rep(resource_df$qtyMin,callNum),nrow=callNum,byrow=T)
  
  
  objParams_list <- ConstructModelObj(callAmount_mat,minUnitValue_mat,haircut_mat,costBasis_mat,eli_mat,newCallInfo_df,
                                      agreementCallType_vec,resource_vec)
  
  # cost+liquidity matrix
  normCost_mat <- objParams_list$cost_mat
  normLiquidity_mat <- objParams_list$liquidity_mat 
  
  optimal_mat <- normCost_mat*pref_vec[1]+normLiquidity_mat*pref_vec[2]
  colnames(optimal_mat) <- resource_vec
  rownames(optimal_mat)<- agreementCallType_vec
  
  #### Substitute ######
  
  newSettledCollaterals$resource <- PasteResource(newSettledCollaterals$asset,newSettledCollaterals$custodianAccount)
  
  settledAmount_mat <- SettledAmountVec2Mat(newSettledCollaterals,resource_vec,agreementCallType_vec)
  newSettledAmount_mat <- settledAmount_mat
  
  settledQuantity_mat <- SettledQuantityVec2Mat(newSettledCollaterals,resource_vec,agreementCallType_vec)
  newSettledQuantity_mat <- settledQuantity_mat
  
  # apply the movement constraint
  # if fungible = FALSE, then apply operLimitAg_vec[i] on agreement[i]
  # if fungible = TRUE, then apply operLimit on all agreements
  if(!fungible){
    for(k in 1:agreementNum){
      agreement <- agreement_vec[k]
      # the index of variation or initial call type for this agreement
      idx_vec <- which(SplitResource(agreementCallType_vec,"asset")==agreement)
    
      # initial first
      if(length(idx_vec)==1){
        i <- idx_vec
        
        resultTemp <- ReplaceWithinAgreementAndCallType(optimal_mat,i,resource_vec,newSettledAmount_mat,newSettledQuantity_mat,resource_df,haircut_mat,operLimitAg_vec[k])
        newSettledAmount_mat <- resultTemp$newSettledAmount_mat
        newSettledQuantity_mat <- resultTemp$newSettledQuantity_mat
        resource_df <- resultTemp$resource_df
        
      } else if(length(idx_vec)==2){
          idxTemp <- which(SplitResource(agreementCallType_vec[idx_vec],"custodianAccount")=="Initial")
          i <- idx_vec[idxTemp]
          
          resultTemp <- ReplaceWithinAgreementAndCallType(optimal_mat,i,resource_vec,newSettledAmount_mat,newSettledQuantity_mat,resource_df,haircut_mat,operLimitAg_vec[k])
          newSettledAmount_mat <- resultTemp$newSettledAmount_mat
          newSettledQuantity_mat <- resultTemp$newSettledQuantity_mat
          resource_df <- resultTemp$resource_df
          
          if(length(resultTemp$replacedNum) < operLimitAg_vec[k]){
            # check variation
            idxTemp <- which(SplitResource(agreementCallType_vec[idx_vec],"custodianAccount")=="Variation")
            i <- idx_vec[idxTemp]
            
            resultTemp <- ReplaceWithinAgreementAndCallType(optimal_mat,i,resource_vec,newSettledAmount_mat,newSettledQuantity_mat,resource_df,haircut_mat,operLimitAg_vec[k]-resultTemp$replacedNum)
            newSettledAmount_mat <- resultTemp$newSettledAmount_mat
            newSettledQuantity_mat <- resultTemp$newSettledQuantity_mat
            resource_df <- resultTemp$resource_df
          }
      }
    }
  } else{
    # replace by order of the agreement-calltype
    leftMovement <- operLimit
    for(i in 1:agreementCallTypeNum){
      resultTemp <- ReplaceWithinAgreementAndCallType(optimal_mat,i,resource_vec,newSettledAmount_mat,newSettledQuantity_mat,resource_df,haircut_mat,operLimitAg_vec[k])
      newSettledAmount_mat <- resultTemp$newSettledAmount_mat
      newSettledQuantity_mat <- resultTemp$newSettledQuantity_mat
      resource_df <- resultTemp$resource_df
      
      leftMovement <- leftMovement - resultTemp$replacedNum
      if(leftMovement==0){
        break
      }
    }
  }
  
  # initialize the newCallSelect_list and agreementSelect_list
  newCallSelect_list  <- list()    
  agreementSelect_list <- list()  
  newAllocation_list <- ResultMat2ListAgreement(newSettledQuantity_mat,agreementCallType_vec,resource_vec,newCallInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df,
                                                newCallSelect_list,agreementSelect_list)
  # allocation changes
  newCallChange_list <- list()
  agreementChange_list <- list()
  leftSettledQuantity_mat <- newSettledQuantity_mat-settledQuantity_mat
  if(agreementCallTypeNum>1){
    idxChange_vec <- apply(leftSettledQuantity_mat&matrix(1,nrow=agreementCallTypeNum,ncol=resourceNum),1,sum)
    idxTemp <- which(idxChange_vec!=0)
    leftSettledQuantity_mat <- leftSettledQuantity_mat[idxTemp,]
    leftSettledQuantity_mat <- matrix(leftSettledQuantity_mat,nrow=length(idxTemp))
    changeAllocation_list <- ResultMat2ListAgreement(leftSettledQuantity_mat,callId_vec[idxTemp],resource_vec,newCallInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df,
                                            newCallChange_list,agreementSelect_list)
  } else{
    if(length(which(leftSettledQuantity_mat!=0)) == 1){
      changeAllocation_list <- ResultMat2ListAgreement(leftSettledQuantity_mat,callId_vec,resource_vec,newCallInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df,
                                              newCallChange_list,agreementSelect_list)
    }
  }
  
  
  return(list(newAllocation_list=newAllocation_list,changeAllocation_list=changeAllocation_list))
}
