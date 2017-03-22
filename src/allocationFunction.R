
#### ALLOCATION MAIN FUNCTION ############
AllocationAlgo <- function(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,timeLimit,inputLimit_vec,callOrderMethod){
  ########### CONSTANTS ################################
  if(missing(callOrderMethod)){
    callOrderMethod <-2
  }
  imLimit <- inputLimit_vec[1]
  vmLimit <- inputLimit_vec[2]
  callLimit <- inputLimit_vec[3]
  msLimit <- inputLimit_vec[4]
  ########### END ######################################
  
  #### ORDER THE CALL ID ################################################
  ## method 0: Keep original
  ## method 1: By margin call amount, decreasing
  ## method 2: By margin type, VM then IM; sub order by call amount
  ## method 3: By total call amount in margin statement, decreasing
  callInfo_df <- OrderCallId(callOrderMethod,callInfo_df)
  callId_vec <- callInfo_df$id
  msId_vec <- unique(callInfo_df$marginStatement)
  ######## END ###########################################################
  
  ######## SPLIT the call ids in to several groups #######################
  # method 1: group by marginType

  # maximum limitVm VM or limitIm IM a time
  groupCallId_list <- GroupCallIdByMs(callLimit,msLimit,callInfo_df,callId_vec)
  ############# END ###################################################
  
  
  ############### PROCESSING DATA #####################################
  
  ### new identifer ####
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  
  # allocate one group a time
  # after each allocation, update the tempQuantity_vec of each asset
  callOutput_list <- list()
  msOutput_list <- list()
  checkCall_mat <- matrix(c(callInfo_df$callAmount,rep(0,callNum)),nrow=callNum, dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
  
  ############ ITERATE THE GROUP, RUN THE ALGO #########################
 
  for(i in 1:length(groupCallId_list)){
    callIdGroup_vec <- groupCallId_list[[i]]
    msIdGroup_vec <- unique(callInfo_df$marginStatement[which(callInfo_df$id %in% callIdGroup_vec)])
    #cat(' group:',i,'\n','callId_vec:',callIdGroup_vec,'\n')
    callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
    availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
    
    ### new identifer ####
    resourceGroup_vec <- unique(availAssetGroup_df$assetCustacId)
    assetIdGroup_vec <- as.character(data.frame(strsplit(resourceGroup_vec,'-'))[1,])
    assetInfoGroup_df <- assetInfo_df[match(assetIdGroup_vec,assetInfo_df$id),]
    
    # input data to the core Algo
    coreInput_list <- AllocationInputData(callIdGroup_vec,resourceGroup_vec,callInfoGroup_df,availAssetGroup_df,assetInfoGroup_df,pref_vec)
    
    # core Algo, assume all data comes in a list
    resultGroup_list <- CoreAlgo(coreInput_list,availAssetGroup_df,timeLimit,pref_vec)
    msOutputGroup_list <- resultGroup_list$msOutput_list
    callOutputGroup_list <- resultGroup_list$callOutput_list
    status <- resultGroup_list$status
    lpsolveRun <- resultGroup_list$lpsolveRun
    checkCallGroup_mat <- resultGroup_list$checkCall_mat
    availAsset_df <- resultGroup_list$availAsset_df

    for(k in 1:length(callIdGroup_vec)){
      callId <- callIdGroup_vec[k]
      j <- which(msIdGroup_vec==callInfo_df$marginStatement[which(callInfo_df$id==callId)])
      msId <- msId_vec[j]
      callOutput_list[[callId]] <- callOutputGroup_list[[callId]]
      msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
      checkCall_mat[which(rownames(checkCall_mat)==callId),2] <- checkCallGroup_mat[which(rownames(checkCallGroup_mat)==callId),2]
    }
  }
  return(list(msOutput=msOutput_list,callOutput=callOutput_list,checkCall_mat=checkCall_mat,status=status,lpsolveRun=lpsolveRun))
}


#### OTHER FUNCTIONS(CALLED IN THE MAIN FUNCTION)##########################
AllocationInputData <- function(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec){
  
  ### new identifer ####
  assetId_vec <- as.character(data.frame(strsplit(resource_vec,'-'))[1,])
  resourceNum <- length(resource_vec)
  callNum <- length(callId_vec)
  callInfo_df$currency[which(is.na(callInfo_df$currency))] <- 'ZZZ' 
  availAsset_df <- availAsset_df[order(availAsset_df$callId),] # order the availAsset_df by callId_vec
  custodianAccount <- as.character(data.frame(strsplit(resource_vec,'-'))[2,])
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

  # fill in matrixes with the data from availAsset_df
 
  callAmount_mat[]<- matrix(rep(callInfo_df$callAmount,resourceNum),nrow=callNum,byrow=F)
  idxTempCallId_vec <- match(availAsset_df$callId,callId_vec)
  
  #resource_vec <- availAsset_df$assetCustacId
  idxTempResource_vec <- match(availAsset_df$assetCustacId,resource_vec)
  
  quantity_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$quantity
  eli_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- 1
  haircut_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$haircut+availAsset_df$FXHaircut
  cost_mat[cbind(idxTempCallId_vec,idxTempResource_vec)]<- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-(availAsset_df$interestRate+availAsset_df$yield)
  
  unitValue_mat[] <- matrix(rep(assetInfo_df$unitValue/assetInfo_df$FXRate,callNum),nrow=callNum,byrow=TRUE)
  minUnit_mat[]<- matrix(rep(assetInfo_df$minUnit,callNum),nrow=callNum,byrow=TRUE)
  minUnitValue_mat[] <- matrix(rep(assetInfo_df$minUnitValue/assetInfo_df$FXRate,callNum),nrow=callNum,byrow=TRUE)
  minUnitQuantity_mat[]<- floor(quantity_mat/minUnit_mat) # round down to the nearest integer

  # convert the matrix format data to vector format
  eli_vec <- as.vector(t(eli_mat))
  haircut_vec <- as.vector(t(haircut_mat))
  cost_vec <- as.vector(t(cost_mat))
  quantity_vec <- as.vector(t(quantity_mat))
  minUnitQuantity_vec <- as.vector(t(minUnitQuantity_mat))
  unitValue_vec <- as.vector(t(unitValue_mat))
  minUnit_vec <- as.vector(t(minUnit_mat))
  minUnitValue_vec <- as.vector(t(minUnitValue_mat))
  
  output_list <- list(resource_vec=resource_vec,callId_vec=callId_vec,assetInfo_df=assetInfo_df,callInfo_df=callInfo_df,pref_vec=pref_vec,
                      custodianAccount=custodianAccount,venue=venue,
                      base_mat=base_mat,
                      eli_mat=eli_mat, eli_vec = eli_vec,
                      haircut_mat=haircut_mat, haircut_vec=haircut_vec,
                      cost_mat = cost_mat, cost_vec = cost_vec,
                      quantity_mat=quantity_mat, quantity_vec=quantity_vec,
                      minUnitQuantity_mat=minUnitQuantity_mat,minUnitQuantity_vec=minUnitQuantity_vec,
                      unitValue_mat=unitValue_mat,unitValue_vec=unitValue_vec,
                      minUnit_mat=minUnit_mat, minUnit_vec=minUnit_vec,
                      minUnitValue_mat=minUnitValue_mat,minUnitValue_vec= minUnitValue_vec,
                      callAmount_mat = callAmount_mat
  )
  return (output_list)
}

OrderCallId <- function(callOrderMethod,callInfo_df){
  if(callOrderMethod==0){ # keep original
    callInfo_df <- callInfo_df
  }else if(callOrderMethod==1){ # by call amount, decreasing
    callInfo_df <- callInfo_df[order(callInfo_df$callAmount,decreasing=T),]
  }else if(callOrderMethod==2){ # by margin type(VM first) and call amount, decreasing
    callInfoVM <- callInfo_df[which(toupper(callInfo_df$marginType)=='VARIATION'),]
    callInfoVM <- callInfoVM[order(callInfoVM$callAmount,decreasing=T),]

    callInfoIM <- callInfo_df[which(toupper(callInfo_df$marginType)=='INITIAL'),]
    callInfoIM <- callInfoIM[order(callInfoIM$callAmount,decreasing=T),]
    callInfo_df <- rbind(callInfoVM,callInfoIM)
  }else if(callOrderMethod==3){ # by margin statement, call amount in margin statement, decreasing
    msAggrCall_df <- aggregate(callAmount~marginStatement,data=callInfo_df,sum)
    msAggrCall_df <- msAggrCall_df[order(msAggrCall_df$callAmount,decreasing=T),]
    tempMs_vec <- msAggrCall_df$marginStatement
    newCallInfo_df <- callInfo_df
    idxCurrent <- 0
    for(i in 1:length(tempMs_vec)){
      idxTemp_vec <- which(tempMs_vec[i]==callInfo_df$marginStatement)
      tempCallInfo_df <- callInfo_df[idxTemp_vec,]
      tempCallInfo_df <- tempCallInfo_df[order(tempCallInfo_df$callAmount,decreasing=F),]
      idxNewTemp_vec <- idxCurrent+1:length(idxTemp_vec)
      newCallInfo_df[idxNewTemp_vec,] <- tempCallInfo_df
      
      idxCurrent <- idxCurrent+length(idxTemp_vec)
    }
    callInfo_df<- newCallInfo_df
  }
  return(callInfo_df)
}

SplitCallId <- function(vmLimit,imLimit,callLimit,msLimit,callInfo_df,callId_vec){
  
  groupCallId_list <- list()
  # if the total call numbers is equal or less than limitTotal, only one group
  if(length(callInfo_df[,1])<=limitTotal){
    groupCallId_list[[1]] <- callId_vec
  } else{
    # index of VM and IM in the call list

    idxVm_vec <- which(toupper(callInfo_df$marginType)=='VARIATION')
    idxIm_vec <- which(toupper(callInfo_df$marginType)=='INITIAL')
    # number of VM and IM groups 
    groupVmNum <- ceiling(length(idxVm_vec)/limitVm) 
    groupImNum <- ceiling(length(idxIm_vec)/limitIm)
    
    # make the group list, VM and IM in the same list
    index <- 0
    if(groupVmNum==1){
      index <- index+1
      groupCallId_list[[index]] <- callId_vec[idxVm_vec]
    } else if(groupVmNum > 1){
      for(i in 1:(groupVmNum-1)){
        index <- index+1
        groupCallId_list[[index]] <- callId_vec[idxVm_vec[(i-1)*limitVm+(1:limitVm)]]
      } 
      index <- index+1
      groupCallId_list[[index]] <- callId_vec[tail(idxVm_vec,length(idxVm_vec)-(groupVmNum-1)*limitVm)]
    }
    
    if(groupImNum==1){
      index <- index+1
      groupCallId_list[[index]] <- callId_vec[idxIm_vec]
    } else if(groupImNum > 1){
        for(i in 1:(groupImNum-1)){
          index <- index+1
          groupCallId_list[[index]] <- callId_vec[idxIm_vec[(i-1)*limitIm+(1:limitIm)]]
        } 
        index <- index+1
        groupCallId_list[[index]] <- callId_vec[tail(idxIm_vec,length(idxIm_vec)-(groupImNum-1)*limitIm)]
    }
  }
  return(groupCallId_list)
}

GroupCallIdByMs <- function(callLimit,msLimit,callInfo_df,callId_vec){
  
  groupCallId_list <- list()
  # if the total call numbers is equal or less than limitTotal, only one group
  if(length(callInfo_df[,1])<=callLimit){
    groupCallId_list[[1]] <- callId_vec
  } else if(length(unique(callInfo_df$marginStatement))<=msLimit){
    groupCallId_list[[1]] <- callId_vec
  } else{
    groupMsId_list <- list()
    callMs_vec <- callInfo_df$marginStatement
    ms_vec <- unique(callMs_vec)
    msGroupNum <- ceiling(length(ms_vec)/msLimit)
    
    for(i in 1:(msGroupNum-1)){
      tempCurrent <- msLimit*(i-1)
      tempMs_vec <- ms_vec[(tempCurrent+1):(tempCurrent+msLimit)]
      tempCall_vec <- callInfo_df$id[which((callInfo_df$marginStatement) %in% tempMs_vec)]
      groupMsId_list[[i]]<- tempMs_vec
      groupCallId_list[[i]]<- tempCall_vec
    }
    tempCurrent <- msLimit*(msGroupNum-1)
    tempMs_vec <- na.omit(ms_vec[(tempCurrent+1):(tempCurrent+msLimit)])
    tempCall_vec <- callInfo_df$id[which((callInfo_df$marginStatement) %in% tempMs_vec)]
    groupMsId_list[[msGroupNum]]<- tempMs_vec
    groupCallId_list[[msGroupNum]]<- tempCall_vec
  }
  return(groupCallId_list)
}
