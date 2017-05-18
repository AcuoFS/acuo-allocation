OrderCallId <- function(callOrderMethod,callInfo_df){
  ## method 0: Keep original
  ## method 1: By margin call amount, decreasing
  ## method 2: By margin type, VM then IM; sub order by call amount
  ## method 3: By total call amount in margin statement, decreasing
  
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
