options(stringsAsFactors = FALSE)


callInfo_df <- callInfoByCallId
callInfo_df$callAmount <- abs(as.numeric(callInfo_df$callAmount)) # make sure the callAmount is non-negative
#callInfo_df$callAmountUSD <- abs(as.numeric(callInfo_df$callAmountUSD)) # make sure the callAmount is non-negative
#print(callInfo_df)

availAsset_df <- availAssetByCallIdAndClientId
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

###### 3 lines added fot testing purposes, comment them after tests ##################
# changing the asset quantity and adding new custodian account to make the optimal assets insufficient,
# so that the allocation function will call the lpSolver.
# availAsset_df$quantity <- availAsset_df$quantity/5
# availAsset_df <- rbind(availAsset_df,availAsset_df)
# availAsset_df$CustodianAccount[1:length(availAsset_df[,1])/2] <- 'custodianAccountTest'
######## end #############################################################################

assetCustacId_vec <- paste(availAsset_df$assetId,availAsset_df$CustodianAccount,sep='-')
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(availAsset_df$assetId)
assetInfo_df <- assetInfoByAssetId
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

###### Manually add FXRate and venue for DEMO purposes ########
# FX rate: value = 1 USD can change how much foreign currency
ccy_vec <- c('USD', 'EUR', 'GBP', 'SGD', 'JPY', 'HKD', 'AUD', 'CNY', 'KRW', 'CAD')
FXRate_vec <- c(1,0.92,0.80,1.39,110.6,7.77,1.32,6.86,1112,1.34)
for(k in 1:length(assetInfo_df[,1])){
  idxTemp <- which(assetInfo_df$currency[k] == ccy_vec)
  assetInfo_df$FXRate[k] <- FXRate_vec[idxTemp]
}

# venue: all SG
venue_vec <- rep('SG',length(availAsset_df[,1]))
availAsset_df$venue <- venue_vec
print(availAsset_df)
###### END ####################################


callId_vec <- callIds
pref_vec <- pref

## main function, interface of java #######
## CALL THE ALLOCATION FUNCTION ###########
algoVersion <- 1
operLimit<- 10; 
inputLimit_vec <- c(7,7,7,5); 
timeLimit <- 10; 
callOrderMethod <- 3
minMoveValue <- 1000;
verbose <- 3

callInfo_df <- OrderCallId(callOrderMethod,callInfo_df)
callId_vec <- callInfo_df$id
msId_vec <- unique(callInfo_df$marginStatement)
#### Order callId_vec END ######################################

cat('msId_vec',msId_vec)

#### Group the callId_vec Start ################################
# method 1: group by marginType
# maximum limitVm VM or limitIm IM a time
# group by margin statements
imLimit <- inputLimit_vec[1]
vmLimit <- inputLimit_vec[2]  
callLimit <- inputLimit_vec[3]
msLimit <- inputLimit_vec[4]

groupCallId_list <- GroupCallIdByMs(callLimit,msLimit,callInfo_df,callId_vec)
#### Group the callId_vec Start #################################

print('groupCallId_list');print(groupCallId_list)

callNum <- length(callId_vec)

# allocate one group a time
# after each allocation, update the tempQuantity_vec of each asset
callOutput_list <- list()
#msOutput_list <- list()
checkCall_mat <- matrix(c(callInfo_df$callAmount,rep(0,callNum)),nrow=callNum, dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
costDaily <- 0
costMonthly <- 0
movements <- 0
############ ITERATE THE GROUP, RUN THE ALGO Start #########################

for(i in 1:length(groupCallId_list)){
  
  callIdGroup_vec <- groupCallId_list[[i]]
  msIdGroup_vec <- unique(callInfo_df$marginStatement[which(callInfo_df$id %in% callIdGroup_vec)])
  #cat(' group:',i,'\n','callId_vec:',callIdGroup_vec,'\n')
  
  callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
  availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
  
  resourceGroup_vec <- unique(availAssetGroup_df$assetCustacId)
  assetIdGroup_vec <- as.character(data.frame(strsplit(resourceGroup_vec,'-'))[1,])
  assetInfoGroup_df <- assetInfo_df[match(assetIdGroup_vec,assetInfo_df$id),]
  
  # input data to the core Algo
  coreInput_list <- AllocationInputData(callIdGroup_vec,resourceGroup_vec,callInfoGroup_df,availAssetGroup_df,assetInfoGroup_df)
  
  #### Pre-allocate Start ######################
  availAssetPre_df <- availAssetGroup_df
  callInfoPre_df <- callInfoGroup_df
  assetInfoPre_df <- assetInfoGroup_df
  callOutputPre_list <- callOutput_list
  for(p in 1:length(callIdGroup_vec)){
    callId <- callIdGroup_vec[p]
    res <- PreAllocation(algoVersion,callId,callInfoPre_df,availAssetPre_df,assetInfoPre_df,pref_vec,minMoveValue,verbose,timeLimit,callOutput_list,checkCall_mat)
    availAssetPre_df <- res$availAsset_df
    #availAssetPre_df[which(availAssetPre_df$callId %in% callId),] <- availAssetPreGroup_df
    callOutputPreGroup_list <- res$callOutput_list
    resultPre_list <- res$resultGroup_list
    checkCallPre_mat <- res$checkCall_mat
    callOutputPre_list[[callId]] <- callOutputPreGroup_list[[callId]]
  }
  
  # parameteres need to pass to the CoreAlgoV2
  # callOutputPre_list
  # availAssetPre_df # don't need, solver will auto deduct the quantity while solving
  # combine into a single list: preAllocation_list
  
  initAllocation_list <- callOutputPre_list
  #### Pre-allocate End ########################
  
  print('initAllocation_list');print(initAllocation_list)
  
  #### Run CoreAlgo Start ######################
  if(algoVersion==1){
    resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue,verbose,initAllocation_list)
  } else if(algoVersion==2){
    resultGroup_list <- CoreAlgoV2(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,operLimit,minMoveValue,verbose,initAllocation_list)
  }
  #### Run CoreAlgo END ########################
  
  #msOutputGroup_list <- resultGroup_list$msOutput_list
  callOutputGroup_list <- resultGroup_list$callOutput_list
  
  status <- resultGroup_list$status
  lpsolveRun <- resultGroup_list$lpsolveRun
  solverObjValue <- round(resultGroup_list$solverObjValue,2)
  checkCallGroup_mat <- resultGroup_list$checkCall_mat
  resultAnalysis_list <- resultGroup_list$resultAnalysis_list
  costDaily <- resultAnalysis_list$costDaily + costDaily
  costMonthly <- resultAnalysis_list$costMonthly + costMonthly
  
  # update the availAsset 
  availAssetGroup_df <- resultGroup_list$availAsset_df
  availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),] <- availAssetGroup_df
  
  for(k in 1:length(callIdGroup_vec)){
    callId <- callIdGroup_vec[k]
    j <- which(msIdGroup_vec==callInfo_df$marginStatement[which(callInfo_df$id==callId)])
    
    msId <- msId_vec[j]
    callOutput_list[[callId]] <- callOutputGroup_list[[callId]]
    # msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
    checkCall_mat[which(rownames(checkCall_mat)==callId),2] <- checkCallGroup_mat[which(rownames(checkCallGroup_mat)==callId),2]
  }
}

costDaily <- round(costDaily,2)
costMonthly <- round(costMonthly,2)
cat('costDaily ',costDaily); cat('costMonthly',costMonthly)
resultAnalysis <- list(costDaily=costDaily,costMonthly=costMonthly)
############ ITERATE THE GROUP, RUN THE ALGO END #########################

result <- list(#msOutput=msOutput_list,
  callOutput=callOutput_list,checkCall_mat=checkCall_mat,
  status=status,lpsolveRun=lpsolveRun,solverObjValue=solverObjValue,resultAnalysis=resultAnalysis)

print(result)



