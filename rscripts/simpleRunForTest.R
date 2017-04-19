options(stringsAsFactors = FALSE)

#source("rscripts/allocationFunction.R")
#source("rscripts/coreAlgo.R")
#source("src/callLpSolve.R")

print('line7')

callInfo_df <- callInfoByCallId
callInfo_df$callAmount <- abs(as.numeric(callInfo_df$callAmount)) # make sure the callAmount is non-negative
#callInfo_df$callAmountUSD <- abs(as.numeric(callInfo_df$callAmountUSD)) # make sure the callAmount is non-negative
#print(callInfo_df)

availAsset_df <- availAssetByCallIdAndClientId
availAsset_df <- availAsset_df[order(availAsset_df$callId),]
print('line16')
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
#print(availAsset_df)
###### END ####################################
print('line47')

callId_vec <- callIds
pref_vec <- pref

## main function, interface of java #######
## CALL THE ALLOCATION FUNCTION ###########
algoVersion <- 2
operLimit<- 2*length(callId_vec); 
inputLimit_vec <- c(7,7,7,5); 
timeLimit <- 10; 
callOrderMethod <- 3
minMoveValue <- 1000;


callInfo_df <- OrderCallId(callOrderMethod,callInfo_df)
callId_vec <- callInfo_df$id
msId_vec <- unique(callInfo_df$marginStatement)
#### Order callId_vec END ######################################

#cat('msId_vec',msId_vec)

#### Group the callId_vec Start ################################

inputLimit_vec <- c(7,7,7,5); 
timeLimit=20; 
callOrderMethod=3
minMoveValue<- 1000;
# build scenario into the function
#### Scenario: Algo suggestion: #####
scenario = 1
#result <- AllocationAlgo(callId_vec,resource_vec,resource_vec,
#                           callInfo_df,availAsset_df,availAsset_df,assetInfo_df,assetInfo_df,pref_vec,operLimit,
#                           algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod)
#######################################################################################################

resourceOri_vec <- resource_vec
availAssetOri_df <- availAsset_df
assetInfoOri_df <- assetInfo_df

#### Order callId_vec Start ######################################
## method 0: Keep original
## method 1: By margin call amount, decreasing
## method 2: By margin type, VM then IM; sub order by call amount
## method 3: By total call amount in margin statement, decreasing

if(missing(callOrderMethod)){
  callOrderMethod <-2
}

callInfo_df <- OrderCallId(callOrderMethod,callInfo_df)
callId_vec <- callInfo_df$id
msId_vec <- unique(callInfo_df$marginStatement)
#### Order callId_vec END ######################################
print('line101')
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

print('line104')
callNum <- length(callId_vec)

# allocate one group a time
# after each allocation, update the tempQuantity_vec of each asset
callOutput_list <- list()
msOutput_list <- list()
checkCall_mat <- matrix(c(callInfo_df$callAmount,rep(0,callNum)),nrow=callNum, dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))

############ ITERATE THE GROUP, RUN THE ALGO Start #########################

for(i in 1:length(groupCallId_list)){
  print('line126')
  callIdGroup_vec <- groupCallId_list[[i]]
  msIdGroup_vec <- unique(callInfo_df$marginStatement[which(callInfo_df$id %in% callIdGroup_vec)])
  #cat(' group:',i,'\n','callId_vec:',callIdGroup_vec,'\n')
  print('line130')
  callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
  availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
  print('line133')
  resourceGroup_vec <- unique(availAssetGroup_df$assetCustacId)
  assetIdGroup_vec <- matrix(unlist(strsplit(resourceGroup_vec,'-')),nrow=2)[1,]
  assetInfoGroup_df <- assetInfo_df[match(assetIdGroup_vec,assetInfo_df$id),]
  print('line137')
  # input data to the core Algo
  coreInput_list <- AllocationInputData(callIdGroup_vec,resourceGroup_vec,callInfoGroup_df,availAssetGroup_df,assetInfoGroup_df)
  print('line140')
  #### Pre-allocate Start ######################
  availAssetPre_df <- availAssetGroup_df
  callInfoPre_df <- callInfoGroup_df
  assetInfoPre_df <- assetInfoGroup_df
  callOutputPre_list <- callOutput_list # currently, store all the cumulated margin calls
  for(p in 1:length(callIdGroup_vec)){
    # consider to change to margin statement 
    callId <- callIdGroup_vec[p] 
    operLimitMs <- operLimit/length(msIdGroup_vec) + 1 # this limit is supposed to set on ms not mc 
    res <- PreAllocation(algoVersion,callId,callInfoPre_df,availAssetPre_df,assetInfoPre_df,pref_vec,operLimitMs,minMoveValue,timeLimit,callOutput_list,checkCall_mat)
    availAssetPre_df <- res$availAsset_df
    #availAssetPre_df[which(availAssetPre_df$callId %in% callId),] <- availAssetPreGroup_df
    callOutputPreGroup_list <- res$callOutput_list
    resultPre_list <- res$resultGroup_list
    checkCallPre_mat <- res$checkCall_mat
    callOutputPre_list[[callId]] <- callOutputPreGroup_list[[callId]]
  }
  print('line158')
  # parameteres need to pass to the CoreAlgoV2
  # callOutputPre_list
  # availAssetPre_df # don't need, solver will auto deduct the quantity while solving
  # combine into a single list: preAllocation_list
  
  initAllocation_list <- callOutputPre_list # currently, store all the cumulated margin calls
  #### Pre-allocate End ########################
  print('line166')
  #### Run CoreAlgo Start ######################
  if(algoVersion==1){
    resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue)#,initAllocation_list)
  } else if(algoVersion==2){print('line170')
    resultGroup_list <- CoreAlgoV2(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,operLimit,minMoveValue,initAllocation_list)
  }
  #### Run CoreAlgo END ########################
  print('line174')
  msOutputGroup_list <- resultGroup_list$msOutput_list
  callOutputGroup_list <- resultGroup_list$callOutput_list
  
  status <- resultGroup_list$status
  lpsolveRun <- resultGroup_list$lpsolveRun
  solverObjValue <- resultGroup_list$solverObjValue
  checkCallGroup_mat <- resultGroup_list$checkCall_mat
  
  # update the availAsset 
  # consider to move the update of available asset from CoreAlgo to outside
  # based on the result list
  availAssetGroup_df <- resultGroup_list$availAsset_df
  availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),] <- availAssetGroup_df
  
  for(k in 1:length(callIdGroup_vec)){
    callId <- callIdGroup_vec[k]
    j <- which(msIdGroup_vec==callInfo_df$marginStatement[which(callInfo_df$id==callId)])
    
    msId <- msId_vec[j]
    callOutput_list[[callId]] <- callOutputGroup_list[[callId]]
    #msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
    checkCall_mat[which(rownames(checkCall_mat)==callId),2] <- checkCallGroup_mat[which(rownames(checkCallGroup_mat)==callId),2]
  }
}


#### Result Analysis Output Start #####################
coreInput_list <- AllocationInputData(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df)
eli_vec <- coreInput_list$eli_vec; idxEli_vec <- which(eli_vec==1)
varInfo_list <- VarInfo(eli_vec,callInfo_df,resource_vec,callId_vec)
varName_vec <- varInfo_list$varName_vec; varNum <- varInfo_list$varNum
varAmount_vec <- callList2AmountVec(callOutput_list,callId_vec,varName_vec[1:varNum])
#### Costs
dailyCost <- CostFun(varAmount_vec,coreInput_list$cost_vec[idxEli_vec])
monthlyCost <- dailyCost*30
dailyCost <- round(dailyCost,2)
monthlyCost <- round(monthlyCost,2)

#### Movements
varAmount_mat <- VarVec2mat(varAmount_vec[1:varNum],varName_vec[1:varNum],callId_vec,resource_vec)
movements <- OperationFun(varAmount_mat,callInfo_df,'matrix')

#### Liquidity
coreInputOri_list <- AllocationInputData(callId_vec,resourceOri_vec,callInfo_df,availAssetOri_df,assetInfoOri_df)
quantityTotal_mat <- coreInputOri_list$minUnitQuantity_mat;
resourceTotal_vec <- coreInputOri_list$resource_vec

if(callNum==1){
  quantityTotal_vec <- quantityTotal_mat
} else{
  quantityTotal_vec <- apply(quantityTotal_mat,2,max)
}

coreInput_list <- AllocationInputData(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df)
quantityRes_mat <- coreInput_list$minUnitQuantity_mat
quantityRes_vec <- quantityTotal_vec

idxTemp_vec <-match(resource_vec,resourceTotal_vec)
if(callNum==1){
  quantityRes_vec[idxTemp_vec] <- quantityRes_mat
} else{
  quantityRes_vec[idxTemp_vec] <- apply(quantityRes_mat,2,max)
}
#cat('quantityTotal_vec',quantityTotal_vec,'\n')
#cat('quantityRes_vec',quantityRes_vec,'\n')
#print('coreInputOri_list$haircut_mat: '); print(coreInputOri_list$haircut_mat)

liquidity_vec <- apply((1-coreInputOri_list$haircut_mat)^2,2,min)
minUnitValue_vec <- apply(coreInputOri_list$minUnitValue_mat,2,max)

#cat('liquidity_vec',liquidity_vec,'\n')
#cat('minUnitValue_vec',minUnitValue_vec,'\n')
reservedLiquidityRatio <- LiquidFun(quantityRes_vec,quantityTotal_vec,liquidity_vec,minUnitValue_vec)

resultAnalysis <- list(dailyCost=dailyCost,monthlyCost=monthlyCost,movements=movements,reservedLiquidityRatio=reservedLiquidityRatio)
#### Result Analysis Output END #########################



############ ITERATE THE GROUP, RUN THE ALGO END #########################

result <- list(#msOutput=msOutput_list,availAsset_df=availAsset_df,
  callOutput=callOutput_list,checkCall_mat=checkCall_mat,
  status=status,lpsolveRun=lpsolveRun,solverObjValue=solverObjValue,resultAnalysis=resultAnalysis)



