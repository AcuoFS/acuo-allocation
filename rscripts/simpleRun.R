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
CallAllocation <- function(algoVersion,callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit){
  inputLimit_vec <- c(7,7,7,5); 
  timeLimit=10; 
  callOrderMethod=3
  minMoveValue<- 1000;
  result <- AllocationAlgo(callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit,
                           algoVersion,minMoveValue,timeLimit,inputLimit_vec,callOrderMethod)
  return(result)
}


## CALL THE ALLOCATION FUNCTION ###########
algoVersion <- 1
operLimit<- 10; 

pref_vec <- c(10,0,0)

result <- CallAllocation(algoVersion,callId_vec,resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit)

callOutput <- result$callOutput; 
col_vec <- names(result$callOutput[[1]])
callNum <- length(callId_vec)
rowNum_vec <- rep(0,callNum)
for(i in 1:callNum){
  rowNum_vec[i] <- length(callOutput[[i]][,1])
}
row_vec <- rep(callId_vec,rowNum_vec)
callOutput_mat <- matrix(0,nrow=length(row_vec),ncol=length(col_vec),dimnames = list(row_vec,col_vec))
count <- 0

for(i in 1:callNum){
  item_df <- callOutput[[i]]
  itemNum <- length(item_df[,1])
  count <- count+itemNum
  callOutput_mat[(count-itemNum+1):count,] <- as.matrix(item_df)
}


print(callOutput_mat)
checkCall_mat <-result$checkCall_mat; print(checkCall_mat)
solverObjValue <- result$solverObjValue; print(solverObjValue)
availAssetResult_df <- result$availAsset_df; print(availAssetResult_df)

result <- result
