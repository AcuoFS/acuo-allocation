options(stringsAsFactors = FALSE)

callInfo_df <- callInfoByCallId
callInfo_df$callAmount <- abs(as.numeric(callInfo_df$callAmount)) # make sure the callAmount is non-negative
#print(callInfo_df)

availAsset_df <- availAssetByCallIdAndClientId
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

### add USD amount hard code
availAsset_df$quantity[which(availAsset_df$assetId=='USD')] <- 1e10
availAsset_df$quantity[which(availAsset_df$quantity<0)] <- 0
###

###### 3 lines added fot testing purposes, comment them after tests ##################
# changing the asset quantity and adding new custodian account to make the optimal assets insufficient,
# so that the allocation function will call the lpSolver.
# availAsset_df$quantity <- availAsset_df$quantity/5
# availAsset_df <- rbind(availAsset_df,availAsset_df)
# availAsset_df$CustodianAccount[1:length(availAsset_df[,1])/2] <- 'custodianAccountTest'
######## end #############################################################################

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(availAsset_df$assetId)
assetInfo_df <- assetInfoByAssetId
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

###### Manually add FXRate and venue for DEMO purposes ########
# FX rate: value = 1 USD can change how much foreign currency
ccy_vec <- c('USD', 'EUR', 'GBP', 'SGD', 'JPY', 'HKD', 'AUD', 'CNY', 'KRW', 'CAD', 'NOK', 'SEK', 'NZD')
FXRate_vec <- c(1,0.92,0.80,1.39,110.6,7.77,1.32,6.86,1112,1.34,8.68,9.02,1.42)
for(k in 1:length(assetInfo_df[,1])){
  idxTemp <- which(assetInfo_df$currency[k] == ccy_vec)
  assetInfo_df$FXRate[k] <- FXRate_vec[idxTemp]
}

# venue: all SG
venue_vec <- rep('SG',length(availAsset_df[,1]))
availAsset_df$venue <- venue_vec
#print(availAsset_df)
###### END ####################################

callId_vec <- callIds
pref_vec <- pref

#print('callId_vec'); print(callId_vec)
## CALL THE ALLOCATION FUNCTION ###########
algoVersion <- 2
operLimitMs <- 2
operLimit<- operLimitMs*length(unique(callInfo_df$marginStatement))
fungible <- FALSE

if(length(callId_vec)==0){
  stop('Empty margin call ids!')
}

if(length(resource_vec)==0){
  stop('Empty eligible assets!')
}

if(length(callInfo_df)==0){
  stop('Empty callInfo_df input!')
}

if(length(availAsset_df)==0){
  stop('Empty availAsset_df input!')
}
if(length(assetInfo_df)==0){
  stop('Empty assetInfo_df input!')
}
#params <- c(algoVersion,callId_vec,pref_vec,operLimit,operLimitMs,fungible)

#stop(paste('params:',params))
print('assetInfo_df:')
print(assetInfo_df)
result <- CallAllocation(algoVersion,scenario=1,callId_vec,resource_vec,
                         callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit,operLimitMs,fungible)

result1 <- result
if(length(result$callOutput)==0){
  stop('empty output!')
  print('empty callOutput: ')
  selectAsset_df <- data.frame(matrix(1:15,nrow=1))
  colnames(selectAsset_df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType','marginStatement','marginCall')
  
  output[['mcp1']] <- selectAsset_df
  result$callOutput <- output
  result1 <- result
}

result <- result1
print(result)

