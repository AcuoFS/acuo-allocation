options(stringsAsFactors = FALSE)

#### Input Prepare Start ###########
callId_vec <- callIds
pref_vec <- pref
algoVersion <- 2
dsAssetId <- assetId
dsCallId_vec <- dsCallIds

print("selections");print(typeof(selections),'\n'); print(length(selections))
print(selections)



#### Convert the Java input to R input
ResultDf2List <- function(result_df,callId_vec){
  callNum <- length(callId_vec)
  result_list <- list()
  for(i in 1:callNum){
    callId <- callId_vec[i]
    idx_vec <- which(result_df$marginCall==callId)
    call_df <- result_df[idx_vec,]
    result_list[[callId]] <- call_df
  }
  return(result_list)
}
print('callId_vec');print(callId_vec)
currentSelection_list <- ResultDf2List(selections,callId_vec)

print("currentSelection_list");print(typeof(currentSelection_list),'\n'); print(length(currentSelection_list))
print(currentSelection_list)

callInfo_df <- callInfoByCallId
callInfo_df$callAmount <- abs(as.numeric(callInfo_df$callAmount)) # make sure the callAmount is non-negative
#print(callInfo_df)

availAsset_df <- availAssetByCallIdAndClientId # available asset for the margin call
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(SplitResource(resource_vec,'asset'))
assetInfo_df <- assetInfoByAssetId
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

operLimitMs <- 3
operLimit <- operLimitMs*length(unique(callInfo_df$marginStatement))
#### Input Prepare END ##############

#### Correct Order for One Margin Call Allocation
outputColnames <- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType','marginStatement','marginCall')

#### Fill in the Missing Columns from Java Start ####
for(m in 1:length(callId_vec)){
  
  callId <- callId_vec[m]
  temp_df <- currentSelection_list[[callId]] 
  
  #### add the missing columns 'NetAmount(USD)' and 'Amount(USD)'
  NetAmountUSD_vec <- temp_df$NetAmount/temp_df$FXRate
  AmountUSD_vec <- temp_df$Amount/temp_df$FXRate
  temp_df$`NetAmount(USD)` <- NetAmountUSD_vec
  temp_df$`Amount(USD)` <- AmountUSD_vec
  currentSelection_list[[callId]] <- temp_df
  
  #### sort the columns into the dedault order defined in R
  newOrder_vec <- match(outputColnames,names(temp_df))
  temp_df <- temp_df[,newOrder_vec]
  currentSelection_list[[callId]] <- temp_df
  
  #### delete rownames
  rownames(temp_df) <- 1:length(temp_df[,1])
  currentSelection_list[[callId]] <- temp_df
}


print("currentSelection_list standard format");print(typeof(currentSelection_list),'\n'); print(length(currentSelection_list))
print(currentSelection_list)

#### Fill in the Missing Columns from Java END ######


#### Call Second Level Algo Start ###
result <- callSecondAllocation(algoVersion,callId_vec, resource_vec,callInfo_df,availAsset_df,assetInfo_df,
                               dsAssetId,dsCallId_vec,currentSelection_list,
                               pref_vec,operLimit,operLimitMs)
result

#### Call Second Level Algo END #####