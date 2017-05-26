options(stringsAsFactors = FALSE)

if(length(unlist(callIds))==0){
  stop('Empty callIds input!')
}

if(length(unlist(dsCallIds))==0){
  stop('Empty dsCallIds input!')
}

if(length(unlist(assetId))==0){
  stop('Empty assetId input!')
}

if(length(unlist(pref))==0){
  stop('Empty pref input!')
}

if(length(unlist(callInfoByCallId))==0){
  stop('Empty callInfoByCallId input!')
}

if(length(unlist(availAssetByCallIdAndClientId))==0){
  stop('Empty availAssetByCallIdAndClientId input!')
}

if(length(unlist(assetInfoByAssetId))==0){
  stop('Empty assetInfoByAssetId input!')
}

#### Input Prepare Start ###########
callId_vec <- callIds
pref_vec <- pref
dsAssetId <- assetId
dsCallId_vec <- dsCallIds

print("selections")
print(selections)

selectedCallId_vec <- unique(selections$marginCall)

if(all(sort(callId_vec)==sort(selectedCallId_vec))){
  stop('callIds not match the margin calls in selections!')
}

#### Convert the Java input to R input

callId_vec <- unlist(callId_vec)
currentSelection_list <- ResultDf2List(selections,callId_vec)

#print("currentSelection_list");print(typeof(currentSelection_list),'\n'); print(length(currentSelection_list))
#print(currentSelection_list)

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

resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)

algoVersion <- 2
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
fungible <- FALSE
#### Input Prepare END ##############

#### Correct Order for One Margin Call Allocation
outputColnames <- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType','marginStatement','marginCall',
                    'CostFactor','Cost')

#### Fill in the Missing Columns from Java Start ####
for(m in 1:length(callId_vec)){
  #### remove columns to resemble the java input
  ## remove 'NetAmount(USD)' and 'Amount(USD)'
  callId <- callId_vec[m]
  temp_df <- currentSelection_list[[callId]] 
  resourceTemp_vec <- PasteResource(temp_df$Asset,temp_df$CustodianAccount)
  
  #### add the missing columns 'NetAmount(USD)' and 'Amount(USD)'
  NetAmountUSD_vec <- temp_df$NetAmount/temp_df$FXRate
  AmountUSD_vec <- temp_df$Amount/temp_df$FXRate
  
  # cost: match resource & call, sum the cost
  idxTemp_vec <- which(availAsset_df$callId==callId & availAsset_df$assetCustacId %in% resourceTemp_vec)
  CostFactorOri_vec <- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-(availAsset_df$interestRate+availAsset_df$yield)
  CostFactor_vec <- CostFactorOri_vec[idxTemp_vec]
  Cost_vec <- CostFactor_vec*AmountUSD_vec
  
  temp_df$`NetAmount(USD)` <- NetAmountUSD_vec
  temp_df$`Amount(USD)` <- AmountUSD_vec
  temp_df$CostFactor <- CostFactor_vec
  temp_df$Cost <- Cost_vec
  currentSelection_list[[callId]] <- temp_df
  
  #### sort the columns into the dedault order defined in R
  newOrder_vec <- match(outputColnames,names(temp_df))
  temp_df <- temp_df[,newOrder_vec]
  currentSelection_list[[callId]] <- temp_df
  
  #### delete rownames
  rownames(temp_df) <- 1:length(temp_df[,1])
  currentSelection_list[[callId]] <- temp_df
}

#### Fill in the Missing Columns from Java END ######


#### Call Second Level Algo Start ###
result <- CallSecondAllocation(algoVersion,callId_vec, resource_vec,callInfo_df,availAsset_df,resource_df,
                               dsAssetId,dsCallId_vec,currentSelection_list,
                               pref_vec,operLimit,operLimitMs_vec,fungible)
print('new suggestions(present in DF format)')
print(ResultList2Df(result$callOutput,callId_vec))

result <- result
#### Call Second Level Algo END #####