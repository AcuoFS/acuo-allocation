######
# Manual allocation algo
# Triggered after the user drag an asset from collateral widget to an asset in call widget
######

#### Sources Start #########
setwd("E:/ACUO/projects/acuo-allocation/")
source('src/functionsOfDBRequestByExecutingCypher.R')

source("src/allocationFunction.R")
source("src/coreAlgo.R")
source("src/callLpSolve.R")

source('src/generalFunctions.R')
source('src/manualAllocationAssetToAsset.R')

#### Sources END ###########

#### Input Prepare Start ###########
msId_vec <- c("54b37296")
callId_vec = unlist(callIdByMsId(msId_vec))
clientId <- '999'
pref_vec<-c(5.4,3.5)

callInfo_df <- callInfoByCallId(callId_vec); callId_vec <- unique(callInfo_df$id)
availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId) # available asset for the margin call
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(SplitResource(resource_vec,'asset'))
assetInfo_df <- assetInfoByAssetId(assetId_vec)
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)

algoVersion <- 2
msNum <- length(unique(callInfo_df$marginStatement))
operLimitMs_vec <- rep(2,msNum)
operLimit<- sum(operLimitMs_vec)
fungible <- FALSE

#### Get Current Allocation from Algo for testing purposes Start
resultpre <- CallAllocation(algoVersion,scenario=1,callId_vec,resource_vec,
                            callInfo_df,availAsset_df,resource_df,pref_vec,operLimit,operLimitMs_vec,fungible,
                            ifNewAlloc=T,list())
#### Get Current Allocation from Algo for testing purposes END

currentSelection_list <- resultpre$callOutput 

outputColnames <- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Hc','Hfx','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType','marginStatement','marginCall',
                    'CostFactor','Cost')

#### Add the Missing Columns Start #####
for(m in 1:length(callId_vec)){
  #### remove columns to resemble the java input
  ## remove 'NetAmount(USD)' and 'Amount(USD)'
  callId <- callId_vec[m]
  temp_df <- currentSelection_list[[callId]] 
  resourceTemp_vec <- PasteResource(temp_df$Asset,temp_df$CustodianAccount)
  rmIdx_vec <- which(names(temp_df) %in% c('NetAmount(USD)','Amount(USD)','CostFactor','Cost'))
  temp_df <- temp_df[,-rmIdx_vec]
  currentSelection_list[[callId]] <- temp_df
  
  #### add the missing columns 'NetAmount(USD)' and 'Amount(USD)'
  NetAmountUSD_vec <- temp_df$NetAmount/temp_df$FXRate
  AmountUSD_vec <- temp_df$Amount/temp_df$FXRate
  
  # cost: match resource & call, sum the cost
  idxTemp_vec <- which(availAsset_df$callId==callId & availAsset_df$assetCustacId %in% resourceTemp_vec)
  CostFactorOri_vec <- availAsset_df$internalCost+availAsset_df$externalCost+availAsset_df$opptCost-(availAsset_df$interestRate+availAsset_df$yield)
  CostFactor_vec <- CostFactorOri_vec[idxTemp_vec]
  Cost_vec <- CostFactor_vec*AmountUSD_vec
  
  #temp_df$Quantity <- round(temp_df$Quantity,2)
  #temp_df$`NetAmount(USD)` <- round(NetAmountUSD_vec,2)
  #temp_df$`Amount(USD)` <- round(AmountUSD_vec)
  #temp_df$CostFactor <- round(CostFactor_vec)
  #temp_df$Cost <- round(Cost_vec)
  
  temp_df$Quantity <- temp_df$Quantity
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
#### Remove Add the Missing Columns END #######

#### Input Prepare END ################


#### Call Manual Allocation Asset to Asset Start #####
# scenario 1
newAssetId <- 'SGD'
newResourceAmount <- 50
operLimitMc <- 2

# scenario 2
newAssetId <- 'USD'
newResourceAmount <- 50
operLimitMc <- 2

# scenario 3
newAssetId <- 'USD'
newResourceAmount <- 50
operLimitMc <- 1

# scenario 4
# we cannot simulate because the the integer minUnit amount does not match
# especially for different currencies, the calculation of fx rate

# scenario 5
newAssetId <- 'USD'
newResourceAmount <- 768.52
operLimitMc <- 2

#
newAssetCustodianAccount <- "CustodianAccount1D"
newResource <- PasteResource(newAssetId,newAssetCustodianAccount)

replaceAsset <- 'SGD'
replaceCustodianAccount <- "CustodianAccount1D"
replaceResource <- PasteResource(replaceAsset,replaceCustodianAccount)

selectCallId <- callId_vec[1]
selectionForCall <- currentSelection_list[[1]]
oriAssetId_vec <- selectionForCall$Asset
assetIds <- unique(c(newAssetId,oriAssetId_vec))
assetInfo_df <- assetInfoByAssetId(assetIds)

callInfo_df <- callInfoByCallId(selectCallId)

availAsset_df <- availAssetByCallIdAndClientId(selectCallId,clientId)
availAsset_df <- availAsset_df[which(availAsset_df$assetId %in% assetIds),]
# delete the newAssetId that is not held by newAssetCustodianAccount
rmrow <- which(availAsset_df$assetId==newAssetId && availAsset_df$CustodianAccount!=newAssetCustodianAccount)
if(length(rmrow>0)){
  availAsset_df <- availAsset_df[-rmrow,]
}


assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec

resource_vec <- unique(assetCustacId_vec)

resource_df <- ResourceInfo(resource_vec,assetInfo_df,availAsset_df)
availAsset_df <- AvailAsset(availAsset_df)

# make sure the resource_df and availAsset_df are in the same resource order
availAsset_df <- availAsset_df[match(availAsset_df$assetCustacId,resource_df$id),]

result1 <- ManualAllocationAssetToAsset(algoVersion,selectCallId, newResource,newResourceAmount,selectionForCall,
                                     replaceResource,resource_df,callInfo_df,availAsset_df,
                               pref_vec,operLimit,operLimitMc,fungible)


#### Call Manual Allocation Asset to Asset END #######
