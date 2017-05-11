# Secondary allocation algo
# Triggered after the user deselects an asset from a call

#### FUNCTION INPUT FROM JAVA LAYER Start #######################
# 1. callId_vec: margin calls from current selected margin statements
# 2. clientId
# 3. uesr preference: 
#   3.1. objective vector: pref_vec (R treat it as 3 elements, but only use the first two--cost&liquidity)
#   3.2. operation limit: operLimit (R using static 2*number of margin calls)
# 4. dsAssetId: deselected asset
# 5. dsCallId_vec: asset deselct from
# 6. callInfo_df <- callInfoByCallId(callId_vec)
# 7. availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId)
# 8. resource_vec <- unique(PasteResourcce(availAsset_df$assetId,availAsset_df$CustodianAccount))
# 9. assetId_vec <- unique(SplitResource(resource_vec,'asset'))
# 10. assetInfo_df <- assetInfoByAssetId(assetId_vec)
# 11. currentSelection_list: same format as the output from algo, (already exclude the delected asset)
#### FUNCTION INPUT FROM JAVA LAYER END ##########################

#### Sources Start #########
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')
source('src/coreAlgoTest.R')
source('src/callLpSolve.R')
source('src/generalFunctionsTest.R')
source('src/secondAllocationFunction.R')
source('src/secondAlgo.R')
#### Sources END ###########

#### Input Prepare Start ###########

callId_vec <- c('mcp50','mcp46','mcp38','mcp34')
callId_vec <- c("41e029b2")
clientId <- '999'
pref_vec<-c(10,0,0)
#### deselct the asset from all custodian accounts? Currently yes. Location 'loc1'
dsAssetId <- 'GBP'
#dsCallId_vec <- c('mcp50','mcp46','mcp38','mcp34')
dsCallId_vec <- c("mcp50")

callInfo_df <- callInfoByCallId(callId_vec); callId_vec <- unique(callInfo_df$id)
availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId) # available asset for the margin call
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(SplitResource(resource_vec,'asset'))
assetInfo_df <- assetInfoByAssetId(assetId_vec)
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

algoVersion <- 2
operLimitMs <- 2
operLimit <- operLimitMs*length(unique(callInfo_df$marginStatement))
fungible <- FALSE

#### Get Current Allocation from Algo for testing purposes Start
resultpre <- CallAllocation(algoVersion,scenario=1,callId_vec,resource_vec,
                         callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit,operLimitMs,fungible)
#### Get Current Allocation from Algo for testing purposes END

currentSelection_list <- resultpre$callOutput  

outputColnames <- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType','marginStatement','marginCall')

#### Remove Some Columns for Testing Start ####
for(m in 1:length(callId_vec)){
  #### remove columns to resemble the java input
  ## remove 'NetAmount(USD)' and 'Amount(USD)'
  callId <- callId_vec[m]
  temp_df <- currentSelection_list[[callId]] 
  rmIdx_vec <- which(names(temp_df) %in% c('NetAmount(USD)','Amount(USD)'))
  temp_df <- temp_df[,-rmIdx_vec]
  currentSelection_list[[callId]] <- temp_df
  
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
#### Remove Some Columns for Testing END ######


#### Input Prepare END ##############

#### Call Second Level Algo Start ###
result <- CallSecondAllocation(algoVersion,callId_vec, resource_vec,callInfo_df,availAsset_df,assetInfo_df,
                     dsAssetId,dsCallId_vec,currentSelection_list,
                     pref_vec,operLimit,operLimitMs,fungible)
result

#### Call Second Level Algo END #####
