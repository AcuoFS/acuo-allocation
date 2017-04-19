# Secondary allocation algo
# Triggered after the user deselects an asset from a call

#### FUNCTION INPUT FROM JAVA LAYER Start #######################
# 1. callId_vec
# 2. clientId
# 3. uesr preference: 
#   3.1. objective vector: pref_vec
#   3.2. operation limit: operLimit
# 4. dsAssetId
# 5. dsCallId_vec: asset deselct from
# 6. callInfo_df <- callInfoByCallId(callId_vec)
# 7. availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId)
# 8. resource_vec <- unique(PasteResourcce(availAsset_df$assetId,availAsset_df$CustodianAccount))
# 9. assetId_vec <- SplitResource(resource_vec,'asset')
# 10. assetInfo_df <- assetInfoByAssetId(assetId_vec)
# 11. currentSelection_list: same format as the output from algo, (already exclude the delected asset)
#### FUNCTION INPUT FROM JAVA LAYER END ##########################

#### Sources Start #########
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')
source('src/coreAlgo.R')
source('src/callLpSolve.R')
source('src/generalFunctions.R')
source('src/secondAllocationFunction.R')
#### Sources END ###########

#### Input Prepare Start ###########

callId_vec <- c('mcp47','mcp46','mcp42','mcp35','mcp34')
clientId <- '999'
pref_vec<-c(2,3,5)
operLimit <- 12
algoVersion <- 2
#### deselct the asset from all custodian accounts? Currently yes. Location 'loc1'
dsAssetId <- 'GBP'
dsCallId_vec <- 'mcp34'

callInfo_df <- callInfoByCallId(callId_vec); callId_vec <- unique(callInfo_df$id)
availAsset_df <- availAssetByCallIdAndClientId(callId_vec,clientId) # available asset for the margin call
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

assetCustacId_vec <- PasteResource(availAsset_df$assetId,availAsset_df$CustodianAccount)
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- SplitResource(resource_vec,'asset')
assetInfo_df <- assetInfoByAssetId(assetId_vec)
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

#### Get Current Allocation from Algo for testing purposes Start
result <- CallAllocation(algoVersion,scenario=1,callId_vec,resource_vec,
                         callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit)
#### Get Current Allocation from Algo for testing purposes END

currentSelection_list <- result$callOutput  
for(i in 1:length(dsCallId_vec)){
  dsCallId <- dsCallId_vec[i]
  idxTemp <- which(currentSelection_list[[dsCallId]]$Asset==dsAssetId) #### 'loc1'
  currentSelection_list[[dsCallId]] <- currentSelection_list[[dsCallId]][-idxTemp,]
}


#callInfo_df <- callInfoByCallId(dsCallId)
#msIds <- callInfo_df$marginStatement
#### Input Prepare END ##############

#### Main Function Start ############
callSecondAllocation <- function(algoVersion,callId_vec, resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit,
                                 dsAssetId,dsCallId_vec, currentSelection_list){
  callIdTotal_vec <- callId_vec
  callInfoTotal_df <- callInfo_df
  resourceTotal_vec <- resource_vec
  availAssetTotal_df <- availAsset_df
  assetInfoTotal_df <- assetInfo_df
  
  if(length(dsCallId_vec)==1){
    dsCallId <- dsCallId_vec

    
    if(algoVersion==1){
      result <- SecondAllocationAlgoV1(callId_vec, resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,
                                        dsAssetId,dsCallId, 
                                        currentSelection_list)
    } else if(algoVersion==2){
      result <- SecondAllocationAlgoV2(callIdTotal_vec,callInfoTotal_df,resourceTotal_vec,availAssetTotal_df,assetInfoTotal_df,
                                       dsAssetId,dsCallId,
                                       pref_vec,operLimit,currentSelection_list)
    }
  } else if(length(dsCallId_vec)>1){
    stop('Cannot handle deselection from multiple margin calls currently!')
  } else{
    stop('Please specify which margin calls the asset is removed from!')
  }
  return(result)
}
#### Main Function END ##############

result <- callSecondAllocation(algoVersion,callId_vec, resource_vec,callInfo_df,availAsset_df,assetInfo_df,pref_vec,operLimit,
                     dsAssetId,dsCallId_vec,
                     currentSelection_list)
result
