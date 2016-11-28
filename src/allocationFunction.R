library("RNeo4j")

allocationAlgo <- function(callId='mc1',clientId='c1',order='assetId',pref=c(0,0,1,0)){
  
########### Load model input from modelInput.R ##############
  source('src/allocationInputData.R')
  
  input.list <- allocationInputData(callId,clientId)
  
  assetId <- input.list$assetId
  assetInfo <- input.list$assetInfo
  assetInfo <- assetInfo[match(assetId,assetInfo$id),]
  
  call.num <- length(callId)
  asset.num <- length(assetId)
  
  eli.mat <- input.list$eli.mat
  haircut.mat<-input.list$haircut.mat
  quantity.mat<- input.list$quantity.mat
  value.mat<-input.list$value.mat
  call.mat <- input.list$call.mat
  cost.percent.mat <- input.list$cost.mat

############### Output Format ###########################
  output.list <- list()

# A list, each element is the allocation result(dataframe) for one margin call
#------------------------------------------------------------------------------------------
# $callId1
#  Asset(assetId)   Name(assetName)   NetAmount(afterHaircut)   Amount        quantity
# 1  a1              asset1             numeric value         numeric value  numeric value
# 2  a2              asset2             numeric value         numeric value  numeric value
#  
# $callId2
#  Asset(assetId)   Name(assetName)   NetAmount(afterHaircut)   Amount        quantity
# 1  a2              asset2             numeric value         numeric value  numeric value
# 2  a3              asset3             numeric value         numeric value  numeric value
# 3  a4              asset4             numeric value         numeric value  numeric value
#------------------------------------------------------------------------------------------


############# ALGORITHM #######################################
  
if(all(pref==c(0,0,1,0))){  # In case of OW-171,173,174, pref=(0,0,1,0)
  
  ######### SORT ASSET PER CALL BY COST ########################
  cost.mat<-call.mat/(1-haircut.mat)*cost.percent.mat  # cost amount
  
  reserve.list <-list()    # store all available assets for each call, list by callId
  select.list  <-list()    # store selected assets for each call, list by callId
  leastCostAsset <- matrix(c(callId,rep('', call.num)),nrow=call.num,ncol=2,dimnames = list(callId,c('callId','assetId')))
  
  for (i in 1:call.num){
    idx1 <- which(eli.mat[i,]!=0)  # return elegible asset idx for mc[i]
    temp <- rbind(cost.mat[i,idx1],idx1,deparse.level = 0)
    if(length(temp[1,])==1){
      sortCost=temp
    }else{
      sortCost<-temp[,order(temp[1,])] # sort the cost, return the cost and asset idx in matrix
    }
    reserve.list[[callId[i]]]<- assetId[sortCost[2,]] # 
    leastCostAsset[i,2] <- assetId[sortCost[2,]][1]
  }
  
  ############# LEAST COST ASSET SUFFICIENCY #####################
  suff.qty.1 <- call.mat/(1-haircut.mat)/value.mat # quantity needed for a single asset to fulfill each call
  
  select.temp.unique <- unique(leastCostAsset[,2]) ; 
  suff.select.unique <- rep(0,length(select.temp.unique))
  for(i in 1:length(select.temp.unique)){
    id <- select.temp.unique[i]
    idx.temp <- leastCostAsset[which(leastCostAsset[,2]==id),1] # calls have the least cost assetId=id
    suff.select.unique[i] <- 1*(sum(suff.qty.1[idx.temp,id]) < quantity.mat[1,id])
  }
  
  if(!is.element(0,suff.select.unique)){ # In case of OW-171, least cost assets are sufficient
    
    for(i in 1:call.num){
      select.asset.idx <- which(assetInfo$id==reserve.list[[i]][1])
      select.asset.name <- assetInfo$name[select.asset.idx]
      select.asset.NetAmount <- call.mat[i,1]
      select.asset.Amount <- select.asset.NetAmount/(1-haircut.mat[i,select.asset.idx])
      select.asset.quantity <- select.asset.Amount/value.mat[i,select.asset.idx]
      select.asset.df <- data.frame(assetId[select.asset.idx],select.asset.name,select.asset.NetAmount,select.asset.Amount,select.asset.quantity)
        colnames(select.asset.df)<- c('Asset','Name','NetAmount','Amount','quantity')
      
      select.list[[callId[i]]] <- select.asset.df       
    }
    
    output.list<- select.list
  }
  
  else if(1){
    
  }
}
  
  return(output.list)
}



