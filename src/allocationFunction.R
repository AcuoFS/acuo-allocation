library('RNeo4j')
library('lpSolve')
allocationAlgo <- function(callId='mc1',clientId='c1',pref=c(0,0,1,0)){
  
########### Load model input from modelInput.R ##############
  source('src/allocationInputData.R')
  
  input.list <- allocationInputData(callId,clientId)
  
  assetId <- input.list$assetId
  assetInfo <- input.list$assetInfo
  assetInfo <- assetInfo[match(assetId,assetInfo$id),]
  
  call.num <- length(callId)
  asset.num <- length(assetId)
  
  eli.mat <- input.list$eli.mat; eli.vec <- input.list$eli.vec
  haircut.mat<-input.list$haircut.mat; haircut.vec <- input.list$haircut.vec
  quantity.mat<- input.list$quantity.mat; quantity.vec <- input.list$quantity.vec
  value.mat<-input.list$value.mat; value.vec <- input.list$value.vec
  call.mat <- input.list$call.mat;
  cost.percent.mat <- input.list$cost.mat; cost.vec <- input.list$cost.vec

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
  
  ######### CHECK WHETHER ASSET POOL IS SUFFICIENT #############
  suffPerCall <- all(apply(eli.mat*(quantity.mat*value.mat*(1-haircut.mat)),1,sum) > call.mat[,1])
  suffAllCall <- sum(quantity.mat[1,]*value.mat[1,]*(1-apply(haircut.mat,2,max)))>sum(call.mat[,1])
  if(!(suffPerCall&suffAllCall)){
    errorMsg <- 'Asset inventoty is not sufficient!'
    return(errorMsg)
  }
  
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
  leastCost.suff.qty <- call.mat/(1-haircut.mat)/value.mat # quantity needed for a single asset to fulfill each call
  
  select.temp.unique <- unique(leastCostAsset[,2]) ; 
  suff.select.unique <- rep(0,length(select.temp.unique))
  for(i in 1:length(select.temp.unique)){
    id <- select.temp.unique[i]
    idx.temp <- leastCostAsset[which(leastCostAsset[,2]==id),1] # calls have the least cost assetId=id
    suff.select.unique[i] <- 1*(sum(leastCost.suff.qty[idx.temp,id]) < quantity.mat[1,id])
  }

  #### In case of OW-171, least cost assets are sufficient ########
  if(!is.element(0,suff.select.unique)){ 
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
  
  ##### In case of OW-174, all assets have quantity limits #######
  else if(1){ 
    ###### USE THE LIBRARY 'lpSolve' #######################
    # variables: x[1:(call.num*asset.num)] qunatity used of each asset for each margin call
    # objective function: minimize  x*value/(1-haircut)*cost
    # constraints: 
    # 0. quantity used of an asset should be a non-negative value
    #    quantity used >= 0
    # 1. quantity limit of each asset for one margin call (call.num*asset.num)
    #    quantity used <= quantity limit; 0 for non-eligible
    # 2. quantity limit of each asset for all margin calls(asset.num)
    # total quantity used <= total quantity (for an asset)
    # 3. margin call requirement (call.num)
    # total net amount of assets for one margin call >= call amount
    
    idx.eli <- which(eli.vec==1)
    var.num <- length(idx.eli)
    
  #  old.var.num <- call.num*asset.num
    
    f.con.0 <- matrix(0,nrow=var.num,ncol=var.num)
    f.con.0[cbind(1:var.num,1:var.num)] <- 1
    f.dir.0 <- rep('>=',var.num)
    f.rhs.0 <- rep(0,var.num)
    
    f.con.1 <- matrix(0,nrow=var.num,ncol=var.num)
    f.con.1[cbind(1:var.num,1:var.num)] <- 1
    f.dir.1 <- rep('<=',var.num)
  #  old.f.dir.1[which(eli.vec==0)] <- '='
    f.rhs.1 <- eli.vec[idx.eli]*quantity.vec[idx.eli]
    
    f.con.2 <- matrix(0,nrow=asset.num,ncol=var.num)
    temp1 <- 1+(0:(call.num-1))*asset.num
    idx.con.2 <- rep(temp1,asset.num)+rep(c(0:(asset.num-1)),rep(call.num,asset.num))
    idx.con.2 <- match(idx.con.2,idx.eli)
  #  old.f.con.2[cbind(rep(c(1:asset.num),rep(call.num,asset.num)),idx.con.2)]<-1
    f.con.2[na.omit(cbind(rep(c(1:asset.num),rep(call.num,asset.num)),idx.con.2))]<-1
    f.dir.2 <- rep('<=',asset.num)
    f.rhs.2 <- quantity.mat[1,]
    
    f.con.3 <- matrix(0,nrow=call.num,ncol=var.num)
    idx.con.3 <- 1:(asset.num*call.num)
    idx.con.3 <- match(idx.con.3,idx.eli)
  #  old.f.con.3[cbind(rep(c(1:call.num),rep(asset.num,call.num)),idx.con.3)]<- value.vec
    f.con.3[na.omit(cbind(rep(c(1:call.num),rep(asset.num,call.num)),idx.con.3))] <- value.vec[idx.eli]*(1-haircut.vec[idx.eli])
    f.dir.3 <- rep('=',call.num)
    f.rhs.3 <- call.mat[,1]
    
    f.obj <-  value.vec[idx.eli]/(1-haircut.vec[idx.eli])*cost.vec[idx.eli]
    f.con <- rbind(f.con.0,f.con.1,f.con.2,f.con.3)
    f.dir <- c(f.dir.0,f.dir.1,f.dir.2,f.dir.3)
    f.rhs <- c(f.rhs.0,f.rhs.1,f.rhs.2,f.rhs.3)
    
 #   f.int.vec <- c(1:var.num)
    
    temp <- lp('min', f.obj, f.con, f.dir, f.rhs) #,int.vec=f.int.vec)
    result.mat <- matrix(0,nrow=call.num,ncol=asset.num,dimnames=list(callId,assetId))
    result.mat <- t(result.mat)
    result.mat[idx.eli]<-temp$solution
    result.mat <- t(result.mat)
    
    ##### CHECK ALLOCATION RESULT #############################
    # 1. whether all variables are non-negative
    # If negative, change to 0.
    
    result.mat[which(result.mat<0)] <- 0
    
    for(i in 1:call.num){
      select.asset.idx <- which(result.mat[i,]!=0)
      
      select.asset.id <- assetId[select.asset.idx]
      
    }
    
    
    for(i in 1:call.num){
      select.asset.idx <- which(result.mat[i,]!=0)
      select.asset.name <- assetInfo$name[select.asset.idx]
      select.asset.quantity <- result.mat[i,select.asset.idx]
      select.asset.value <- value.mat[i,select.asset.idx]
      select.asset.Amount <- select.asset.quantity*select.asset.value
      select.asset.NetAmount <- select.asset.Amount*(1-haircut.mat[i,select.asset.idx])
      select.asset.df <- data.frame(assetId[select.asset.idx],select.asset.name,select.asset.NetAmount,select.asset.Amount,select.asset.quantity)
      colnames(select.asset.df)<- c('Asset','Name','NetAmount','Amount','quantity')
      
      select.list[[callId[i]]] <- select.asset.df       
    }
    
    output.list <- select.list
  }
}
  
  return(list(input.list,output.list))
}



