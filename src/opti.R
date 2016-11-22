library("RNeo4j")

allocation <- function(callId='mc1',order='assetId',pref=c(0,0,1,0)){
  
########### Load model input from modelInput.R ##############
  source('src/modelInput.R')
  
  input.list <- modelInput(callId,order)
  
  assetId <- input.list$assetId
  call.num <- length(callId)
  asset.num <- length(assetId)
  
  ele.mat <- input.list$ele.mat
  haircut.mat<-input.list$haircut.mat
  quantity.mat<- input.list$quantity.mat
  value.mat<-input.list$value.mat
  call.mat <- input.list$call.mat
  cost.percent.mat <- input.list$cost.mat

############### Output Format ###########################
  output.list <- list()

############### Asset Sufficientcy #########################
# suff.mat.1: mat[2,1]=1--asset 1 sufficient for margin call 2
# suff.mat.2: mat[1,3]=1--asset 3 sufficient for all margin calls(available)
# two ways to compute sufficientcy
# 1. Amount equivalent if use up an asset > call amount
# 2. quantity needed of an asset < quantity of asset
# prefer 2, it can deal with the sum sufficiency
  suff.mat.1 <- ele.mat*(call.mat/(1-haircut.mat)/value.mat < quantity.mat)
  suff.mat.2 <- 1*(apply(ele.mat*call.mat/(1-haircut.mat)/value.mat,2,sum) < quantity.mat[1,])

############# Algorithm ####################################
  # In case of OW-171,173,174, pref=(0,0,1,0)
  if(all(pref==c(0,0,1,0))){
    
    # In case of OW-170, all assets are sufficient
    if(!is.element(0,suff.mat.2)){
      cost.mat<-call.mat/(1-haircut.mat)*cost.percent.mat
      reserve.list <-list()
      select.list  <-list()
      for (i in 1:call.num){
        idx1 <- which(ele.mat[i,]!=0)  # return elegible asset idx for mc[i]
        temp <- rbind(cost.mat[i,idx1],idx1,deparse.level = 0)
        if(length(temp[1,])==1){
          sortCost=temp
        }else{
          sortCost<-temp[,order(temp[1,])] # sort the cost, return the cost and asset idx in matrix
        }
        reserve.list[[callId[i]]]<- assetId[sortCost[2,]] # 
        
        select.asset.idx <- sortCost[2,1]
        select.asset.NetAmount <- call.mat[i,1]
        select.asset.Amount <- select.asset.NetAmount/(1-haircut.mat[i,select.asset.idx])
        select.asset.df <- data.frame(assetId[select.asset.idx],select.asset.NetAmount,select.asset.Amount)
          colnames(select.asset.df)<- c('Asset','NetAmount','Amount')
        
        select.list[[callId[i]]] <- select.asset.df       
      }
      
    }
      
  }
  
  output.list<- select.list
  return(output.list)
}



