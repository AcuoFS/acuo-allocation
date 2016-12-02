library('RNeo4j')
library('lpSolveAPI')


allocationAlgo <- function(callId='mc1',clientId='c1',pref=c(0,0,1,0)){
  
########### Load model input from modelInput.R ##############
  source('src/allocationInputData.R')
  
  input.list <- allocationInputData(callId,clientId)
  
  assetId <- input.list$assetId         # all eligible asset ids
  assetInfo <- input.list$assetInfo     # eligible asset information
  assetInfo <- assetInfo[match(assetId,assetInfo$id),]  # sort the assetInfo in the assetId order
  
  call.num <- length(callId)            # total margin call number
  asset.num <- length(assetId)          # total asset number
  
  eli.mat <- input.list$eli.mat; eli.vec <- input.list$eli.vec               # eligibility matrix & vector
  haircut.mat<-input.list$haircut.mat; haircut.vec <- input.list$haircut.vec # haircut mat & vec
  quantity.mat<- input.list$quantity.mat; quantity.vec <- input.list$quantity.vec # asset quantity mat & vec
  value.mat<-input.list$value.mat; value.vec <- input.list$value.vec         # asset value mat & vec
  call.mat <- input.list$call.mat;                                           # margin call amount mat
  cost.percent.mat <- input.list$cost.mat; cost.vec <- input.list$cost.vec   # cost mat & vec
  minUnit.mat <- input.list$minUnit.mat; minUnit.vec <- input.list$minUnit.vec   # minimum denomination mat & vec

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
    errorMsg <- 'Asset inventory is not sufficient!'
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
    idx.eli <- which(eli.vec==1)  # Exclude the non-eligible asset variable for each margin call
    var.num <- length(idx.eli)    # variable numbers
    
    ############# MODEL SETUP ###########################################
    # decision variables: x, qunatity used of each asset for each margin call
    # 
    # objective function: f.obj, minimize  x*value*cost
    # 
    # constraints: A*x (direction) b
    # A-- constraint matrix: f.con;
    # b-- constraint value: f.rhs;
    # direction -- constraint direction: f.dir.
    #
    # Constraints are specified below:
    # 0. quantity used of an asset should be a non-negative value
    #    quantity used >= 0
    # 1. quantity limit of each asset for one margin call (call.num*asset.num)
    #    quantity used <= quantity limit; 0 for non-eligible
    # 2. quantity limit of each asset for all margin calls(asset.num)
    #    total quantity used <= total quantity (for an asset)
    # 3. margin call requirement (call.num)
    #    total net amount of assets for one margin call >= call amount
    #
    # variable bounds: a < x < x_quantity
    #    specified by constraint 0 and 1. 
    # variable kind: semi-continuous, value below 'a' will automately set to 0
    #
    ######
    
    f.con.0 <- matrix(0,nrow=var.num,ncol=var.num)
    f.con.0[cbind(1:var.num,1:var.num)] <- 1
    f.dir.0 <- rep('>=',var.num)
    f.rhs.0 <- rep(0,var.num)
    
    f.con.1 <- matrix(0,nrow=var.num,ncol=var.num)
    f.con.1[cbind(1:var.num,1:var.num)] <- 1
    f.dir.1 <- rep('<=',var.num)
    f.rhs.1 <- eli.vec[idx.eli]*quantity.vec[idx.eli]
    
    f.con.2 <- matrix(0,nrow=asset.num,ncol=var.num)
    temp1 <- 1+(0:(call.num-1))*asset.num
    idx.con.2 <- rep(temp1,asset.num)+rep(c(0:(asset.num-1)),rep(call.num,asset.num))
    idx.con.2 <- match(idx.con.2,idx.eli)
    f.con.2[na.omit(cbind(rep(c(1:asset.num),rep(call.num,asset.num)),idx.con.2))]<-1
    f.dir.2 <- rep('<=',asset.num)
    f.rhs.2 <- quantity.mat[1,]
    
    f.con.3 <- matrix(0,nrow=call.num,ncol=var.num)
    idx.con.3 <- 1:(asset.num*call.num)
    idx.con.3 <- match(idx.con.3,idx.eli)
    f.con.3[na.omit(cbind(rep(c(1:call.num),rep(asset.num,call.num)),idx.con.3))] <- value.vec[idx.eli]*(1-haircut.vec[idx.eli])
    f.dir.3 <- rep('>=',call.num)
    f.rhs.3 <- call.mat[,1]
    
    f.obj <-  value.vec[idx.eli]*cost.vec[idx.eli]
    names(f.obj) <- paste('var',1:var.num)
    

    ###### USE THE PACKAGE 'lpSolveAPI' #############################
    # decision variables: x, qunatity used of each asset for each margin call
    # 
    # objective function: f.obj, minimize  x*value*cost
    # 
    # variable bounds: a < x < x_quantity
    # variable kind: semi-continuous, value below 'a' will automately set to 0
    #
    # constraints: A*x (direction) b
    # A-- constraint matrix: lp.con;
    # b-- constraint value: lp.rhs;
    # direction -- constraint direction: lp.dir.
    #
    # Constraints are specified below:
    # 1. quantity limit of each asset for all margin calls(asset.num)
    #    total quantity used <= total quantity (for an asset)
    # 2. margin call requirement (call.num)
    #    total net amount of assets for one margin call >= call amount
    ######
    
    # constraints
    lp.con <- rbind(f.con.2,f.con.3)
    lp.dir <- c(f.dir.2,f.dir.3)
    lp.rhs <- c(f.rhs.2,f.rhs.3)
    
    lps.model <- make.lp(length(lp.con),var.num)  # make model
    set.objfn(lps.model,f.obj)                    # set objective
    
    for (i in 1:length(lp.con[,1])){              # set constraints
      add.constraint(lps.model,lp.con[i,],lp.dir[i],lp.rhs[i])
    }
    
    idx.int <- sort(na.omit(match(which(minUnit.vec==1),idx.eli)))
    set.type(lps.model,idx.int,type='integer')    # set integer variables
    set.semicont(lps.model,1:var.num,TRUE)        # set semi-continuous variables
    set.bounds(lps.model,lower=rep(10,var.num),upper=quantity.vec[idx.eli])
                                                  # set variables lower/upper bounds
    lp.control(lps.model,epsb=1e-30,epsd=1e-30)   # modify tolerance
    solve(lps.model)                              # solve model
    #get.objective(lps.model) 
    lpSolveAPI.solution <- get.variables(lps.model)
                                                  # get solution
    result.mat <- matrix(0,nrow=call.num,ncol=asset.num,dimnames=list(callId,assetId))
    result.mat <- t(result.mat)
    result.mat[idx.eli]<-lpSolveAPI.solution
    result.mat <- t(result.mat)                   # convert solution into matrix format
    
    ##### CHECK ALLOCATION RESULT #############################
    # 
    # STATUS: UNDEVELOPPED
    #
    # 1. whether all variables are non-negative
    # 2. whether statisfy the quantity limits
    # 3. whether meet all margin call requirements
    #
    ##########################################################

    for(i in 1:call.num){                          # store the result into select list
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
  
  return(list(input=input.list,output=output.list))
}



