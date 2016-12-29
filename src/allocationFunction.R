library('RNeo4j')
library('lpSolveAPI')


allocationAlgo <- function(callId='mc1',clientId='999',pref=c(0,0,1)){
  
########### Load model input from modelInput.R ########################
  source('src/allocationInputData.R')
  
  input.list <- allocationInputData(callId,clientId)
  
  assetId <- input.list$assetId         # all eligible asset ids
  assetInfo <- input.list$assetInfo     # eligible asset information
  assetInfo <- assetInfo[match(assetId,assetInfo$id),]  # sort the assetInfo in the assetId order
  
  callInfo <- input.list$callInfo
  callInfo <- callInfo[match(callId,callInfo$id),]
  custodianAccount <- input.list$custodianAccount  
  venue <- input.list$venue

  call.num <- length(callId)            # total margin call number
  asset.num <- length(assetId)          # total asset number
  
  eli.mat <- input.list$eli.mat; eli.vec <- input.list$eli.vec                    # eligibility matrix & vector
  haircut.mat<-input.list$haircut.mat; haircut.vec <- input.list$haircut.vec      # haircut mat & vec
  quantity.mat<- input.list$quantity.mat; quantity.vec <- input.list$quantity.vec # asset quantity mat & vec
  minUnitQuantity.mat<- input.list$minUnitQuantity.mat; minUnitQuantity.vec <- input.list$minUnitQuantity.ve
  
  unitValue.mat<-input.list$unitValue.mat; unitValue.vec <- input.list$unitValue.vec     # asset unit value mat & vec
  minUnit.mat <- input.list$minUnit.mat; minUnit.vec <- input.list$minUnit.vec;
  minUnitValue.mat <- input.list$minUnitValue.mat; minUnitValue.vec <- input.list$minUnitValue.vec;
  
  amount.mat <- unitValue.mat*quantity.mat; amount.vec <- unitValue.vec*quantity.vec     # amount of asset
  
  call.mat <- input.list$call.mat; call.vec <- as.vector(t(call.mat))             # margin call amount mat
  cost.percent.mat <- input.list$cost.mat; cost.vec <- input.list$cost.vec        # cost mat & vec

############### CONSTANTS DEFINED INSIDE THE ALGO ###################
  minMoveValue <- 1000
  
############### Output Format #######################################
  output.list <- list()

# A list, each element is the allocation result(dataframe) for one margin call
#------------------------------------------------------------------------------------------------------------------------------------------
# $callId1
#  Asset(assetId)   Name(assetName)   NetAmount(USD)(afterHaircut)    Amount      currency    quantity        value       custodianAccount
# 1  a1              asset1               numeric value           numeric value    CCY1    numeric value  numeric value     custac1
# 2  a2              asset2               numeric value           numeric value    CCY2    numeric value  numeric value     custac2
# 
# $callId2
#  Asset(assetId)   Name(assetName)   NetAmount(USD)(afterHaircut)   Amount      currency    quantity        value       custodianAccount
# 1  a2              asset2               numeric value          numeric value     CCY2    numeric value  numeric value     custac2
# 2  a3              asset3               numeric value          numeric value     CCY1    numeric value  numeric value     custac1
# 3  a4              asset4               numeric value          numeric value     CCY3    numeric value  numeric value     custac3
#------------------------------------------------------------------------------------------------------------------------------------------


############# ALGORITHM ############################################

######### CHECK WHETHER ASSET POOL IS SUFFICIENT #############
suffPerCall <- all(apply(eli.mat*(minUnitQuantity.mat*minUnitValue.mat*(1-haircut.mat)),1,sum) > call.mat[,1])
suffAllCall <- sum(minUnitQuantity.mat[1,]*minUnitValue.mat[1,]*(1-apply(haircut.mat,2,max)))>sum(call.mat[,1])
if(!(suffPerCall&suffAllCall)){
  errorMsg <- 'Error: Asset inventory is insufficient!'
  return(errorMsg)
}

#### Recalculate the parameters weight setting
pref <- pref/sum(pref)

#### calculate the optimal asset sufficiency
reserve.list <-list()    # store all available assets for each call, list by callId
select.list  <-list()    # store selected assets for each call, list by callId
optimalAsset <- matrix(c(callId,rep('', call.num)),nrow=call.num,ncol=2,dimnames = list(callId,c('callId','assetId')))

cost.mat<-call.mat/(1-haircut.mat)*cost.percent.mat  # cost amount
asset.liquid <- apply((1-haircut.mat*eli.mat)^2,2,min) # define asset liquidity
liquidity.mat <- matrix(rep(asset.liquid,call.num),nrow=call.num,byrow=TRUE,dimnames=list(callId,assetId)) 
liquidity.vec <- as.vector(t(liquidity.mat))
excess.call.percent <- 0.2
call.mat <- call.mat*(1+excess.call.percent*pref[1])
call.vec <- call.vec*(1+excess.call.percent*pref[1])

call.ccy <- callInfo$currency
# calculate the cost if only the integral units of asset can be allocated
integer.call.mat <- ceiling(call.mat/(1-haircut.mat)/minUnitValue.mat)*minUnitValue.mat*(1-haircut.mat)
cost.mat<-integer.call.mat/(1-haircut.mat)*cost.percent.mat  # cost amount
operation.mat <- matrix(rep(1,asset.num*call.num),nrow=call.num,byrow=TRUE,dimnames=list(callId,assetId)) 

for(i in 1:call.num){
  ccy.idx <- which(call.ccy[i]==assetId)    # return the index of mc[i] currency cash in the assetId list
  idx1 <- which(eli.mat[i,]!=0)             # return elegible asset idx for mc[i]
  if(length(ccy.idx)==1 && is.element(ccy.idx,idx1)){  # if there exist call currency cash in the inventory, and it's available
    operation.mat[i,ccy.idx] <- 0
  }
}
operation.vec <- as.vector(t(operation.mat))

norm.cost <- t(apply(cost.mat,1,scale))
norm.liquidity <- t(apply(liquidity.mat,1,scale))
norm.operation <- operation.mat
for(i in 1:call.num){
  if(all(operation.mat[i,]==1)){
    norm.operation[i,]<-0
  }else{
    norm.operation[i,]<- scale(operation.mat[i,])
  }
}

optimal.mat <- norm.operation*pref[1]+norm.liquidity*pref[2]+norm.cost*pref[3]
colnames(optimal.mat) <- assetId; rownames(optimal.mat)<-callId

for(i in 1:call.num){
  idx1 <- which(eli.mat[i,]!=0)  # return elegible asset idx for mc[i]
  temp <- rbind(optimal.mat[i,idx1],idx1,deparse.level = 0) # combine the asset cost and index together
  # sort the asset per call by cost
  if(length(temp[1,])==1){       # if there's only one eligible asset, no need to sort.
    sortOptimal=temp
  }else{
    sortOptimal<-temp[,order(temp[1,])] # sort the cost, return the cost and asset idx in matrix
  }
  reserve.list[[callId[i]]]<- assetId[sortOptimal[2,]] # 
  optimalAsset[i,2] <- assetId[sortOptimal[2,]][1]
}

optimal.suff.qty <- call.mat/(1-haircut.mat)/minUnitValue.mat # quantity needed for a single asset to fulfill each call

select.temp.unique <- unique(optimalAsset[,2]) ; 
suff.select.unique <- rep(0,length(select.temp.unique))
for(i in 1:length(select.temp.unique)){
  id <- select.temp.unique[i]
  idx.temp <- optimalAsset[which(optimalAsset[,2]==id),1] # calls have the least cost assetId=id
  suff.select.unique[i] <- 1*(sum(optimal.suff.qty[idx.temp,id]) < minUnitQuantity.mat[1,id])
}

#### In case of OW-291, optimal assets are sufficient
if(!is.element(0,suff.select.unique)){ 
  
  for(i in 1:call.num){
    select.asset.idx <- which(assetInfo$id==reserve.list[[i]][1])
    select.asset.id <- assetId[select.asset.idx]
    select.asset.custodianAccount <- custodianAccount[select.asset.idx]
    select.asset.venue <- venue[select.asset.idx]
    select.asset.name <- assetInfo$name[select.asset.idx]
    select.asset.NetAmount <- integer.call.mat[i,1]
    select.asset.haircut <- haircut.mat[i,select.asset.idx]
    select.asset.Amount <- select.asset.NetAmount/(1-haircut.mat[i,select.asset.idx])
    select.asset.currency <- assetInfo$currency[select.asset.idx]
    select.asset.quantity <- select.asset.Amount/unitValue.mat[i,select.asset.idx]
    select.asset.df <- data.frame(select.asset.id,select.asset.name,select.asset.NetAmount,select.asset.haircut,select.asset.Amount,select.asset.currency,
                                  select.asset.quantity,select.asset.custodianAccount,select.asset.venue )
    colnames(select.asset.df)<- c('Asset','Name','NetAmount(USD)','Haircut','Amount','Currency','Quantity','CustodianAccount','Venue')
    
    select.list[[callId[i]]] <- select.asset.df       
  }
  output.list<- select.list
}


else if(all(pref==c(0,0,1))){  # In case of OW-171,173,174, pref=(0,0,1)
  
 
  
  
  ##### In case of OW-174, all assets have quantity limits #######
   
    idx.eli <- which(eli.vec==1)  # Exclude the non-eligible asset variable for each margin call
    var.num <- length(idx.eli)    # variable numbers
    
    ############# MODEL SETUP ###########################################
    # decision variables: x, qunatity used of each asset for each margin call
    #                 quantity or minUnitQuantity
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
    #    quantity used <= quantity limit; (quantity or minUnitQuantity)
    # 2. quantity limit of each asset for all margin calls(asset.num)
    #    total quantity used <= total quantity (for an asset) (quantity or minUnitQuantity)
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
    f.rhs.1 <- eli.vec[idx.eli]*minUnitQuantity.vec[idx.eli]
    
    f.con.2 <- matrix(0,nrow=asset.num,ncol=var.num)
    temp1 <- 1+(0:(call.num-1))*asset.num
    idx.con.2 <- rep(temp1,asset.num)+rep(c(0:(asset.num-1)),rep(call.num,asset.num))
    idx.con.2 <- match(idx.con.2,idx.eli)
    f.con.2[na.omit(cbind(rep(c(1:asset.num),rep(call.num,asset.num)),idx.con.2))]<-1
    f.dir.2 <- rep('<=',asset.num)
    f.rhs.2 <- minUnitQuantity.mat[1,]
    
    f.con.3 <- matrix(0,nrow=call.num,ncol=var.num)
    idx.con.3 <- 1:(asset.num*call.num)
    idx.con.3 <- match(idx.con.3,idx.eli)
    f.con.3[na.omit(cbind(rep(c(1:call.num),rep(asset.num,call.num)),idx.con.3))] <- minUnitValue.vec[idx.eli]*(1-haircut.vec[idx.eli])
    f.dir.3 <- rep('>=',call.num)
    f.rhs.3 <- call.mat[,1]
    
    f.obj <-  minUnitValue.vec[idx.eli]*cost.vec[idx.eli]
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
    
    
  #  idx.int <- 1:var.num
  #  set.type(lps.model,idx.int,type='integer')    # set integer variables
    set.semicont(lps.model,1:var.num,TRUE)        # set semi-continuous variables
    
    minMoveQuantity <- ceiling(minMoveValue/minUnitValue.vec[idx.eli])
    if(length(call.vec[which(minMoveValue > call.vec[idx.eli])])!=0){
      temp.idx <- which(minMoveValue > call.vec[idx.eli])
      call.eli.vec <- call.vec[idx.eli]
      minUnitValue.eli.vec <- minUnitValue.vec[idx.eli]
      minMoveQuantity[temp.idx] <- ceiling(call.eli.vec[temp.idx]/minUnitValue.eli.vec[temp.idx])
    }
    set.bounds(lps.model,lower=minMoveQuantity,upper=minUnitQuantity.vec[idx.eli])
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
      select.asset.id <- assetId[select.asset.idx]
      select.asset.custodianAccount <- custodianAccount[select.asset.idx]
      select.asset.venue <- venue[select.asset.idx]
      select.asset.name <- assetInfo$name[select.asset.idx]
      select.asset.haircut <- haircut.mat[i,select.asset.idx]
      select.asset.currency <- assetInfo$currency[select.asset.idx]
      select.asset.quantity <- result.mat[i,select.asset.idx]*minUnit.mat[i,select.asset.idx]
      select.asset.unitValue <- unitValue.mat[i,select.asset.idx]
      select.asset.Amount <- select.asset.quantity*select.asset.unitValue
      select.asset.NetAmount <- select.asset.Amount*(1-haircut.mat[i,select.asset.idx])

      select.asset.df <- data.frame(select.asset.id,select.asset.name,select.asset.NetAmount,select.asset.haircut,select.asset.Amount,select.asset.currency,
                                    select.asset.quantity,select.asset.custodianAccount,select.asset.venue)
      colnames(select.asset.df)<- c('Asset','Name','NetAmount(USD)','Haircut','Amount','Currency','Quantity','CustodianAccount','venue')
      rownames(select.asset.df)<- paste('Asset',c(1:length(select.asset.id)),sep='')
      
      select.list[[callId[i]]] <- select.asset.df       
    }
    output.list <- select.list
  
}

else if(all(pref==c(0,1,0))){
  
 
  ##### In case of OW-250, all assets have quantity limits ##########
  
    idx.eli <- which(eli.vec==1)  # Exclude the non-eligible asset variable for each margin call
    var.num <- length(idx.eli)    # variable numbers
   
    f.con.0 <- matrix(0,nrow=var.num,ncol=var.num)
    f.con.0[cbind(1:var.num,1:var.num)] <- 1
    f.dir.0 <- rep('>=',var.num)
    f.rhs.0 <- rep(0,var.num)
    
    f.con.1 <- matrix(0,nrow=var.num,ncol=var.num)
    f.con.1[cbind(1:var.num,1:var.num)] <- 1
    f.dir.1 <- rep('<=',var.num)
    f.rhs.1 <- eli.vec[idx.eli]*minUnitQuantity.vec[idx.eli]
    
    f.con.2 <- matrix(0,nrow=asset.num,ncol=var.num)
    temp1 <- 1+(0:(call.num-1))*asset.num
    idx.con.2 <- rep(temp1,asset.num)+rep(c(0:(asset.num-1)),rep(call.num,asset.num))
    idx.con.2 <- match(idx.con.2,idx.eli)
    f.con.2[na.omit(cbind(rep(c(1:asset.num),rep(call.num,asset.num)),idx.con.2))]<-1
    f.dir.2 <- rep('<=',asset.num)
    f.rhs.2 <- minUnitQuantity.mat[1,]
    
    f.con.3 <- matrix(0,nrow=call.num,ncol=var.num)
    idx.con.3 <- 1:(asset.num*call.num)
    idx.con.3 <- match(idx.con.3,idx.eli)
    f.con.3[na.omit(cbind(rep(c(1:call.num),rep(asset.num,call.num)),idx.con.3))] <- minUnitValue.vec[idx.eli]*(1-haircut.vec[idx.eli])
    f.dir.3 <- rep('>=',call.num)
    f.rhs.3 <- call.mat[,1]
    
    f.obj <-  minUnitValue.vec[idx.eli]*liquidity.vec[idx.eli]
    names(f.obj) <- paste('var',1:var.num)
    
    
    ###### USE THE PACKAGE 'lpSolveAPI' ############################
    #
    # objective function: f.obj, minimize  x*value*liquidity
    #
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
    
    
    #  idx.int <- 1:var.num
    #  set.type(lps.model,idx.int,type='integer') # set integer variables
    set.semicont(lps.model,1:var.num,TRUE)        # set semi-continuous variables
    
    minMoveQuantity <- ceiling(minMoveValue/minUnitValue.vec[idx.eli])
    if(length(call.vec[which(minMoveValue > call.vec[idx.eli])])!=0){
      temp.idx <- which(minMoveValue > call.vec[idx.eli])
      call.eli.vec <- call.vec[idx.eli]
      minUnitValue.eli.vec <- minUnitValue.vec[idx.eli]
      minMoveQuantity[temp.idx] <- ceiling(call.eli.vec[temp.idx]/minUnitValue.eli.vec[temp.idx])
    }
    set.bounds(lps.model,lower=minMoveQuantity,upper=minUnitQuantity.vec[idx.eli])
    # set variables lower/upper bounds
    lp.control(lps.model,epsb=1e-30,epsd=1e-30)   # modify tolerance
    solve(lps.model)                              # solve model
    
    lpSolveAPI.solution <- get.variables(lps.model)
    
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
      select.asset.id <- assetId[select.asset.idx]
      select.asset.custodianAccount <- custodianAccount[select.asset.idx]
      select.asset.venue <- venue[select.asset.idx]
      select.asset.name <- assetInfo$name[select.asset.idx]
      select.asset.haircut <- haircut.mat[i,select.asset.idx]
      select.asset.currency <- assetInfo$currency[select.asset.idx]
      select.asset.quantity <- result.mat[i,select.asset.idx]*minUnit.mat[i,select.asset.idx]
      select.asset.unitValue <- unitValue.mat[i,select.asset.idx]
      select.asset.Amount <- select.asset.quantity*select.asset.unitValue
      select.asset.NetAmount <- select.asset.Amount*(1-haircut.mat[i,select.asset.idx])
      
      select.asset.df <- data.frame(select.asset.id,select.asset.name,select.asset.NetAmount,select.asset.haircut,select.asset.Amount,select.asset.currency,
                                    select.asset.quantity,select.asset.custodianAccount,select.asset.venue)
      colnames(select.asset.df)<- c('Asset','Name','NetAmount(USD)','Haircut','Amount','Currency','Quantity','CustodianAccount','venue')
      
      select.list[[callId[i]]] <- select.asset.df       
    }
    output.list <- select.list
  
  
}

else if(all(pref==c(1,0,0))){
  
  ##### In case of OW-254, all assets have quantity limits ##########
  
    idx.eli <- which(eli.vec==1)  # Exclude the non-eligible asset variable for each margin call
    var.num <- length(idx.eli)    # variable numbers
    var.num2 <- var.num*2
    
    f.con.0 <- matrix(0,nrow=var.num2,ncol=var.num2)
    f.con.0[cbind(1:var.num2,1:var.num2)] <- 1
    f.dir.0 <- rep('>=',var.num2)
    f.rhs.0 <- rep(0,var.num2)
    
    f.con.1 <- matrix(0,nrow=var.num2,ncol=var.num2)
    f.con.1[cbind(1:var.num2,1:var.num)] <- 1
    f.dir.1 <- rep('<=',var.num2)
    f.rhs.1 <- c(eli.vec[idx.eli]*minUnitQuantity.vec[idx.eli],rep(1,var.num))
    
    
    f.con.2 <- matrix(0,nrow=asset.num,ncol=var.num)
    temp1 <- 1+(0:(call.num-1))*asset.num
    idx.con.2 <- rep(temp1,asset.num)+rep(c(0:(asset.num-1)),rep(call.num,asset.num))
    idx.con.2 <- match(idx.con.2,idx.eli)
    f.con.2[na.omit(cbind(rep(c(1:asset.num),rep(call.num,asset.num)),idx.con.2))]<-1
    f.con.2 <- cbind(f.con.2,f.con.2*0)
    f.dir.2 <- rep('<=',asset.num)
    f.rhs.2 <- minUnitQuantity.mat[1,]
    
    f.con.3 <- matrix(0,nrow=call.num,ncol=var.num)
    idx.con.3 <- 1:(asset.num*call.num)
    idx.con.3 <- match(idx.con.3,idx.eli)
    f.con.3[na.omit(cbind(rep(c(1:call.num),rep(asset.num,call.num)),idx.con.3))] <- minUnitValue.vec[idx.eli]*(1-haircut.vec[idx.eli])
    f.con.3 <- cbind(f.con.3,f.con.3*0)
    f.dir.3 <- rep('>=',call.num)
    f.rhs.3 <- call.mat[,1]
    
    f.con.4 <- matrix(0,nrow=var.num,ncol=var.num)
    f.con.4[cbind(1:var.num,1:var.num)] <- 1
    f.con.4 <- cbind(f.con.4,f.con.4*(-10000000000))
    f.dir.4 <- rep('<=',var.num)
    f.rhs.4 <- rep(0,var.num)
    
    f.con.5 <- matrix(0,nrow=var.num,ncol=var.num)
    f.con.5[cbind(1:var.num,1:var.num)] <- 1
    f.con.5 <- cbind(f.con.5,-f.con.5)
    f.dir.5 <- rep('>=',var.num)
    f.rhs.5 <- rep(0,var.num)
    
    # when the total movements are the same, then compare the cost.
    #f.obj <-  c(minUnitValue.vec[idx.eli]*cost.vec[idx.eli],rep(1,var.num)*mean(call.mat[,1]))
    f.obj <-  c(rep(0,var.num),rep(1,var.num)*100)
    
    names(f.obj) <- paste('var',1:var.num2)
    
    ###### USE THE PACKAGE 'lpSolveAPI' ############################
    #
    # objective function: f.obj, minimize  (x*value*cost + sum(I_x)*avg(callAmount))
    #                     x, quantity used of an asset; 
    #                     I_x, indicator variable, =1 if x>0, =0 if x=0;
    # minMoveValue <- 1000
    #
    ######
    
    # constraints
    lp.con <- rbind(f.con.2,f.con.3,f.con.4,f.con.5)
    lp.dir <- c(f.dir.2,f.dir.3,f.dir.4,f.dir.5)
    lp.rhs <- c(f.rhs.2,f.rhs.3,f.rhs.4,f.rhs.5)
    
    lps.model <- make.lp(length(lp.con),var.num2)  # make model
    set.objfn(lps.model,f.obj)                    # set objective
    
    for (i in 1:length(lp.con[,1])){              # set constraints
      add.constraint(lps.model,lp.con[i,],lp.dir[i],lp.rhs[i])
    }
    
    #  idx.int <- 1:var.num
    #  set.type(lps.model,idx.int,type='integer') # set integer variables
    set.semicont(lps.model,1:var.num2,TRUE)        # set semi-continuous variables
    set.type(lps.model,(var.num+1):var.num2,type="binary")
    
    minMoveQuantity <- ceiling(minMoveValue/minUnitValue.vec[idx.eli])
    
    if(length(call.vec[which(minMoveValue > call.vec[idx.eli]/(1-haircut.vec[idx.eli]))])!=0){
      temp.idx <- which(minMoveValue > call.vec[idx.eli]/(1-haircut.vec[idx.eli]))
      call.eli.vec <- call.vec[idx.eli]/(1-haircut.vec[idx.eli])
      minUnitValue.eli.vec <- minUnitValue.vec[idx.eli]
      minMoveQuantity[temp.idx] <- ceiling(call.eli.vec[temp.idx]/minUnitValue.eli.vec[temp.idx])
    }
    set.bounds(lps.model,lower=c(minMoveQuantity,rep(0,var.num)),upper=c(minUnitQuantity.vec[idx.eli],rep(1,var.num)))
    # set variables lower/upper bounds
    lp.control(lps.model,epsd=1e-10,presolve='knapsack',timeout=5)
    
    result.status <- solve(lps.model)                              # solve model
  
    if(result.status==2){
      errorMsg <- 'Error: Asset inventory might be insufficient!'
      return(errorMsg)
    } else if(result.status==5){                            # solve model
      errorMsg <- 'Error: Fail to calculate!'
      return(errorMsg)
    } else{
    
      lpSolveAPI.solution <- get.variables(lps.model)
      
      result.mat <- matrix(0,nrow=call.num,ncol=asset.num,dimnames=list(callId,assetId))
      result.mat <- t(result.mat)
      result.mat[idx.eli]<-lpSolveAPI.solution[1:var.num]
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
        select.asset.id <- assetId[select.asset.idx]
        select.asset.custodianAccount <- custodianAccount[select.asset.idx]
        select.asset.venue <- venue[select.asset.idx]
        select.asset.name <- assetInfo$name[select.asset.idx]
        select.asset.haircut <- haircut.mat[i,select.asset.idx]
        select.asset.currency <- assetInfo$currency[select.asset.idx]
        select.asset.quantity <- result.mat[i,select.asset.idx]*minUnit.mat[i,select.asset.idx]
        select.asset.unitValue <- unitValue.mat[i,select.asset.idx]
        select.asset.Amount <- select.asset.quantity*select.asset.unitValue
        select.asset.NetAmount <- select.asset.Amount*(1-haircut.mat[i,select.asset.idx])
        
        select.asset.df <- data.frame(select.asset.id,select.asset.name,select.asset.NetAmount,select.asset.haircut,select.asset.Amount,select.asset.currency,
                                      select.asset.quantity,select.asset.custodianAccount,select.asset.venue)
        colnames(select.asset.df)<- c('Asset','Name','NetAmount(USD)','Haircut','Amount','Currency','Quantity','CustodianAccount','venue')
        
        select.list[[callId[i]]] <- select.asset.df       
      }
      
      output.list <- select.list
    }
    
  
  
}

  return(list(input=input.list,output=output.list))
}



