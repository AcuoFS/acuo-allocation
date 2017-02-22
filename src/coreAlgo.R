
coreAlgo <- function(input.list,availAssets,time.limit){
pref=pref
callIds=input.list$callIds
assetIds=input.list$assetIds
callInfo <- renjinFix(input.list$callInfo, "callInfo.")
assetInfo <- renjinFix(input.list$assetInfo, "assetInfo.")
custodianAccount <- input.list$custodianAccount
venue <- input.list$venue

call.num <- length(callIds)            # total margin call number
asset.num <- length(assetIds)          # total asset number

eli.mat <- input.list$eli.mat; eli.vec <- input.list$eli.vec                    # eligibility matrix & vector
haircut.mat<-input.list$haircut.mat; haircut.vec <- input.list$haircut.vec      # haircut mat & vec
quantity.mat<- input.list$quantity.mat; quantity.vec <- input.list$quantity.vec # asset quantity mat & vec
minUnitQuantity.mat<- input.list$minUnitQuantity.mat; minUnitQuantity.vec <- input.list$minUnitQuantity.vec

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
#  Asset(assetIds)   Name(assetName)   NetAmount(USD)(afterHaircut)    Amount      currency    quantity        value       custodianAccount
# 1  a1              asset1               numeric value           numeric value    CCY1    numeric value  numeric value     custac1
# 2  a2              asset2               numeric value           numeric value    CCY2    numeric value  numeric value     custac2
#
# $callId2
#  Asset(assetIds)   Name(assetName)   NetAmount(USD)(afterHaircut)   Amount      currency    quantity        value       custodianAccount
# 1  a2              asset2               numeric value          numeric value     CCY2    numeric value  numeric value     custac2
# 2  a3              asset3               numeric value          numeric value     CCY1    numeric value  numeric value     custac1
# 3  a4              asset4               numeric value          numeric value     CCY3    numeric value  numeric value     custac3
#------------------------------------------------------------------------------------------------------------------------------------------


############# ALGORITHM ############################################

######### CHECK WHETHER ASSET POOL IS SUFFICIENT #############
suffPerCall <- all(apply(eli.mat*(minUnitQuantity.mat*minUnitValue.mat*(1-haircut.mat)),1,sum) > call.mat[,1])
suffAllCall <- sum(minUnitQuantity.mat[1,]*minUnitValue.mat[1,]*(1-apply(haircut.mat,2,max)))>sum(call.mat[,1])
if(!(suffPerCall&suffAllCall)){
#errorMsg <- 'Error: Asset inventory is insufficient!'
stop('Asset inventory is insufficient!')
#return(errorMsg)
}

#### Recalculate the parameters weight setting
pref <- pref/sum(pref)

#### calculate the optimal asset sufficiency
reserve.list <-list()    # store all available assets for each call, list by callIds
select.list  <-list()    # store selected assets for each call, list by callIds
optimalAsset <- matrix(c(callIds,rep('', call.num)),nrow=call.num,ncol=2,dimnames = list(callIds,c('callIds','assetIds')))

call.ccy <- callInfo$currency
excess.call.percent <- 0.2
call.mat <- call.mat*(1+excess.call.percent*pref[1])
call.vec <- call.vec*(1+excess.call.percent*pref[1])

# calculate the cost if only the integral units of asset can be allocated
integer.call.mat <- ceiling(call.mat/(1-haircut.mat)/minUnitValue.mat)*minUnitValue.mat*(1-haircut.mat)

cost.mat<-integer.call.mat/(1-haircut.mat)*cost.percent.mat  # cost amount

asset.liquid <- apply((1-haircut.mat*eli.mat)^2,2,min) # define asset liquidity
liquidity.mat <- matrix(rep(asset.liquid,call.num),nrow=call.num,byrow=TRUE,dimnames=list(callIds,assetIds))
liquidity.vec <- as.vector(t(liquidity.mat))

operation.mat <- matrix(rep(1,asset.num*call.num),nrow=call.num,byrow=TRUE,dimnames=list(callIds,assetIds))
for(i in 1:call.num){
ccy.idx <- which(call.ccy[i]==assetIds)    # return the index of mc[i] currency cash in the assetIds list
idx1 <- which(eli.mat[i,]!=0)             # return elegible asset idx for mc[i]
if(length(ccy.idx)==1 && is.element(ccy.idx,idx1)){  # if there exist call currency cash in the inventory, and it's available
operation.mat[i,ccy.idx] <- 0
}
}
operation.vec <- as.vector(t(operation.mat))

norm.cost.mat <- cost.mat
for(i in 1:call.num){
if(length(unique(cost.mat[i,]))==1){
norm.cost.mat[i,]<-1
}else{
norm.cost.mat[i,]<- scale(cost.mat[i,])
norm.cost.mat[i,]<- norm.cost.mat[i,]+(-min(norm.cost.mat[i,])*2)
}
}
norm.cost.mat <- norm.cost.mat*10
norm.cost.vec <- as.vector(t(norm.cost.mat))

norm.liquidity.mat <- liquidity.mat
for(i in 1:call.num){
if(length(unique(liquidity.mat[i,]))==1){
norm.liquidity.mat[i,]<-1
}else{
norm.liquidity.mat[i,]<- scale(liquidity.mat[i,])
norm.liquidity.mat[i,]<- norm.liquidity.mat[i,]+(-min(norm.liquidity.mat[i,])*2)
}
}
norm.liquidity.mat <- norm.liquidity.mat*10
norm.liquidity.vec <- as.vector(t(norm.liquidity.mat))

norm.operation.mat <- operation.mat*9+1
norm.operation.vec <- as.vector(t(norm.operation.mat))

optimal.mat <- norm.operation.mat*pref[1]+norm.liquidity.mat*pref[2]+norm.cost.mat*pref[3]
colnames(optimal.mat) <- assetIds; rownames(optimal.mat)<-callIds


temp.minUnitQuantity.mat <- minUnitQuantity.mat
for(i in 1:call.num){
idx1 <- which(eli.mat[i,]!=0)  # return elegible asset idx for mc[i]
temp <- matrix(c(optimal.mat[i,idx1],idx1),nrow=2,byrow = T) # combine the asset cost and index together
# sort the asset per call by cost
if(length(temp[1,])==1){       # if there's only one eligible asset, no need to sort.
sortOptimal=temp
}else{
sortOptimal<-temp[,order(temp[1,])] # sort the cost, return the cost and asset idx in matrix
}
reserve.list[[callIds[i]]]<- assetIds[sortOptimal[2,]]
# if there are more than one assets have the same score, we cannot simply select the first one
# because this may cause the case that there are 3 assets have the same score for 3 calls
# if we just select the first asset, then it's possible this single asset is not sufficient to fulfill
# all these 3 calls, but these three assets can fulfill one of the call respectively

# selecting order:
# select the one which hasn't been selected to the previous call
# unless, they are from the same margin statment (deal with that in OW-379)
# Best approach, allocate the most sufficient asset to the largest call amount, deal with that later
# better to deal with that now
# round to 2 digits
min.idx <- sortOptimal[2,which(round(sortOptimal[1,],2)==round(min(sortOptimal[1,]),2))]
# if min.idx contains only one element, don't need to sort
if(length(min.idx) > 1){
    temp.optimal.asset <- assetIds[min.idx]

# temp.largestAmount.asset: the least score assets score and index(>=1)
temp.largestAmount.asset <- matrix(c(temp.minUnitQuantity.mat[i,min.idx]*minUnitValue.mat[i,min.idx],min.idx),nrow=2,byrow=T)
if(length(temp.largestAmount.asset[1,])>1){
    temp.largestAmount.asset <- temp.largestAmount.asset[,order(temp.largestAmount.asset[1,],decreasing=T)]
# substitute in sortOptimal
sortOptimal[,1:length(temp.largestAmount.asset[1,])]<- temp.largestAmount.asset
colnames(sortOptimal)[1:length(temp.largestAmount.asset[1,])] <- colnames(temp.largestAmount.asset)
}
}
optimalAsset[i,2] <- assetIds[sortOptimal[2,1]]
temp.minUnitQuantity <- temp.minUnitQuantity.mat[,sortOptimal[2,1]]
temp.minUnitQuantity.mat[,sortOptimal[2,1]]<- temp.minUnitQuantity-call.mat[i,1]/(1-haircut.mat[i,1])/minUnitValue.mat[,sortOptimal[2,1]]
#for(m in 1:length(min.idx)){
#  if(!is.element(temp.optimal.asset[m],optimalAsset[,2])){
#    optimalAsset[i,2] <- temp.optimal.asset[m]
#    break
#  }
#}
# if all possible assets have been selected as optimal of previous margin calls
# then, select the first asset
if(optimalAsset[i,2]==''){
    optimalAsset[i,2] <- temp.optimal.asset[1]
}
}

optimal.suff.qty <- call.mat/(1-haircut.mat)/minUnitValue.mat # quantity needed for a single asset to fulfill each call
select.temp.unique <- unique(optimalAsset[,2])
suff.select.unique <- rep(0,length(select.temp.unique))
for(i in 1:length(select.temp.unique)){
    id <- select.temp.unique[i]
idx.temp <- optimalAsset[which(optimalAsset[,2]==id),1] # calls have the least cost assetIds=id
suff.select.unique[i] <- 1*(sum(optimal.suff.qty[idx.temp,id]) < minUnitQuantity.mat[1,id])
}

#### In case of OW-291, optimal assets are sufficient
if(!is.element(0,suff.select.unique)){
    status <- 'solved'
for(i in 1:call.num){
    select.asset.idx <- which(assetInfo$id==optimalAsset[i,2])
select.asset.id <- assetIds[select.asset.idx]
select.asset.custodianAccount <- custodianAccount[select.asset.idx]
select.asset.venue <- venue[select.asset.idx]
select.asset.name <- assetInfo$name[select.asset.idx]
select.asset.NetAmountUSD <- integer.call.mat[i,1]
select.asset.haircut <- haircut.mat[i,select.asset.idx]
select.asset.AmountUSD <- select.asset.NetAmountUSD/(1-haircut.mat[i,select.asset.idx])
select.asset.currency <- assetInfo$currency[select.asset.idx]
select.asset.minUnitQuantity <- select.asset.AmountUSD/minUnitValue.mat[i,select.asset.idx]
select.asset.quantity <- select.asset.minUnitQuantity*minUnit.mat[i,select.asset.idx]
select.marginType <- rep(callInfo$marginType[i],length(select.asset.idx))
select.asset.FX <- assetInfo$FXRate[select.asset.idx]
select.asset.Amount <- select.asset.AmountUSD*select.asset.FX
select.asset.NetAmount <- select.asset.NetAmountUSD*select.asset.FX

#### UPDATE THE ASSET QUANTITY ########
availQuantity <- availAssets$quantity[which(availAssets$assetId==select.asset.id)]
quantity <- availAssets$totalQuantity[which(availAssets$assetId==select.asset.id)]
availAssets$quantity[which(availAssets$assetId==select.asset.id)]<- availQuantity-select.asset.quantity
availAssets$totalQuantity[which(availAssets$assetId==select.asset.id)]<- quantity-select.asset.quantity
#### END ##############################

select.asset.custodianAccount <- custodianAccount[select.asset.idx]
select.asset.df <- data.frame(select.asset.id,select.asset.name,select.asset.NetAmount,select.asset.NetAmountUSD,select.asset.FX,select.asset.haircut,select.asset.Amount,select.asset.AmountUSD,select.asset.currency,
                              select.asset.quantity,select.asset.custodianAccount,select.asset.venue,select.marginType)
colnames(select.asset.df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType')

select.list[[callIds[i]]] <- select.asset.df
}
output.list<- select.list
result.objective <- 0
}

else if(1){
    ##### In case of OW-292, consider all preference, with quantity limits ##########

idx.eli <- which(eli.vec==1)  # Exclude the non-eligible asset variable for each margin call
var.num <- length(idx.eli)    # variable numbers
var.num2 <- var.num*2

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
# 4.& 5. movements
#    Similating the dummy of each x
# variable bounds: a < x < x_quantity
#    specified by constraint 0 and 1.
# variable kind: semi-continuous, value below 'a' will automately set to 0
#
######

# objective function
operation.obj <-  c(rep(0,var.num),norm.operation.vec[idx.eli]*max(call.mat)*10)
liquidity.obj <-  c(minUnitValue.vec[idx.eli]*norm.liquidity.vec[idx.eli],rep(0,var.num))
cost.obj <-  c(minUnitValue.vec[idx.eli]*norm.cost.vec[idx.eli],rep(0,var.num))

f.obj <- operation.obj*pref[1]+liquidity.obj*pref[2]+cost.obj*pref[3]
#names(f.obj) <- paste('var',1:var.num2)

# constraints
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


# minimum movement quantity of each asset
minMoveQuantity <- ceiling(minMoveValue/minUnitValue.vec[idx.eli])
if(length(call.vec[which(minMoveValue > call.vec[idx.eli]/(1-haircut.vec[idx.eli]))])!=0){
  temp.idx <- which(minMoveValue > call.vec[idx.eli]/(1-haircut.vec[idx.eli]))
  call.eli.vec <- call.vec[idx.eli]/(1-haircut.vec[idx.eli])
  minUnitValue.eli.vec <- minUnitValue.vec[idx.eli]
  minMoveQuantity[temp.idx] <- ceiling(call.eli.vec[temp.idx]/minUnitValue.eli.vec[temp.idx])
}

### solver inputs #######
lp.obj <- f.obj
lp.con <- rbind(f.con.2,f.con.3,f.con.4,f.con.5)
lp.dir <- c(f.dir.2,f.dir.3,f.dir.4,f.dir.5)
lp.rhs <- c(f.rhs.2,f.rhs.3,f.rhs.4,f.rhs.5)
lp.kind <- rep('semi-continuous',var.num2)
lp.type <- rep('real',var.num2)
lp.type[which(minUnitValue.vec[idx.eli]>=100)] <- 'integer'
lp.bounds.lower <- c(minMoveQuantity,rep(1,var.num))
lp.bounds.upper <- c(minUnitQuantity.vec[idx.eli],rep(1,var.num))
lp.branch.mode <- c(rep('floor',var.num),rep('auto',var.num))

lp.presolve <- ifelse(call.num<=5,'none','knapsack')
lp.epsd <- 1e-11
lp.timeout <- time.limit
### end ##################


### call lpSolve solver####
lpSolve.output <- callLpSolve(f.obj,lp.con,lp.dir,lp.rhs,lp.type,lp.kind,lp.bounds.lower,lp.bounds.upper,lp.branch.mode,presolve=lp.presolve,epsd=lp.epsd,timeout=lp.timeout)
### end ##################

#### solver outputs########
result.status<- lpSolve.output$result.status
lpSolveAPI.solution <- lpSolve.output$lpSolveAPI.solution
result.objective <- lpSolve.output$result.objective
#### end ##################

status <- 'solved'
if(is.element(result.status,c(2,13))){
  #errorMsg <- 'Error: Asset inventory might be insufficient!'
  #return(errorMsg)
  stop('Asset inventory might be insufficient!')
  status <- 'insufficient'
} else if(is.element(result.status,c(5,6,10))){                            # solve model
  #errorMsg <- 'Error: Fail to calculate!'
  #return(errorMsg)
  stop('Fail to calculate!')
  status <- 'fail'
} else if(result.status==1){
  #warning('sub-optimal result!')
  status<-'sub-optimal'
} else if(result.status==7){
  #stop('Time out!')
  status<-'timeout'
}

# round up the decimal quantity to the nearest integer.
# if it's larger than 0.5
result.mat <- matrix(0,nrow=call.num,ncol=asset.num,dimnames=list(callIds,assetIds))
result.mat <- t(result.mat)
result.mat[idx.eli]<-lpSolveAPI.solution[1:var.num]
result.mat[which(result.mat>0.5)] <- ceiling(result.mat[which(result.mat>0.5)])
result.mat <- t(result.mat)                   # convert solution into matrix format


##### CHECK ALLOCATION RESULT #############################
###########################################################
# STATUS: Developing
#
# 1. whether all variables are non-negative
neg.idx <- which(result.mat<0)
if(length(neg.idx)>=1){
    result.mat[neg.idx] <-0 # set to 0 first, then check the other two criteria
}

# 2. whether statisfy the quantity limits
asset.quantity.used <- apply(result.mat,2,sum)
asset.quantity.left <- minUnitQuantity.mat[1,]-asset.quantity.used
excess.idx <- which(asset.quantity.used>minUnitQuantity.mat[1,])
if(length(excess.idx)>=1){
    for(i in excess.idx){          # i: the index of the excess quantity asset in assetIds
current.allocate <- matrix(c(which(result.mat[,i]>0),result.mat[which(result.mat[,i]>0),i]),nrow=2,byrow=T)
if(length(current.allocate[1,])>1){
    current.allocate<-current.allocate[,order(current.allocate[2,])]
}
for(k in 1:length(current.allocate[1,])){ # k: the kth margin call which asset[i] allocated to
j = current.allocate[1,k]  # j: the index of the the kth margin call in callIds
# current allocated quantity < excess quanity
if(current.allocate[2,k]< (-asset.quantity.left[i])){
    # the amount missing for the margin call j if excluding the asset i
new.quantity <- 0
other.amount <- sum(result.mat[j,1+which(result.mat[j,-i]>0)]*minUnitValue.mat[j,1+which(result.mat[j,-i]>0)]*(1-haircut.mat[j,1+which(result.mat[j,-i]>0)]))
missing.amount <- call.mat[j,1]-(other.amount+new.quantity/(1-haircut.mat[j,i])/minUnitValue.mat[j,i])
# missing.amount<0, means even we substract the exceed quantity of the asset,
# the sub-total is still larger than call amount, then, we update asset to the
# least quantity(already 0) which can meet the margin call requirement, no swaps occur
if(missing.amount<=0){
    result.mat[j,i]<- new.quantity

asset.quantity.used <- apply(result.mat,2,sum)
asset.quantity.left <- minUnitQuantity.mat[1,]-asset.quantity.used
break
}
# first check whether the other previous allocated assets are sufficient,based on the operation efficiency
# find the other asset which is sufficient and eligible for margin call j

missing.quantity <- ceiling((missing.amount/(1-haircut.mat)/minUnitValue.mat)[j,])
suff.idx <- intersect(which(missing.quantity<=asset.quantity.left),which(eli.mat[j,]==1))

# whether there are other assets allocated to call j
swap.prob.idx <- intersect(which(result.mat[j,]>0),suff.idx)
if(length(swap.prob.idx)>=1){
    swap.new.idx <- swap.prob.idx[1]
}else{
    swap.new.idx <- suff.idx[1]
}
swap.new.quantity <- missing.quantity[swap.new.idx]+result.mat[j,swap.new.idx]
new.allocate <- matrix(current.allocate[,-which(current.allocate[1,]==j)],nrow=2)

if(length(which(result.mat[,swap.new.idx]>0))){
    swap.allocate<- matrix(c(which(result.mat[,swap.new.idx]>0),result.mat[which(result.mat[,swap.new.idx]>0),swap.new.idx]),nrow=2,byrow=T)
swap.allocate[2,which(swap.allocate[1,]==j)] <- swap.new.quantity
}else{
    swap.allocate<- matrix(c(swap.new.idx,swap.new.quantity),nrow=2)
}
# update the result.mat
result.mat[j,c(i,swap.new.idx)]<- c(new.quantity,swap.new.quantity)

asset.quantity.used <- apply(result.mat,2,sum)
asset.quantity.left <- minUnitQuantity.mat[1,]-asset.quantity.used
}
else{
    # the amount missing for the margin call j if excluding the asset i
# shouldn't exclude the asset i, just reduce to the sufficient amount, and use other assets to fulfil the left call amount
new.quantity<- current.allocate[2,which(current.allocate[1,]==j)]+asset.quantity.left[i]

# if this asset is the only selection
if(call.num==1){
    other.amount <- sum(result.mat[,-i][which(result.mat[-i]>0)]*minUnitValue.mat[,-i][which(result.mat[-i]>0)]*
(1-haircut.mat[,-i][which(result.mat[-i]>0)]))
} else{
    other.amount <- sum(result.mat[,-i][j,which(result.mat[j,-i]>0)]*minUnitValue.mat[,-i][j,which(result.mat[j,-i]>0)]*
(1-haircut.mat[,-i][j,which(result.mat[j,-i]>0)]))
}
missing.amount <- call.mat[j,1]-(other.amount+new.quantity*minUnitValue.mat[j,i]*(1-haircut.mat[j,i]))
# missing.amount<0, means even we substract the exceed quantity of the asset,
# the sub-total is still larger than call amount, then, we update asset to the
# least quantity which can meet the margin call requirement, no swaps occur
if(missing.amount<=0){
    new.quantity <-  ceiling((call.mat[j,1]-other.amount)/minUnitValue.mat[j,i]/(1-haircut.mat[j,i]))
result.mat[j,i]<- new.quantity
asset.quantity.used <- apply(result.mat,2,sum)
asset.quantity.left <- minUnitQuantity.mat[1,]-asset.quantity.used
break
}

# first check whether the other previous allocated assets are sufficient,based on the operation efficiency
# find the other asset which is sufficient and eligible for margin call j
missing.quantity <- ceiling((missing.amount/(1-haircut.mat)/minUnitValue.mat)[j,])
suff.idx <- intersect(which(missing.quantity<=asset.quantity.left),which(eli.mat[j,]==1))

if(length(suff.idx)==0){
    # sacrifice the fulfilled call amount if the it is still larger than the shreshod
if((call.mat[j,1]-missing.amount)>=callInfo$callAmount[j]){
    result.mat[j,i]<- new.quantity
}
# left quantity of each available asset for this call is not sufficient
# need more than one assets to allocate to this call
# compare the missing amount and the sum of the left asset left amount
# asset.amount.left <- matrix(c(1:asset.num,asset.quantity.left*minUnitValue.mat[j,]),nrow=2,byrow=T)

# there should be more than one assets available(else will be detected in the pre-check sufficiency part)
# order by amount from larger to smaller, make sure the least movements
# asset.amount.left <- asset.amount.left[,order(asset.amount.left[2,])]

# the index of available assets, excluding the
# temp.idx <- intersect(which(asset.quantity.left>0),which(eli.mat[j,]==1))
} else{
    # whether there are other assets allocated to call j
swap.prob.idx <- intersect(which(result.mat[j,]>0),suff.idx)
if(length(swap.prob.idx)>=1){
    swap.new.idx <- swap.prob.idx[1]
} else{
    swap.new.idx <- suff.idx[1]
}
swap.new.quantity <- missing.quantity[swap.new.idx]+result.mat[j,swap.new.idx]

new.allocate <- current.allocate
new.allocate[,-which(current.allocate[1,]==j)] <- new.quantity

if(length(which(result.mat[,swap.new.idx]>0))){
    swap.allocate<- matrix(c(which(result.mat[,swap.new.idx]>0),result.mat[which(result.mat[,swap.new.idx]>0),swap.new.idx]),nrow=2,byrow=T)
swap.allocate[2,which(swap.allocate[1,]==j)] <- swap.new.quantity
}else{
    swap.allocate<- matrix(c(swap.new.idx,swap.new.quantity),nrow=2)
}

# update the result.mat
result.mat[j,c(i,swap.new.idx)]<- c(new.quantity,swap.new.quantity)
}


asset.quantity.used <- apply(result.mat,2,sum)
asset.quantity.left <- minUnitQuantity.mat[1,]-asset.quantity.used
# break
break
}
}
}
}

# 3. whether meet all margin call requirements
asset.quantity.used <- apply(result.mat,2,sum)
asset.quantity.left <- minUnitQuantity.mat[1,]-asset.quantity.used
# compare with the call amount, not the custimized amount based on the user preference
call.fulfilled <- apply(result.mat*minUnitValue.mat*(1-haircut.mat),1,sum)
call.missing.amount <- callInfo$callAmount-call.fulfilled
call.missing.idx <- which(call.missing.amount>0)
if(length(call.missing.idx)>=1){
    for(i in call.missing.idx){
    current.allocate <- matrix(c(which(result.mat[i,]>0),result.mat[i,which(result.mat[i,]>0)]),nrow=2,byrow=T)
missing.amount <- call.missing.amount[i]
missing.quantity <- ceiling((missing.amount/(1-haircut.mat)/minUnitValue.mat)[j,])
suff.idx <- intersect(which(missing.quantity<=asset.quantity.left),which(eli.mat[j,]==1))
if(length(suff.idx)==0){
    # which means none of the asset itself is enough to to fulfill the left amount of the margin call
# This should be a very extreme case, and it's more complicated to develop for this case
# so, I will leave here blank, once I'm done the rest part I'll return to check
# Also, the exception handling will be a long-run development, and it will be raised once we have exception
}

# whether there are assets which are sufficient allocated to call i
current.prob.idx <- intersect(suff.idx,current.allocate[1,])
if(length(current.prob.idx)==0){
    current.prob.idx<- suff.idx
}
add.new.idx <- current.prob.idx[1]
add.new.quantity <- missing.quantity[add.new.idx]+result.mat[i,add.new.idx]
result.mat[i,add.new.idx] <- add.new.quantity
}
}
##########################################################
############## END #######################################



for(i in 1:call.num){                          # store the result into select list
select.asset.idx <- which(result.mat[i,]!=0)
select.asset.id <- assetIds[select.asset.idx]
select.asset.custodianAccount <- custodianAccount[select.asset.idx]
select.asset.venue <- venue[select.asset.idx]
select.asset.name <- assetInfo$name[select.asset.idx]
select.asset.haircut <- haircut.mat[i,select.asset.idx]
select.asset.currency <- assetInfo$currency[select.asset.idx]
select.asset.minUnitQuantity <- result.mat[i,select.asset.idx]
select.asset.quantity <- result.mat[i,select.asset.idx]*minUnit.mat[i,select.asset.idx]
select.marginType <- rep(callInfo$marginType[i],length(select.asset.idx))

#### UPDATE THE ASSET QUANTITY ########
for(k in 1:length(select.asset.id)){
    assetId <- select.asset.id[k]
availQuantity <- availAssets$quantity[which(availAssets$assetId==assetId)]
quantity <- availAssets$totalQuantity[which(availAssets$assetId==assetId)]
availAssets$quantity[which(availAssets$assetId==assetId)]<- availQuantity-select.asset.quantity[k]
availAssets$totalQuantity[which(availAssets$assetId==assetId)]<- quantity-select.asset.quantity[k]
}
#### END ##############################

select.asset.unitValue <- unitValue.mat[i,select.asset.idx]
select.asset.AmountUSD <- select.asset.quantity*select.asset.unitValue
select.asset.NetAmountUSD <- select.asset.AmountUSD*(1-haircut.mat[i,select.asset.idx])
select.asset.FX <- assetInfo$FXRate[select.asset.idx]
select.asset.Amount <- select.asset.AmountUSD*select.asset.FX
select.asset.NetAmount <- select.asset.NetAmountUSD*select.asset.FX
select.asset.df <- data.frame(select.asset.id,select.asset.name,select.asset.NetAmount,select.asset.NetAmountUSD,select.asset.FX,select.asset.haircut,select.asset.Amount,select.asset.AmountUSD,select.asset.currency,
                              select.asset.quantity,select.asset.custodianAccount,select.asset.venue,select.marginType)
colnames(select.asset.df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType')

select.list[[callIds[i]]] <- select.asset.df
}
output.list <- select.list

}

subtotal.fulfilled<- matrix(c(input.list$call.mat[,1],rep(0, call.num)),nrow=call.num,ncol=2,dimnames = list(callIds,c('callAmount','fulfilledAmount')))
for(i in 1:call.num){
    subtotal.fulfilled[i,2] <- sum(select.list[[callIds[i]]]$`NetAmount(USD)`)
}
check.call <- round(subtotal.fulfilled,2)
return(list(output=output.list,check.call=check.call,availAssets=availAssets,status=status,objective=result.objective))
}

renjinFix <- function(frame, name) {
  d <- data.frame(frame);
  colnames(d) <- gsub(name, "", colnames(d));
  return(d);
}

callLpSolve <- function(lp.obj,lp.con,lp.dir,lp.rhs,
                        lp.type=lp.type,lp.kind=lp.kind,lp.bounds.lower=lp.bounds.lower,lp.bounds.upper=lp.bounds.upper,lp.branch.mode=lp.branch.mode,
                        ...){
  # input variables
  # must have: lp.obj,lp.con,lp.dir,lp.rhs
  # optional: lp.type,lp.kind,lp.bounds.lower,lp.bounds.upper,lp.branch.mode
  # optional: ...
  # if optional, then the default parameters will apply
  
  # number of decision variables
  var.num <- length(lp.con[1,])
  
  # make model
  lps.model <- make.lp(0,var.num)  
  
  # set objective
  set.objfn(lps.model,lp.obj)                    
  
  # set constraints
  for (i in 1:length(lp.con[,1])){              
    add.constraint(lps.model,lp.con[i,],lp.dir[i],lp.rhs[i])
  }
  
  if(!missing(lp.kind)){
    # set semi-continuous variables
    semi.idx <- which(lp.kind=='semi-continuous')
    set.semicont(lps.model,semi.idx,TRUE)        
  }

  if(!missing(lp.type)){
    # set integer variables
    int.idx <- which(lp.type=='integer')
    set.type(lps.model,int.idx,'integer')
  }

  if(!(missing(lp.bounds.lower)|| missing(lp.bounds.upper))){
    # set variables bounds
    set.bounds(lps.model,lower=lp.bounds.lower,upper=lp.bounds.upper)
  }

  if(!missing(lp.branch.mode)){
    # set branch mode
    for(k in 1:length(lp.branch.mode)){
      set.branch.mode(lps.model,k,lp.branch.mode[k])
    }  
  }
  
  # set control options
  lp.control(lps.model,...)
  
  # solve the problem
  result.status <- solve(lps.model)  
  
  # get the variables(minUnitQuantity)
  lpSolveAPI.solution <- get.variables(lps.model)
  
  # get the objective
  result.objective <- get.objective(lps.model)
  
  return(list(result.status=result.status,lpSolveAPI.solution=lpSolveAPI.solution,result.objective=result.objective))
}
