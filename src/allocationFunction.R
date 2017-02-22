library('lpSolveAPI')

#### ALLOCATION MAIN FUNCTION ############
allocationAlgo <- function(callIds,assetIds,clientId,callInfo,availAssets,assetInfo,pref,time.limit,call.limit=c(10,6,6)){

    ########### CONSTANTS ################################
    order.method <- 2
    limit.VM <- limit[1]
    limit.IM <- limit[2]
    limit.total <- limit[3]
    ########### END ######################################

    #### ORDER THE CALL ID ################################################
    ## method 1: By margin call amount, decreasing
    ## method 2: By margin type, VM then IM; sub order by call amount
    callInfo <- orderCallIds(order.method,callInfo)
    callIds <- callInfo$id

    ######## END ###########################################################

    ######## SPLIT the call ids in to several groups #######################
    # method 1: group by marginType
    # maximum limit.VM VM or limit.IM IM a time
    group.list <- splitCallIds(limit.VM,limit.IM,limit.total,callInfo,callIds)
    ############# END ###################################################


    ############### PROCESSING DATA #####################################

    asset.num <- length(assetIds)
    call.num <- length(callIds)

    # allocate one group a time
    # after each allocation, update the quantity of each asset
    output.list <- list()
    check.call <- matrix(c(callInfo$callAmount,rep(0,call.num)),nrow=call.num, dimnames = list(callIds,c('callAmount','fulfilledAmount')))
    objective <- rep(0,length(group.list))

    ############ ITERATE THE GROUP, RUN THE ALGO #########################
    for(i in 1:length(group.list)){
        callIds.group <- group.list[[i]]
        cat(' group:',i,'\n','callIds:',callIds.group,'\n')
        callInfo.group <- callInfo[match(callIds.group,callInfo$id),]
        availAssets.group <- availAssets[which(availAssets$callId %in% callIds.group),]
        assetIds.group <- unique(availAssets.group$assetId)
        assetInfo.group <- assetInfo[match(assetIds.group,assetInfo$id),]

        # input data to the core Algo
        input.list <- allocationInputData(callIds.group,assetIds.group,clientId,callInfo.group,availAssets.group,assetInfo.group,pref)

        # core Algo, assume all data comes in a list

        result.group <- coreAlgo(input.list,availAssets,time.limit)
        output.group <- result.group$output

        status <- result.group$status
        check.call.group <- result.group$check.call
        availAssets <- result.group$availAssets
        objective[i] <- result.group$objective
        for(k in 1:length(callIds.group)){
            callId <- callIds.group[k]
            output.list[[callId]] <- output.group[[callId]]
            check.call[which(rownames(check.call)==callId),2] <- check.call.group[which(rownames(check.call.group)==callId),2]
        }
    }

    return(list(output=output.list,check.call=check.call,status=status,objective=objective))
}

allocation <- function(callIds,assetIds,clientId,callInfo,availAssets,assetInfo,pref,time.limit){

    ########### CONSTANTS ################################
    order.method <- 2
    ########### END ######################################

    #### ORDER THE CALL ID ################################################
    ## method 1: By margin call amount, decreasing
    ## method 2: By margin type, VM then IM; sub order by call amount
    callInfo <- orderCallIds(order.method,callInfo)
    callIds <- callInfo$id

    ######## END ###########################################################

    ############### PROCESSING DATA #####################################

    asset.num <- length(assetIds)
    call.num <- length(callIds)

    assetInfo <- assetInfo[match(assetIds,assetInfo$id),]

    # input data to the core Algo
    input.list <- allocationInputData(callIds,assetIds,clientId,callInfo,availAssets,assetInfo,pref)

    # core Algo, assume all data comes in a list
    result <- coreAlgo(input.list,availAssets,time.limit)

    return(result)
}

allocationInputData = function(callIds,assetIds,clientId,callInfo,availAssets,assetInfo,pref){

    asset.num <- length(assetIds)
    call.num <- length(callIds)

    callInfo$currency[which(is.na(callInfo$currency))] <- 'ZZZ'

    availAssets <- availAssets[order(availAssets$callId),] # order the availAssets by callIds

    custodianAccount <- availAssets$CustodianAccount[match(assetIds,availAssets$assetId)]
    venue <- availAssets$venue[match(assetIds,availAssets$assetId)]
    ###############################################
    # eligibility matrix: 1-eligible, 0-ineligible
    # haircut matrix: haircut+FX haircut
    # quantity matrix
    # unitValue matrix: unitValue/FX rate
    # cost matrix: internal+external+opptunity-yield(interestRate)
    # call amount matrix: duplicate the column
    # minUnit matrix: minUnit[i,j]=x, asset j for margin call i has a minimum denomination x,
    #     which means we can only allocate the integral multiples quantity of A_j to MC_i.
    #     To start with, we use (i>=1) for non-cash securities; 0.01 for cash, apply to all margin calls.
    ############################################

    base.mat <- matrix(0,nrow=call.num,ncol=asset.num, dimnames = list(callIds,assetIds))

    eli.mat <- base.mat
    haircut.mat <- base.mat
    cost.mat <- base.mat
    quantity.mat <- base.mat
    minUnitQuantity.mat <- base.mat
    call.mat <- base.mat

    unitValue.mat<- base.mat
    minUnit.mat <- base.mat
    minUnitValue.mat <- base.mat


    # fill in matrixes with the data from availAssets
    call.mat[callIds,] <- matrix(rep(callInfo$callAmount,asset.num),nrow=call.num,byrow=F)
    quantity.mat[,]<- matrix(rep(as.numeric(unique(cbind(availAssets$assetId,availAssets$quantity))[,2]),call.num),nrow=call.num,byrow=T)

    temp.callIds.idx <- match(availAssets$callId,callIds)
    temp.assetIds.idx <- match(availAssets$assetId,assetIds)
    eli.mat[cbind(temp.callIds.idx,temp.assetIds.idx)]<- 1
    haircut.mat[cbind(temp.callIds.idx,temp.assetIds.idx)]<- availAssets$haircut+availAssets$FXHaircut
    cost.mat[cbind(temp.callIds.idx,temp.assetIds.idx)]<- availAssets$internalCost+availAssets$externalCost+availAssets$opptCost-(availAssets$interestRate+availAssets$yield)

    #eli.mat = buildMatrix(unique(availAssets$callId), unique(availAssets$assetId))
    #haircut.mat = buildMatrix2(unique(availAssets$callId), unique(availAssets$assetId), availAssets$haircut+availAssets$FXHaircut)
    #cost.mat = buildMatrix2(unique(availAssets$callId), unique(availAssets$assetId), availAssets$internalCost+availAssets$externalCost+availAssets$opptCost-(availAssets$interestRate+availAssets$yield))

    unitValue.mat[,] <- matrix(rep(assetInfo$unitValue/assetInfo$FXRate,call.num),nrow=call.num,byrow=TRUE)
    minUnit.mat[,]<- matrix(rep(assetInfo$minUnit,call.num),nrow=call.num,byrow=TRUE)
    minUnitValue.mat[,] <- matrix(rep(assetInfo$minUnitValue/assetInfo$FXRate,call.num),nrow=call.num,byrow=TRUE)

    minUnitQuantity.mat[,]<- floor(quantity.mat/minUnit.mat) # round down to the nearest integer

    # convert the matrix format data to vector format
    eli.vec <- as.vector(t(eli.mat))
    haircut.vec <- as.vector(t(haircut.mat))
    cost.vec <- as.vector(t(cost.mat))
    quantity.vec <- as.vector(t(quantity.mat))
    minUnitQuantity.vec <- as.vector(t(minUnitQuantity.mat))
    unitValue.vec <- as.vector(t(unitValue.mat))
    minUnit.vec <- as.vector(t(minUnit.mat))
    minUnitValue.vec <- as.vector(t(minUnitValue.mat))

    output.list <- list(assetIds=assetIds,callIds=callIds,assetInfo=assetInfo,callInfo=callInfo,pref=pref,
    custodianAccount=custodianAccount,venue=venue,
    eli.mat=eli.mat, eli.vec = eli.vec,
    haircut.mat=haircut.mat, haircut.vec=haircut.vec,
    cost.mat = cost.mat, cost.vec = cost.vec,
    quantity.mat=quantity.mat, quantity.vec=quantity.vec,
    minUnitQuantity.mat=minUnitQuantity.mat,minUnitQuantity.vec=minUnitQuantity.vec,
    unitValue.mat=unitValue.mat,unitValue.vec=unitValue.vec,
    minUnit.mat=minUnit.mat, minUnit.vec=minUnit.vec,
    minUnitValue.mat=minUnitValue.mat,minUnitValue.vec= minUnitValue.vec,
    call.mat = call.mat
    )

    return (output.list)
}

orderCallIds <- function(order.method,callInfo){
    if(order.method==1){
        callInfo <- callInfo[order(callInfo$callAmount,decreasing=T),]
    }else if(order.method==2){
        callInfoVM <- callInfo[which(toupper(callInfo$marginType)=='VARIATION'),]
        callInfoVM <- callInfoVM[order(callInfoVM$callAmount,decreasing=T),]
        callInfoIM <- callInfo[which(toupper(callInfo$marginType)=='INITIAL'),]
        callInfoIM <- callInfoIM[order(callInfoIM$callAmount,decreasing=T),]

        callInfo <- rbind(callInfoVM,callInfoIM)
    }
    return(callInfo)
}
splitCallIds <- function(limit.VM,limit.IM,limit.total,callInfo,callIds){

group.list <- list()

# if the total call numbers is equal or less than 6, only one group
if(length(callInfo[,1])<=limit.total){
group.list[[1]] <- callIds
} else{
# index of VM and IM in the call list
idx.VM <- which(toupper(callInfo$marginType)=='VARIATION')
idx.IM <- which(toupper(callInfo$marginType)=='INITIAL')

# number of VM and IM groups
group.VM.num <- ceiling(length(idx.VM)/limit.VM)
group.IM.num <- ceiling(length(idx.IM)/limit.IM)

# make the group list, VM and IM in the same list
index <- 0
if(group.VM.num==1){
index <- index+1
group.list[[index]] <- callIds[idx.VM]
} else if(group.VM.num > 1){
for(i in 1:(group.VM.num-1)){
index <- index+1
group.list[[index]] <- callIds[idx.VM[(i-1)*limit.VM+(1:limit.VM)]]
}
index <- index+1
group.list[[index]] <- callIds[tail(idx.VM,length(idx.VM)-(group.VM.num-1)*limit.VM)]
}

if(group.IM.num==1){
    index <- index+1
group.list[[index]] <- callIds[idx.IM]
} else if(group.IM.num > 1){
    for(i in 1:(group.IM.num-1)){
    index <- index+1
group.list[[index]] <- callIds[idx.IM[(i-1)*limit.IM+(1:limit.IM)]]
}
index <- index+1
group.list[[index]] <- callIds[tail(idx.IM,length(idx.IM)-(group.IM.num-1)*limit.IM)]
}
}
return(group.list)
}

buildMatrix2 <- function(x, y, value){
  nx = length(x)
  ny = length(y)
  m = value
  dim(m) = c(nx, ny)
  dimnames(m) = list(x, y)
  return(m)
}

buildMatrix <- function(x, y){
  nx = length(x)
  ny = length(y)
  m = rep(1, nx*ny)
  dim(m) = c(nx, ny)
  dimnames(m) = list(x, y)
  return(m)
}