options(stringsAsFactors = FALSE)

#source("rscripts/allocationFunction.R")
#source("rscripts/coreAlgo.R")
#source("src/callLpSolve.R")



callInfo_df <- callInfoByCallId
callInfo_df$callAmount <- abs(as.numeric(callInfo_df$callAmount)) # make sure the callAmount is non-negative
#callInfo_df$callAmountUSD <- abs(as.numeric(callInfo_df$callAmountUSD)) # make sure the callAmount is non-negative
#print(callInfo_df)

availAsset_df <- availAssetByCallIdAndClientId
availAsset_df <- availAsset_df[order(availAsset_df$callId),]

###### 3 lines added fot testing purposes, comment them after tests ##################
# changing the asset quantity and adding new custodian account to make the optimal assets insufficient,
# so that the allocation function will call the lpSolver.
# availAsset_df$quantity <- availAsset_df$quantity/5
# availAsset_df <- rbind(availAsset_df,availAsset_df)
# availAsset_df$CustodianAccount[1:length(availAsset_df[,1])/2] <- 'custodianAccountTest'
######## end #############################################################################

assetCustacId_vec <- paste(availAsset_df$assetId,availAsset_df$CustodianAccount,sep='-')
availAsset_df$assetCustacId <- assetCustacId_vec
resource_vec <- unique(assetCustacId_vec)

assetId_vec <- unique(availAsset_df$assetId)
assetInfo_df <- assetInfoByAssetId
assetInfo_df <- assetInfo_df[match(assetId_vec,assetInfo_df$id),]

###### Manually add FXRate and venue for DEMO purposes ########
# FX rate: value = 1 USD can change how much foreign currency
ccy_vec <- c('USD', 'EUR', 'GBP', 'SGD', 'JPY', 'HKD', 'AUD', 'CNY', 'KRW', 'CAD')
FXRate_vec <- c(1,0.92,0.80,1.39,110.6,7.77,1.32,6.86,1112,1.34)
for(k in 1:length(assetInfo_df[,1])){
  idxTemp <- which(assetInfo_df$currency[k] == ccy_vec)
  assetInfo_df$FXRate[k] <- FXRate_vec[idxTemp]
}

# venue: all SG
venue_vec <- rep('SG',length(availAsset_df[,1]))
availAsset_df$venue <- venue_vec
#print(availAsset_df)
###### END ####################################


callId_vec <- callIds
pref_vec <- pref

## main function, interface of java #######
## CALL THE ALLOCATION FUNCTION ###########
algoVersion <- 1
operLimit<- 10; 
inputLimit_vec <- c(7,7,7,5); 
timeLimit <- 10; 
callOrderMethod <- 3
minMoveValue <- 1000;


callInfo_df <- OrderCallId(callOrderMethod,callInfo_df)
callId_vec <- callInfo_df$id
msId_vec <- unique(callInfo_df$marginStatement)
#### Order callId_vec END ######################################

#cat('msId_vec',msId_vec)

#### Group the callId_vec Start ################################
# method 1: group by marginType
# maximum limitVm VM or limitIm IM a time
# group by margin statements
imLimit <- inputLimit_vec[1]
vmLimit <- inputLimit_vec[2]  
callLimit <- inputLimit_vec[3]
msLimit <- inputLimit_vec[4]

groupCallId_list <- GroupCallIdByMs(callLimit,msLimit,callInfo_df,callId_vec)
#### Group the callId_vec Start #################################

#print('groupCallId_list');print(groupCallId_list)

callNum <- length(callId_vec)

# allocate one group a time
# after each allocation, update the tempQuantity_vec of each asset
callOutput_list <- list()
msOutput_list <- list()
checkCall_mat <- matrix(c(callInfo_df$callAmount,rep(0,callNum)),nrow=callNum, dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
costDaily <- 0
costMonthly <- 0
movements <- 0
############ ITERATE THE GROUP, RUN THE ALGO Start #########################

for(i in 1:length(groupCallId_list)){
  
  callIdGroup_vec <- groupCallId_list[[i]]
  msIdGroup_vec <- unique(callInfo_df$marginStatement[which(callInfo_df$id %in% callIdGroup_vec)])
  cat(' group:',i,'\n','callId_vec:',callIdGroup_vec,'\n')
  
  callInfoGroup_df <- callInfo_df[match(callIdGroup_vec,callInfo_df$id),]
  availAssetGroup_df <- availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),]
  
  #print('availAssetGroup_df'); print(availAssetGroup_df)
  
  resourceGroup_vec <- unique(availAssetGroup_df$assetCustacId)
  assetIdGroup_vec <- matrix(unlist(strsplit(resourceGroup_vec,'-')),nrow=2)[1,]
  #print('assetIdGroup_vec'); print(assetIdGroup_vec)
  assetInfoGroup_df <- assetInfo_df[match(assetIdGroup_vec,assetInfo_df$id),]
  #print('assetInfoGroup_df');print(assetInfoGroup_df)
  # input data to the core Algo
  coreInput_list <- AllocationInputData(callIdGroup_vec,resourceGroup_vec,callInfoGroup_df,availAssetGroup_df,assetInfoGroup_df)
  print(coreInput_list)
  #### Pre-allocate Start ######################
  availAssetPre_df <- availAssetGroup_df
  callInfoPre_df <- callInfoGroup_df
  assetInfoPre_df <- assetInfoGroup_df
  callOutputPre_list <- callOutput_list
  print('callOutputPre_list before pre'); print(callOutputPre_list)
  for(p in 1:length(callIdGroup_vec)){
    callId <- callIdGroup_vec[p]
    res <- PreAllocation(algoVersion,callId,callInfoPre_df,availAssetPre_df,assetInfoPre_df,pref_vec,operLimit,minMoveValue,timeLimit,callOutput_list,checkCall_mat)
    print(paste('res',p,callId)); #print(res)
    availAssetPre_df <- res$availAsset_df
    #availAssetPre_df[which(availAssetPre_df$callId %in% callId),] <- availAssetPreGroup_df
    callOutputPreGroup_list <- res$callOutput_list
    print('callOutputPreGroup_list:');print(callOutputPreGroup_list)
    #resultPre_list <- res$resultGroup_list
    checkCallPre_mat <- res$checkCall_mat
    callOutputPre_list[[callId]] <- callOutputPreGroup_list[[callId]]
  }
  
  # parameteres need to pass to the CoreAlgoV2
  # callOutputPre_list
  # availAssetPre_df # don't need, solver will auto deduct the quantity while solving
  # combine into a single list: preAllocation_list
  
  initAllocation_list <- callOutputPre_list
  #### Pre-allocate End ########################
  
  print('initAllocation_list');print(initAllocation_list)
  print('availAssetGroup_df:'); print(availAssetGroup_df)
  #### Run CoreAlgo Start ######################
  if(algoVersion==1){
    print('algoVersion=1')
    #resultGroup_list <- CoreAlgoV1(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,minMoveValue,verbose,initAllocation_list)
    
    
    #### Prepare Parameters Start #############################
    pref_vec <- pref_vec/sum(pref_vec[2:3]) # Recalculate the parameters weight setting
    callId_vec<-coreInput_list$callId_vec
    resource_vec<-coreInput_list$resource_vec
    
    assetId_vec <- matrix(unlist(strsplit(resource_vec,'-')),nrow=2)[1,]
    msId_vec <- unique(callInfo_df$marginStatement)
    
    callInfo_df <- renjinFix(coreInput_list$callInfo_df, "callInfo.")
    assetInfo_df <- renjinFix(coreInput_list$assetInfo_df, "assetInfo.")
    availAsset_df <- renjinFix(availAsset_df,"availAsset.")
    
    custodianAccount <- coreInput_list$custodianAccount  
    venue <- coreInput_list$venue
    
    callNum <- length(callId_vec)            # total margin call number
    resourceNum <- length(resource_vec)          # total asset number
    
    base_mat <- coreInput_list$base_mat
    eli_mat <- coreInput_list$eli_mat; eli_vec <- coreInput_list$eli_vec                    # eligibility matrix & vector
    haircut_mat<-coreInput_list$haircut_mat; haircut_vec <- coreInput_list$haircut_vec      # haircut mat & vec
    quantity_mat<- coreInput_list$quantity_mat; quantity_vec <- coreInput_list$quantity_vec # asset quantity mat & vec
    minUnitQuantity_mat<- coreInput_list$minUnitQuantity_mat; minUnitQuantity_vec <- coreInput_list$minUnitQuantity_vec
    
    unitValue_mat<-coreInput_list$unitValue_mat; unitValue_vec <- coreInput_list$unitValue_vec     # asset unit value mat & vec
    minUnit_mat <- coreInput_list$minUnit_mat; minUnit_vec <- coreInput_list$minUnit_vec;
    minUnitValue_mat <- coreInput_list$minUnitValue_mat; minUnitValue_vec <- coreInput_list$minUnitValue_vec;
    
    callAmount_mat <- coreInput_list$callAmount_mat; callAmount_vec <- as.vector(t(callAmount_mat)) 
    
    costBasis_mat <- coreInput_list$cost_mat; costBasis_vec <- coreInput_list$cost_vec 
    
    #### Prepare Parameters END ##############################
    
    #### CONSTANTS DEFINED INSIDE THE ALGO START #############
    if(missing(minMoveValue)){
      minMoveValue <- 1000
    }
    #### CONSTANTS DEFINED INSIDE THE ALGO END ###############
    
    #### Output Format Start ######################
    # A list, each element is the allocation result(dataframe) for one margin call
    callSelect_list  <- list()    # store selected assets for each call, list by callId_vec
    msSelect_list <- list()   # store selected assets for each margin statement, list by msId
    #----------------------------------------------------------------------------------------------------------------
    # $callOutput$mcp38
    #  Asset         Name        NetAmount     NetAmount(USD)   FXRate  Haircut Amount   Amount(USD) Currency Quantity
    #   SGD     Singapore Dollar    113878          80196        1.42    0      113878    80196        SGD     113878
    
    #  CustodianAccount   venue  marginType    marginStatement  marginCall
    # CustodianAccount1D    SG   Variation           msp38        mcp38
    #---------------------------------------------------------------------------------------------------------------
    #### Output Format End ########################
    
    #### CHECK WHETHER ASSET POOL IS SUFFICIENT START #######
    suffPerCall <- all(apply(eli_mat*(minUnitQuantity_mat*minUnitValue_mat*(1-haircut_mat)),1,sum) > callAmount_mat[,1])
    suffAllCall <- sum(minUnitQuantity_mat[1,]*minUnitValue_mat[1,]*(1-apply(haircut_mat,2,max)))>sum(callAmount_mat[,1])
    if(!(suffPerCall&suffAllCall)){
      #errorMsg <- 'Error: Asset inventory is insufficient!'
      stop('Asset inventory is insufficient!')
      #return(errorMsg)
    }
    #### CHECK WHETHER ASSET POOL IS SUFFICIENT END ############
    
    #### Calculate the Objectives Parameters Start #############
    #### calculate the cost if only the integral units of asset can be allocated
    integerCallAmount_mat <- ceiling(callAmount_mat/(1-haircut_mat)/minUnitValue_mat)*minUnitValue_mat
    
    cost_mat<-integerCallAmount_mat*costBasis_mat  # cost amount
    costBasis_mat <- costBasis_mat/(1-haircut_mat)
    costBasis_vec <- as.vector(t(costBasis_mat))
    
    assetLiquidity_vec <- apply((1-haircut_mat*eli_mat)^2,2,min) # define asset liquidity
    liquidity_mat <- matrix(rep(assetLiquidity_vec,callNum),nrow=callNum,byrow=TRUE,dimnames=list(callId_vec,resource_vec)) 
    liquidity_vec <- as.vector(t(liquidity_mat))
    
    callCcy <- callInfo_df$currency
    operation_mat <- matrix(rep(1,resourceNum*callNum),nrow=callNum,byrow=TRUE,dimnames=list(callId_vec,resource_vec)) 
    for(i in 1:callNum){
      idxCcy <- which(callCcy[i]==assetId_vec)    # return the index of mc[i] currency cash in the assetId_vec list
      idx1 <- which(eli_mat[i,]!=0)             # return elegible asset idx for mc[i]
      if(length(idxCcy)==1 && is.element(idxCcy,idx1)){  # if there exist call currency cash in the inventory, and it's available
        operation_mat[i,idxCcy] <- 0
      }
    }
    operation_vec <- as.vector(t(operation_mat))
    
    normCost_mat <- cost_mat
    for(i in 1:callNum){
      if(length(unique(cost_mat[i,]))==1){
        normCost_mat[i,]<-1
      }else{
        normCost_mat[i,]<- scale(cost_mat[i,])
        normCost_mat[i,]<- normCost_mat[i,]+(-min(normCost_mat[i,])*2)
      }
    }
    normCost_vec <- as.vector(t(normCost_mat))
    
    normLiquidity_mat <- liquidity_mat
    for(i in 1:callNum){
      if(length(unique(liquidity_mat[i,]))==1){
        normLiquidity_mat[i,]<-1
      }else{
        normLiquidity_mat[i,]<- scale(liquidity_mat[i,])
        normLiquidity_mat[i,]<- normLiquidity_mat[i,]+(-min(normLiquidity_mat[i,])*2)
      }
    }
    normLiquidity_vec <- as.vector(t(normLiquidity_mat))
    
    normLiquidity_vec <- as.vector(t(normLiquidity_mat))
    normOperation_mat <- operation_mat*9+1
    normOperation_vec <- as.vector(t(normOperation_mat))
    #### Calculate the Objectives Parameters END ##############
    
    #### Calculate the Optimal Asset Sufficiency Start #######
    optimal_mat <- normLiquidity_mat*pref_vec[1]+normLiquidity_mat*pref_vec[2]+normCost_mat*pref_vec[3]
    colnames(optimal_mat) <- resource_vec; rownames(optimal_mat)<-callId_vec
    
    optimalAsset_mat <- matrix(c(callId_vec,rep('', callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callId','assetCustacId')))
    
    tempMinUnitQuantity_mat <- minUnitQuantity_mat
    for(i in 1:callNum){
      idx1 <- which(eli_mat[i,]!=0)  # return elegible asset idx for mc[i]
      temp_mat <- matrix(c(optimal_mat[i,idx1],idx1),nrow=2,byrow = T) # combine the asset cost and index together
      # sort the asset per call by cost
      if(length(temp_mat[1,])==1){       # if there's only one eligible asset, no need to sort.
        sortOptimal_mat=temp_mat
      }else{
        sortOptimal_mat<-temp_mat[,order(temp_mat[1,])] # sort the cost, return the cost and asset idx in matrix
      }
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
      idxMinScore_vec <- sortOptimal_mat[2,which(round(sortOptimal_mat[1,],2)==round(min(sortOptimal_mat[1,]),2))]
      # if idxMinScore_vec contains only one element, don't need to sort
      if(length(idxMinScore_vec) > 1){
        optimalResource_vec <- resource_vec[idxMinScore_vec]
        
        # temp.largestAmount.asset: the least score assets score and index(>=1)
        largestAmountResource_vec <- matrix(c(tempMinUnitQuantity_mat[i,idxMinScore_vec]*minUnitValue_mat[i,idxMinScore_vec],idxMinScore_vec),nrow=2,byrow=T)
        if(length(largestAmountResource_vec[1,])>1){  
          largestAmountResource_vec <- largestAmountResource_vec[,order(largestAmountResource_vec[1,],decreasing=T)]
          # substitute in sortOptimal_mat
          sortOptimal_mat[,1:length(largestAmountResource_vec[1,])]<- largestAmountResource_vec
          colnames(sortOptimal_mat)[1:length(largestAmountResource_vec[1,])] <- colnames(largestAmountResource_vec)
        }
      }
      optimalAsset_mat[i,2] <- resource_vec[sortOptimal_mat[2,1]]
      temp.minUnitQuantity <- tempMinUnitQuantity_mat[,sortOptimal_mat[2,1]]
      tempMinUnitQuantity_mat[,sortOptimal_mat[2,1]]<- temp.minUnitQuantity-callAmount_mat[i,1]/(1-haircut_mat[i,1])/minUnitValue_mat[,sortOptimal_mat[2,1]]
      #for(m in 1:length(idxMinScore_vec)){
      #  if(!is.element(temp.optimal.asset[m],optimalAsset_mat[,2])){
      #    optimalAsset_mat[i,2] <- temp.optimal.asset[m]
      #    break
      #  }
      #}
      # if all possible assets have been selected as optimal of previous margin calls
      # then, select the first asset
      if(optimalAsset_mat[i,2]==''){
        optimalAsset_mat[i,2] <- optimalResource_vec[1]
      }
    }
    
    assetSuffQty_mat <- ceiling(callAmount_mat/(1-haircut_mat)/minUnitValue_mat) # quantity needed for a single asset to fulfill each call
    selectUniqueAsset_vec <- unique(optimalAsset_mat[,2]) 
    ifSelectAssetSuff_vec <- rep(0,length(selectUniqueAsset_vec))
    
    for(i in 1:length(selectUniqueAsset_vec)){
      id <- selectUniqueAsset_vec[i]
      idx.temp <- optimalAsset_mat[which(optimalAsset_mat[,2]==id),1] # calls have the least cost assetId_vec=id
      ifSelectAssetSuff_vec[i] <- 1*(sum(assetSuffQty_mat[idx.temp,id]) < max(minUnitQuantity_mat[,id]))
    }
    #### Calculate the Optimal Asset Sufficiency END ##########
    
    #### ALLOCATION START #####################################
    
    if(!is.element(0,ifSelectAssetSuff_vec)){
      #### Optimal Assets are Sufficient Start ##########
      result_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
      for(k in 1:callNum){
        tempCall <- optimalAsset_mat[k,1]
        tempResource <- optimalAsset_mat[k,2]
        idxTempResource <- which(resource_vec==tempResource)
        result_mat[k,idxTempResource] <- assetSuffQty_mat[k,idxTempResource]
      }
      status <- 'solved'
      lpsolveRun <- FALSE
      solverObjValue <- -1
      
      print('solverObjValue');print(solverObjValue)
      
      #### Optimal Assets are Sufficient END #############
    } else if(1){
      
      #### Optimal Assets are not Sufficient Start #########
      lpsolveRun<-TRUE
      
      #### Construct Variable Names Start ######
      varInfo_list <- VarInfo(eli_vec,callInfo_df,resource_vec,callId_vec)
      
      varName_vec <- varInfo_list$varName_vec
      varNum <- varInfo_list$varNum
      varNum2 <- varInfo_list$varNum2
      varNum3 <- varInfo_list$varNum3
      msVar_mat <- varInfo_list$msVar_mat
      idxEli_vec <- which(eli_vec==1)  
      #### Construct Variable Names END ########
      cat('varNum3 ',varNum3)
      cat('varName_vec ',varName_vec)
      
      #### MODEL SETUP Start ##################################################
      # decision variables: x, qunatity used of each asset for each margin call
      #                 quantity or minUnitQuantity
      # 
      # objective function: fObj_vec, minimize  x*value*cost
      # 
      # constraints: A*x (direction) b
      # A-- constraint matrix: f.con;
      # b-- constraint value: f.rhs;
      # direction -- constraint direction: f.dir.
      #
      # Constraints are specified below:
      # 0. quantity used of an asset should be a non-negative value
      #    quantity used >= 0
      # 1. quantity limit of each asset for one margin call (callNum*resourceNum)
      #    quantity used <= quantity limit; (quantity or minUnitQuantity)
      # 2. quantity limit of each asset for all margin calls(resourceNum)
      #    total quantity used <= total quantity (for an asset) (quantity or minUnitQuantity)
      # 3. margin call requirement (callNum)
      #    total net amount of assets for one margin call >= call amount
      # 4.& 5. movements
      #    Similating the dummy of each x
      # 6. in same margin statement
      #   
      # 7. constraint on the asset movements(operLimit)
      #
      # variable bounds: a < x < x_quantity
      #    specified by constraint 0 and 1. 
      # variable kind: semi-continuous, value below 'a' will automately set to 0
      #
      #### MODEL SETUP END ####################################################
      
      #### Build the Optimization Model ########
      # objective function
      operationTemp_vec <- normOperation_vec[idxEli_vec]
      operationObj_vec <-  c(rep(0,varNum),operationTemp_vec*max(callAmount_mat)*10,-operationTemp_vec[msVar_mat[,1]-varNum]*max(callAmount_mat)*10)
      liquidityObj_vec <-  c(minUnitValue_vec[idxEli_vec]*normLiquidity_vec[idxEli_vec],rep(0,varNum3-varNum))
      costObj_vec <-  c(minUnitValue_vec[idxEli_vec]*costBasis_vec[idxEli_vec],rep(0,varNum3-varNum))
      fObj_vec <- operationObj_vec*pref_vec[1]+liquidityObj_vec*pref_vec[2]+costObj_vec*pref_vec[3]
      
      cat('fObj_vec length: ',length(fObj_vec))
      
      names(fObj_vec) <- varName_vec
      
      # constraints
      fCon0_mat <- matrix(0,nrow=varNum,ncol=varNum3)
      fCon0_mat[cbind(1:varNum,1:varNum)] <- 1
      fDir0_vec <- rep('>=',varNum)
      fRhs0_vec <- rep(0,varNum)
      
      fCon1_mat <- matrix(0,nrow=varNum,ncol=varNum3)
      fCon1_mat[cbind(1:varNum,1:varNum)] <- 1
      fDir1_vec <- rep('<=',varNum)
      fRhs1_vec <- c(eli_vec[idxEli_vec]*minUnitQuantity_vec[idxEli_vec],rep(1,varNum))
      
      fCon2_mat <- matrix(0,nrow=resourceNum,ncol=varNum)
      fConTemp_mat <- matrix(0,nrow=resourceNum,ncol=varNum3-varNum2)
      temp1 <- 1+(0:(callNum-1))*resourceNum
      idxCon2_vec <- rep(temp1,resourceNum)+rep(c(0:(resourceNum-1)),rep(callNum,resourceNum))
      idxCon2_vec <- match(idxCon2_vec,idxEli_vec)
      fCon2_mat[na.omit(cbind(rep(c(1:resourceNum),rep(callNum,resourceNum)),idxCon2_vec))]<-1
      fCon2_mat <- cbind(fCon2_mat,fCon2_mat*0,fConTemp_mat)
      fDir2_vec <- rep('<=',resourceNum)
      fRhs2_vec <- minUnitQuantity_mat[1,]
      
      fCon3_mat <- matrix(0,nrow=callNum,ncol=varNum)
      fConTemp_mat <- matrix(0,nrow=callNum,ncol=varNum3-varNum2)
      idxCon3_vec <- 1:(resourceNum*callNum)
      idxCon3_vec <- match(idxCon3_vec,idxEli_vec)
      fCon3_mat[na.omit(cbind(rep(c(1:callNum),rep(resourceNum,callNum)),idxCon3_vec))] <- minUnitValue_vec[idxEli_vec]*(1-haircut_vec[idxEli_vec])
      fCon3_mat <- cbind(fCon3_mat,fCon3_mat*0,fConTemp_mat)
      fDir3_vec <- rep('>=',callNum)
      fRhs3_vec <- callAmount_mat[,1]
      
      fCon4_mat <- matrix(0,nrow=varNum,ncol=varNum)
      fConTemp_mat <- matrix(0,nrow=varNum,ncol=varNum3-varNum2)
      fCon4_mat[cbind(1:varNum,1:varNum)] <- 1
      # use the margin amount instead of a static large number
      scaleFactor_vec <- t(callAmount_vec)[idxEli_vec]*200
      fCon4_mat <- cbind(fCon4_mat,fCon4_mat*(-scaleFactor_vec),fConTemp_mat)
      fDir4_vec <- rep('<=',varNum)
      fRhs4_vec <- rep(0,varNum)
      
      fCon5_mat <- matrix(0,nrow=varNum,ncol=varNum)
      fConTemp_mat <- matrix(0,nrow=varNum,ncol=varNum3-varNum2)
      fCon5_mat[cbind(1:varNum,1:varNum)] <- 1
      fCon5_mat <- cbind(fCon5_mat,-fCon5_mat,fConTemp_mat)
      fDir5_vec <- rep('>=',varNum)
      fRhs5_vec <- rep(0,varNum)
      
      if(varNum3>varNum2){
        fCon6_mat <- matrix(0,nrow=varNum3-varNum2,ncol=varNum3)
        fCon6_mat[cbind(1:(varNum3-varNum2),msVar_mat[,1])] <- 1
        fCon6_mat[cbind(1:(varNum3-varNum2),msVar_mat[,2])] <- 1
        fCon6_mat[cbind(1:(varNum3-varNum2),msVar_mat[,3])] <- -2
        fDir6_vec <- rep(">=",varNum3-varNum2)
        fRhs6_vec <- rep(0,varNum3-varNum2)
      }
      
      # minimum movement quantity of each asset
      minMoveQuantity_vec <- ceiling(minMoveValue/minUnitValue_vec[idxEli_vec])
      if(length(callAmount_vec[which(minMoveValue > callAmount_vec[idxEli_vec]/(1-haircut_vec[idxEli_vec]))])!=0){
        idxTemp <- which(minMoveValue > callAmount_vec[idxEli_vec]/(1-haircut_vec[idxEli_vec]))
        call.eli_vec <- callAmount_vec[idxEli_vec]/(1-haircut_vec[idxEli_vec])
        minUnitValue.eli_vec <- minUnitValue_vec[idxEli_vec]
        minMoveQuantity_vec[idxTemp] <- ceiling(call.eli_vec[idxTemp]/minUnitValue.eli_vec[idxTemp])
      }
      
      #### Optimization Model END ##############
      
      #### Solver Inputs Start #################
      lpObj_vec <- fObj_vec
      if(varNum3>varNum2){
        lpCon_mat <- rbind(fCon2_mat,fCon3_mat,fCon4_mat,fCon5_mat,fCon6_mat)
        lpDir_vec <- c(fDir2_vec,fDir3_vec,fDir4_vec,fDir5_vec,fDir6_vec)
        lpRhs_vec <- c(fRhs2_vec,fRhs3_vec,fRhs4_vec,fRhs5_vec,fRhs6_vec)
      } else{
        lpCon_mat <- rbind(fCon2_mat,fCon3_mat,fCon4_mat,fCon5_mat)
        lpDir_vec <- c(fDir2_vec,fDir3_vec,fDir4_vec,fDir5_vec)
        lpRhs_vec <- c(fRhs2_vec,fRhs3_vec,fRhs4_vec,fRhs5_vec)      
      }
      
      lpKind_vec <- rep('semi-continuous',varNum3)
      lpType_vec <- rep('real',varNum3)
      lpType_vec[which(minUnitValue_vec[idxEli_vec]>=1)] <- 'integer'
      lpType_vec[(varNum+1):varNum3] <- 'integer'
      lpLowerBound_vec <- c(minMoveQuantity_vec,rep(1,varNum3-varNum))
      lpUpperBound_vec <- c(minUnitQuantity_vec[idxEli_vec],rep(1,varNum3-varNum))
      lpBranchMode_vec <- c(rep('floor',varNum),rep('auto',varNum3-varNum))
      
      lpPresolve <- ifelse(callNum<=5,'none','knapsack')
      lpEpsd <- 1e-11
      lpTimeout <- timeLimit
      #lpVerbose <- 'normal'
      # bbRule <-  c("pseudononint", "restart","autoorder","stronginit", "dynamic","rcostfixing")
      bbRule <- c("pseudononint", "greedy", "dynamic","rcostfixing") # default
      
      #### INITIAL GUESS BASIS
      lpGuessBasis_vec <- rep(0,varNum3)
      if(!missing(initAllocation_list)){
        # the initial guess must be a feasible point
        lpGuessBasis_vec<-callList2Var(initAllocation_list,callId_vec,minUnit_vec,varName_vec,varNum3,varNum,idxEli_vec)
      }
      
      #### Solver Inputs END ###################
      
      #### Solve the Model Start ###############
      #### Call lpSolve Solver
      cat('\n','lpGuessBasis_vec', lpGuessBasis_vec)
      
      solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                                       lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                                       lpGuessBasis_vec=lpGuessBasis_vec,
                                       presolve=lpPresolve,epsd=lpEpsd,timeout=lpTimeout,verbose=verbose,bb.rule=bbRule)
      
      #### Solver Outputs
      status<- solverOutput_list$resultStatus
      solverSolution_vec <- solverOutput_list$solverSolution_vec
      solverObjValue <- solverOutput_list$solverObjValue
      
      cat('solverObjValue: ',solverObjValue)
      # round up the decimal quantity to the nearest integer.
      # if it's larger than 0.5
      # if close to 0, then set both real and dummies to 0, and if this action causes the 
      # the insufficiency of the total amount, make it up at the checking module
      # not only update result_mat but also the original solverSolution_vec
      
      solNum1_vec <- solverSolution_vec[1:varNum]
      solNum2_vec <- solverSolution_vec[(varNum+1):varNum2]
      
      # Rounding
      solNum1_vec[which(solNum1_vec >= 0.5)] <- ceiling(solNum1_vec[which(solNum1_vec >= 0.5)])
      solNum1_vec[which(solNum1_vec < 0.5)] <- 0
      
      solNum2_vec <- 1*(solNum1_vec & 1) # recalculate the dummy value
      
      # substitute
      solverSolution_vec[1:varNum] <- solNum1_vec 
      solverSolution_vec[(varNum+1):varNum2] <- solNum2_vec
      
      if(varNum3>varNum2){
        idxTemp1_vec <- msVar_mat[,1]
        idxTemp2_vec <- msVar_mat[,2]
        solNum3_vec <- 1*(solverSolution_vec[idxTemp1_vec] & solverSolution_vec[idxTemp2_vec])
        solverSolution_vec[(varNum2+1):varNum3] <- solNum3_vec
      }
      
      result_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
      result_mat <- t(result_mat); resultDummy_mat <- result_mat
      result_mat[idxEli_vec]<-solverSolution_vec[1:varNum]
      resultDummy_mat[idxEli_vec]<- solverSolution_vec[(varNum+1):varNum2]
      result_mat[which(result_mat>0.5)] <- ceiling(result_mat[which(result_mat>0.5)])
      result_mat <- t(result_mat) ;   resultDummy_mat <- t(resultDummy_mat)     # convert solution into matrix format
      #print('result_mat: '); print(result_mat)
      #print('resultDummy_mat: '); print(resultDummy_mat)
      
      #### Solve the Model END #################
      
      
      #### CHECK ALLOCATION RESULT Start#############
      # STATUS: Developing
      #
      # 1. whether all variables are non-negative
      idxNeg_vec <- which(result_mat<0)
      if(length(idxNeg_vec)>=1){
        result_mat[idxNeg_vec] <-0 # set to 0 first, then check the other two criteria
      }
      
      # 2. whether statisfy the quantity limits
      assetQuantityUsed_vec <- apply(result_mat,2,sum)
      assetQuantityLeft_vec <- minUnitQuantity_mat[1,]-assetQuantityUsed_vec
      idxExcess_vec <- which(assetQuantityUsed_vec>minUnitQuantity_mat[1,])
      if(length(idxExcess_vec)>=1){
        for(i in idxExcess_vec){          # i: the index of the excess quantity asset in assetId_vec
          currentAllocation_mat <- matrix(c(which(result_mat[,i]>0),result_mat[which(result_mat[,i]>0),i]),nrow=2,byrow=T)
          if(length(currentAllocation_mat[1,])>1){
            currentAllocation_mat<-currentAllocation_mat[,order(currentAllocation_mat[2,])]
          }
          for(k in 1:length(currentAllocation_mat[1,])){ # k: the kth margin call which asset[i] allocated to
            j = currentAllocation_mat[1,k]  # j: the index of the the kth margin call in callId_vec
            # current allocated quantity < excess quanity
            if(currentAllocation_mat[2,k]< (-assetQuantityLeft_vec[i])){
              # the amount missing for the margin call j if excluding the asset i
              newQuantity <- 0
              otherAmount <- sum(result_mat[j,1+which(result_mat[j,-i]>0)]*minUnitValue_mat[j,1+which(result_mat[j,-i]>0)]*(1-haircut_mat[j,1+which(result_mat[j,-i]>0)]))
              missingAmount <- callAmount_mat[j,1]-(otherAmount+newQuantity/(1-haircut_mat[j,i])/minUnitValue_mat[j,i])
              # missingAmount<0, means even we substract the exceed quantity of the asset, 
              # the sub-total is still larger than call amount, then, we update asset to the 
              # least quantity(already 0) which can meet the margin call requirement, no swaps occur
              if(missingAmount<=0){
                result_mat[j,i]<- newQuantity
                
                assetQuantityUsed_vec <- apply(result_mat,2,sum)
                assetQuantityLeft_vec <- minUnitQuantity_mat[1,]-assetQuantityUsed_vec
                break
              }
              # first check whether the other previous allocated assets are sufficient,based on the operation efficiency
              # find the other asset which is sufficient and eligible for margin call j
              
              missingQuantity_vec <- ceiling((missingAmount/(1-haircut_mat)/minUnitValue_mat)[j,])
              idxSuff_vec <- intersect(which(missingQuantity_vec<=assetQuantityLeft_vec),which(eli_mat[j,]==1))
              
              # whether there are other assets allocated to call j
              idxSwapProb_vec <- intersect(which(result_mat[j,]>0),idxSuff_vec)
              if(length(idxSwapProb_vec)>=1){
                idxSwapNew <- idxSwapProb_vec[1]
              }else{
                idxSwapNew <- idxSuff_vec[1]
              }
              swapNewQuantity <- missingQuantity_vec[idxSwapNew]+result_mat[j,idxSwapNew]
              newAllocation_mat <- matrix(currentAllocation_mat[,-which(currentAllocation_mat[1,]==j)],nrow=2)
              
              if(length(which(result_mat[,idxSwapNew]>0))){
                swapAllocation_mat<- matrix(c(which(result_mat[,idxSwapNew]>0),result_mat[which(result_mat[,idxSwapNew]>0),idxSwapNew]),nrow=2,byrow=T)
                swapAllocation_mat[2,which(swapAllocation_mat[1,]==j)] <- swapNewQuantity
              }else{
                swapAllocation_mat<- matrix(c(idxSwapNew,swapNewQuantity),nrow=2)
              }
              # update the result_mat
              result_mat[j,c(i,idxSwapNew)]<- c(newQuantity,swapNewQuantity)
              
              assetQuantityUsed_vec <- apply(result_mat,2,sum)
              assetQuantityLeft_vec <- minUnitQuantity_mat[1,]-assetQuantityUsed_vec
            }
            else{
              # the amount missing for the margin call j if excluding the asset i
              # shouldn't exclude the asset i, just reduce to the sufficient amount, and use other assets to fulfil the left call amount
              newQuantity<- currentAllocation_mat[2,which(currentAllocation_mat[1,]==j)]+assetQuantityLeft_vec[i]
              
              # if this asset is the only selection
              if(callNum==1){
                otherAmount <- sum(result_mat[,-i][which(result_mat[-i]>0)]*minUnitValue_mat[,-i][which(result_mat[-i]>0)]*
                                     (1-haircut_mat[,-i][which(result_mat[-i]>0)]))
              } else{
                otherAmount <- sum(result_mat[,-i][j,which(result_mat[j,-i]>0)]*minUnitValue_mat[,-i][j,which(result_mat[j,-i]>0)]*
                                     (1-haircut_mat[,-i][j,which(result_mat[j,-i]>0)]))
              }
              missingAmount <- callAmount_mat[j,1]-(otherAmount+newQuantity*minUnitValue_mat[j,i]*(1-haircut_mat[j,i]))
              # missingAmount<0, means even we substract the exceed quantity of the asset, 
              # the sub-total is still larger than call amount, then, we update asset to the 
              # least quantity which can meet the margin call requirement, no swaps occur
              if(missingAmount<=0){
                newQuantity <-  ceiling((callAmount_mat[j,1]-otherAmount)/minUnitValue_mat[j,i]/(1-haircut_mat[j,i]))
                result_mat[j,i]<- newQuantity
                assetQuantityUsed_vec <- apply(result_mat,2,sum)
                assetQuantityLeft_vec <- minUnitQuantity_mat[1,]-assetQuantityUsed_vec
                break
              }
              
              # first check whether the other previous allocated assets are sufficient,based on the operation efficiency
              # find the other asset which is sufficient and eligible for margin call j
              missingQuantity_vec <- ceiling((missingAmount/(1-haircut_mat)/minUnitValue_mat)[j,])
              idxSuff_vec <- intersect(which(missingQuantity_vec<=assetQuantityLeft_vec),which(eli_mat[j,]==1))
              
              if(length(idxSuff_vec)==0){
                # sacrifice the fulfilled call amount if the it is still larger than the shreshod
                if((callAmount_mat[j,1]-missingAmount)>=callInfo_df$callAmount[j]){
                  result_mat[j,i]<- newQuantity
                }
                # left quantity of each available asset for this call is not sufficient
                # need more than one assets to allocate to this call
                # compare the missing amount and the sum of the left asset left amount
                # asset.amount.left <- matrix(c(1:resourceNum,assetQuantityLeft_vec*minUnitValue_mat[j,]),nrow=2,byrow=T)
                
                # there should be more than one assets available(else will be detected in the pre-check sufficiency part)
                # order by amount from larger to smaller, make sure the least movements
                # asset.amount.left <- asset.amount.left[,order(asset.amount.left[2,])]
                
                # the index of available assets, excluding the 
                # idxTemp <- intersect(which(assetQuantityLeft_vec>0),which(eli_mat[j,]==1))
              } else{
                # whether there are other assets allocated to call j
                idxSwapProb_vec <- intersect(which(result_mat[j,]>0),idxSuff_vec)
                if(length(idxSwapProb_vec)>=1){
                  idxSwapNew <- idxSwapProb_vec[1]
                } else{
                  idxSwapNew <- idxSuff_vec[1]
                }
                swapNewQuantity <- missingQuantity_vec[idxSwapNew]+result_mat[j,idxSwapNew]
                
                newAllocation_mat <- currentAllocation_mat
                newAllocation_mat[,-which(currentAllocation_mat[1,]==j)] <- newQuantity
                
                if(length(which(result_mat[,idxSwapNew]>0))){
                  swapAllocation_mat<- matrix(c(which(result_mat[,idxSwapNew]>0),result_mat[which(result_mat[,idxSwapNew]>0),idxSwapNew]),nrow=2,byrow=T)
                  swapAllocation_mat[2,which(swapAllocation_mat[1,]==j)] <- swapNewQuantity
                }else{
                  swapAllocation_mat<- matrix(c(idxSwapNew,swapNewQuantity),nrow=2)
                }
                
                # update the result_mat
                result_mat[j,c(i,idxSwapNew)]<- c(newQuantity,swapNewQuantity)
              }
              
              assetQuantityUsed_vec <- apply(result_mat,2,sum)
              assetQuantityLeft_vec <- minUnitQuantity_mat[1,]-assetQuantityUsed_vec
              break
            }
          } 
        }
      }
      
      # 3. whether meet all margin call requirements
      assetQuantityUsed_vec <- apply(result_mat,2,sum)
      assetQuantityLeft_vec <- minUnitQuantity_mat[1,]-assetQuantityUsed_vec
      # compare with the call amount, not the custimized amount based on the user preference
      callFulfilled_vec <- apply(result_mat*minUnitValue_mat*(1-haircut_mat),1,sum)
      callMissingAmount_vec <- callInfo_df$callAmount-callFulfilled_vec
      idxCallMissing_vec <- which(callMissingAmount_vec>0)
      if(length(idxCallMissing_vec)>=1){
        for(i in idxCallMissing_vec){
          
          currentAllocation_mat <- matrix(c(which(result_mat[i,]>0),result_mat[i,which(result_mat[i,]>0)]),nrow=2,byrow=T)
          
          missingAmount <- callMissingAmount_vec[i]
          missingQuantity_vec <- ceiling((missingAmount/(1-haircut_mat)/minUnitValue_mat)[i,])
          idxSuff_vec <- intersect(which(missingQuantity_vec<=assetQuantityLeft_vec),which(eli_mat[i,]==1))
          if(length(idxSuff_vec)==0){
            # which means none of the asset itself is enough to to fulfill the left amount of the margin call
            # This should be a very extreme case, and it's more complicated to develop for this case
            # so, I will leave here blank, once I'm done the rest part I'll return to check
            # Also, the exception handling will be a long-run development, and it will be raised once we have exception
          }
          
          # whether there are assets which are sufficient allocated to call i
          idxCurrentProb_vec <- intersect(idxSuff_vec,currentAllocation_mat[1,])
          if(length(idxCurrentProb_vec)==0){
            idxCurrentProb_vec<- idxSuff_vec
          }
          idxAddNew <- idxCurrentProb_vec[1]
          addNewQuantity <- missingQuantity_vec[idxAddNew]+result_mat[i,idxAddNew]
          result_mat[i,idxAddNew] <- addNewQuantity
        }
      }
      #### CHECK ALLOCATION RESULT END ################
    } # else if END
    
    #### Result Analysis Start ########################
    #### calculate the daily cost & month cost
    costDaily <- sum(result_mat * costBasis_mat * minUnit_mat)
    costMonthly <- costDaily*30
    #### calculate the operational movements 
    
    #### Result Analysis END ##########################
    
    #### Prepare Outputs Start #######################
    resultAnalysis_list <- list(costDaily=costDaily,costMonthly=costMonthly)
    #### Convert the result_mat to List
    result_list <- Result2callList(result_mat,assetId_vec,availAsset_df,coreInput_list,callSelect_list,msSelect_list)
    
    callSelect_list <- result_list$callSelect_list
    msSelect_list <- result_list$msSelect_list
    
    subtotalFulfilled_mat<- matrix(c(coreInput_list$callAmount_mat[,1],rep(0, callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
    for(i in 1:callNum){
      subtotalFulfilled_mat[i,2] <- sum(callSelect_list[[callId_vec[i]]]$`NetAmount(USD)`)
    }
    checkCall_mat <- subtotalFulfilled_mat
    #### Prepare Outputs END ########################
    resultGroup_list <-list(#msOutput_list=msSelect_list,
      callOutput_list=callSelect_list,checkCall_mat=checkCall_mat,availAsset_df=availAsset_df,
      status=status,lpsolveRun=lpsolveRun,solverObjValue=solverObjValue,resultAnalysis_list=resultAnalysis_list)
    
    
    print('resultGroup_list:');print(resultGroup_list)
  } else if(algoVersion==2){
    resultGroup_list <- CoreAlgoV2(coreInput_list,availAssetGroup_df,timeLimit,pref_vec,operLimit,minMoveValue,verbose,initAllocation_list)
  }
  #### Run CoreAlgo END ########################
  
  #msOutputGroup_list <- resultGroup_list$msOutput_list
  callOutputGroup_list <- resultGroup_list$callOutput_list
  
  status <- resultGroup_list$status
  lpsolveRun <- resultGroup_list$lpsolveRun
  solverObjValue <- round(resultGroup_list$solverObjValue,2)
  checkCallGroup_mat <- resultGroup_list$checkCall_mat
  resultAnalysis_list <- resultGroup_list$resultAnalysis_list
  costDaily <- resultAnalysis_list$costDaily + costDaily
  costMonthly <- resultAnalysis_list$costMonthly + costMonthly
  
  # update the availAsset 
  availAssetGroup_df <- resultGroup_list$availAsset_df
  availAsset_df[which(availAsset_df$callId %in% callIdGroup_vec),] <- availAssetGroup_df
  
  for(k in 1:length(callIdGroup_vec)){
    callId <- callIdGroup_vec[k]
    j <- which(msIdGroup_vec==callInfo_df$marginStatement[which(callInfo_df$id==callId)])
    
    msId <- msId_vec[j]
    callOutput_list[[callId]] <- callOutputGroup_list[[callId]]
    # msOutput_list[[msId]] <- msOutputGroup_list[[msId]]
    checkCall_mat[which(rownames(checkCall_mat)==callId),2] <- checkCallGroup_mat[which(rownames(checkCallGroup_mat)==callId),2]
  }
}

costDaily <- round(costDaily,2)
costMonthly <- round(costMonthly,2)
cat('costDaily ',costDaily); cat('costMonthly',costMonthly)
resultAnalysis <- list(costDaily=costDaily,costMonthly=costMonthly)
############ ITERATE THE GROUP, RUN THE ALGO END #########################

result <- list(#msOutput=msOutput_list,
  callOutput=callOutput_list,checkCall_mat=checkCall_mat,
  status=status,lpsolveRun=lpsolveRun,solverObjValue=solverObjValue)

print(result)



