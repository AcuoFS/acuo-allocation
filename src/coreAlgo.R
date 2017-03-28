
CoreAlgo <- function(coreInput_list,availAsset_df,timeLimit,pref_vec){
  pref_vec<-pref_vec
  callId_vec<-coreInput_list$callId_vec
  resource_vec<-coreInput_list$resource_vec
  #assetId_vec <- as.character(data.frame(strsplit(resource_vec,'-'))[1,])
  assetId_vec <- matrix(unlist(strsplit(resource_vec,'-')),nrow=2)[1,]
  
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
  
  #amount.mat <- unitValue_mat*quantity_mat; amount.vec <- unitValue_vec*quantity_vec     # amount of asset
  
  callAmount_mat <- coreInput_list$callAmount_mat; callAmount_vec <- as.vector(t(callAmount_mat))             # margin call amount mat
  
  costBasis_mat <- coreInput_list$cost_mat; cost_vec <- coreInput_list$cost_vec        # cost mat & vec
  
  ############### CONSTANTS DEFINED INSIDE THE ALGO ###################
  minMoveValue <- 1000
  
  ############### Output Format #######################################
  callOutput_list <- list()
  msOutput_list <- list()
  
  # A list, each element is the allocation result(dataframe) for one margin call
  #------------------------------------------------------------------------------------------------------------------------------------------
  # $callId1
  #  Asset(assetId_vec)   Name(assetName)   NetAmount(USD)(afterHaircut)    Amount      currency    quantity        value       custodianAccount
  # 1  a1              asset1               numeric value           numeric value    CCY1    numeric value  numeric value     custac1
  # 2  a2              asset2               numeric value           numeric value    CCY2    numeric value  numeric value     custac2
  # 
  # $callId2
  #  Asset(assetId_vec)   Name(assetName)   NetAmount(USD)(afterHaircut)   Amount      currency    quantity        value       custodianAccount
  # 1  a2              asset2               numeric value          numeric value     CCY2    numeric value  numeric value     custac2
  # 2  a3              asset3               numeric value          numeric value     CCY1    numeric value  numeric value     custac1
  # 3  a4              asset4               numeric value          numeric value     CCY3    numeric value  numeric value     custac3
  #------------------------------------------------------------------------------------------------------------------------------------------
  
  ######### CHECK WHETHER ASSET POOL IS SUFFICIENT #############
  suffPerCall <- all(apply(eli_mat*(minUnitQuantity_mat*minUnitValue_mat*(1-haircut_mat)),1,sum) > callAmount_mat[,1])
  suffAllCall <- sum(minUnitQuantity_mat[1,]*minUnitValue_mat[1,]*(1-apply(haircut_mat,2,max)))>sum(callAmount_mat[,1])
  if(!(suffPerCall&suffAllCall)){
    #errorMsg <- 'Error: Asset inventory is insufficient!'
    stop('Asset inventory is insufficient!')
    #return(errorMsg)
  }
  
  #### Recalculate the parameters weight setting
  pref_vec <- pref_vec/sum(pref_vec)
  
  #### calculate the optimal asset sufficiency
  #reserve_list <-list()    # store all available assets for each call, list by callId_vec
  callSelect_list  <- list()    # store selected assets for each call, list by callId_vec
  msSelect_list <- list()   # store selected assets for each margin statement, list by msId
  
  optimalAsset_mat <- matrix(c(callId_vec,rep('', callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callId','assetCustacId')))
  
  callCcy <- callInfo_df$currency
  excellCallPercent <- 0.2
  callAmount_mat <- callAmount_mat*(1+excellCallPercent*pref_vec[1])
  callAmount_vec <- callAmount_vec*(1+excellCallPercent*pref_vec[1])
  
  # calculate the cost if only the integral units of asset can be allocated
  integer.callAmount_mat <- ceiling(callAmount_mat/(1-haircut_mat)/minUnitValue_mat)*minUnitValue_mat*(1-haircut_mat)
  
  cost_mat<-integer.callAmount_mat/(1-haircut_mat)*costBasis_mat  # cost amount
  
  assetLiquidity_vec <- apply((1-haircut_mat*eli_mat)^2,2,min) # define asset liquidity
  liquidity_mat <- matrix(rep(assetLiquidity_vec,callNum),nrow=callNum,byrow=TRUE,dimnames=list(callId_vec,resource_vec)) 
  liquidity_vec <- as.vector(t(liquidity_mat))
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
  normOperation_mat <- operation_mat*9+1
  normOperation_vec <- as.vector(t(normOperation_mat))
  
  optimal_mat <- normOperation_mat*pref_vec[1]+normLiquidity_mat*pref_vec[2]+normCost_mat*pref_vec[3]
  colnames(optimal_mat) <- resource_vec; rownames(optimal_mat)<-callId_vec
  
  
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
    #reserve_list[[callId_vec[i]]]<- assetId_vec[sortOptimal_mat[2,]] 
    
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
  
  optimalAssetSuffQty_vec <- callAmount_mat/(1-haircut_mat)/minUnitValue_mat # quantity needed for a single asset to fulfill each call
  selectUniqueAsset_vec <- unique(optimalAsset_mat[,2]) 
  ifSelectAssetSuff_vec <- rep(0,length(selectUniqueAsset_vec))
  for(i in 1:length(selectUniqueAsset_vec)){
    id <- selectUniqueAsset_vec[i]
    idx.temp <- optimalAsset_mat[which(optimalAsset_mat[,2]==id),1] # calls have the least cost assetId_vec=id
    ifSelectAssetSuff_vec[i] <- 1*(sum(optimalAssetSuffQty_vec[idx.temp,id]) < minUnitQuantity_mat[1,id])
  }
  
  #### In case of OW-291, optimal assets are sufficient
  if(!is.element(0,ifSelectAssetSuff_vec)){
    status <- 'solved'
    lpsolveRun <- FALSE
    msId_vec <- unique(callInfo_df$marginStatement)
    for(i in 1:callNum){
      # j, corresponding index of margin statement
      j <- which(msId_vec==callInfo_df$marginStatement[which(callInfo_df$id==callId_vec[i])])
      idxSelectResource_vec <- which(resource_vec==optimalAsset_mat[i,2])
      selectAssetId_vec <- assetId_vec[idxSelectResource_vec]
      idxSelectAsset_vec <- rep(0,length(idxSelectResource_vec))
      for(m in 1:length(idxSelectResource_vec)){
        idxSelectAsset_vec[m] <- which(assetInfo_df$id==selectAssetId_vec[m])[1]
      }
      selectAssetCustodianAccount_vec <- custodianAccount[idxSelectResource_vec]
      selectAssetVenue_vec <- venue[idxSelectResource_vec]
      selectAssetName_vec <- assetInfo_df$name[idxSelectAsset_vec]
      selectAssetNetAmountUSD_vec <- integer.callAmount_mat[i,1]
      selectAssetHaircut_vec <- haircut_mat[i,idxSelectResource_vec]
      selectAssetAmountUSD_vec <- selectAssetNetAmountUSD_vec/(1-haircut_mat[i,idxSelectResource_vec])
      selectAssetCurrency_vec <- assetInfo_df$currency[idxSelectAsset_vec]
      selectAssetMinUnitQuantity_vec <- selectAssetAmountUSD_vec/minUnitValue_mat[i,idxSelectResource_vec]
      selectAssetQuantity_vec <- selectAssetMinUnitQuantity_vec*minUnit_mat[i,idxSelectResource_vec]
      selectMarginType_vec <- rep(callInfo_df$marginType[i],length(idxSelectResource_vec))
      selectMs_vec <- rep(callInfo_df$marginStatement[i],length(idxSelectResource_vec))
      selectCall_vec <- rep(callId_vec[i],length(idxSelectResource_vec))
      selectAssetFX_vec <- assetInfo_df$FXRate[idxSelectAsset_vec]
      selectAssetAmount_vec <- selectAssetAmountUSD_vec*selectAssetFX_vec
      selectAssetNetAmount_vec <- selectAssetNetAmountUSD_vec*selectAssetFX_vec
      
      #### UPDATE THE ASSET QUANTITY ########
      tempAvailQuantity_vec <- availAsset_df$quantity[which(availAsset_df$assetCustacId==idxSelectResource_vec)]
      tempQuantity_vec <- availAsset_df$totalQuantity[which(availAsset_df$assetCustacId==idxSelectResource_vec)]
      availAsset_df$quantity[which(availAsset_df$assetCustacId==idxSelectResource_vec)]<- tempAvailQuantity_vec-selectAssetQuantity_vec
      availAsset_df$totalQuantity[which(availAsset_df$assetCustacId==idxSelectResource_vec)]<- tempQuantity_vec-selectAssetQuantity_vec
      #### END ##############################
      
      
      selectAsset_df <- data.frame(selectAssetId_vec,selectAssetName_vec,selectAssetNetAmount_vec,selectAssetNetAmountUSD_vec,selectAssetFX_vec,selectAssetHaircut_vec,selectAssetAmount_vec,selectAssetAmountUSD_vec,selectAssetCurrency_vec,
                                   selectAssetQuantity_vec,selectAssetCustodianAccount_vec,selectAssetVenue_vec,selectMarginType_vec,selectMs_vec,selectCall_vec)
      colnames(selectAsset_df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType','marginStatement','marginCall')
      
      callSelect_list[[callId_vec[i]]] <- selectAsset_df
      if(is.null(msSelect_list[[msId_vec[j]]])){
        msSelect_list[[msId_vec[j]]] <- selectAsset_df
      } else{
        tempAsset_df <- msSelect_list[[msId_vec[j]]]
        selectAsset_df <- rbind(selectAsset_df,tempAsset_df)
        msSelect_list[[msId_vec[j]]] <- selectAsset_df
      }
      
    }
    callOutput_list<- callSelect_list
    msOutput_list <- msSelect_list
  } else if(1){
    lpsolveRun<-TRUE
    
    ##### In case of OW-292, consider all preference, with quantity limits ##########
    
    # matrix store the index number, by row
    idx_mat <- matrix(1:length(base_mat),nrow=callNum,byrow = TRUE,dimnames = dimnames(base_mat))
    # matrix store the variable name("msId_mcId_assetCustId"), by row
    fullNameOri_mat <-  matrix('',nrow=callNum,ncol=resourceNum,byrow = TRUE,dimnames = dimnames(base_mat))
    for(i in 1:callNum){
      msId <- callInfo_df$marginStatement[i]
      fullNameOri_mat[i,]<-paste(msId,callId_vec[i],resource_vec,sep='_')
    }
    fullNameDummy_mat <- fullNameOri_mat
    fullNameDummy_mat[] <- paste(fullNameOri_mat,'dummy',sep='-')
    
    fullNameAll_mat <- rbind(fullNameOri_mat,fullNameDummy_mat)
    
    #splitName_mat <- data.matrix(t(data.frame(strsplit(t(fullNameAll_mat),'_'))))		305	    splitName_mat <- data.matrix(t(data.frame(strsplit(t(fullNameAll_mat),'_'))))
    splitName_mat <- matrix(unlist(strsplit(t(fullNameAll_mat),'_')),ncol=3,byrow=TRUE)
    splitName_mat <- cbind(splitName_mat,1:(2*length(base_mat)))
    colnames(splitName_mat) <- c('msId','callId','assetCustId','index'); rownames(splitName_mat)<- 1:(2*length(base_mat))
    
    ## filter the eligible/available asset variables
    idxEli_vec <- which(eli_vec==1)  
    ## variable numbers
    varNum <- length(idxEli_vec)    # variable numbers
    varNum2 <- varNum*2
    
    VarName_vec <- c(t(fullNameOri_mat)[idxEli_vec],t(fullNameDummy_mat[idxEli_vec]))
    varSplitName_mat <- rbind(splitName_mat[idxEli_vec,],splitName_mat[length(base_mat)+idxEli_vec,])
    
    # update the variable index
    varSplitName_mat[,4]<-1:varNum2
    varDummy_mat <- varSplitName_mat[(varNum+1):varNum2,]
    
    ### Margin Statement ###
    marginTypeNum <- 2
    msVarNum <- 0
    msVar_mat <- matrix(0,nrow=ceiling(varNum/2),ncol=3,dimnames=list(1:ceiling(varNum/2),c("index_1","index_2","index")))
    
    msId_vec <- unique(callInfo_df$marginStatement)
    for(i in 1:length(msId_vec)){
      tempCallId_vec <- which(callInfo_df$marginStatement==msId_vec[i])
      tempLength <- length(tempCallId_vec)
      if(tempLength==2){
        idxTemp <- which(varDummy_mat[,1]==msId_vec[i])
        temp_df <- as.data.frame(table(varDummy_mat[which(varDummy_mat[,1]==msId_vec[i]),3]))
        idxTempRep_vec <- which(temp_df[,2]==2)
        tempResource_vec <- as.character(temp_df[idxTempRep_vec,1])
        
        temp_mat <- varDummy_mat[idxTemp,]
        temp2_mat <- temp_mat[which(!is.na(match(temp_mat[,3],tempResource_vec))),]
        PasteFun1 <- function(x1='',x2=''){
          temp=paste(x1,x2,sep='_',collapse = '')
          return(temp)
        }
        tempRes_df <- aggregate(index~msId+assetCustId,data=temp2_mat,PasteFun1)
        tempPair_vec <- tempRes_df[,3]
        idxTempNew_vec <- msVarNum+1:length(tempPair_vec)
        msVarNum <- msVarNum + length(tempPair_vec)
        msVar_mat[idxTempNew_vec,c(1,2)] <- as.numeric(t(data.frame((strsplit(tempPair_vec,'_')))))
        msVar_mat[idxTempNew_vec,3] <- idxTempNew_vec+varNum2
      }
    }
    # update msVar_mat
    if(msVarNum==0){
      msVar_mat <- msVar_mat[-c(1:length(msVar_mat[,1])),]
    } else{
      msVar_mat <- msVar_mat[1:msVarNum,]
      msVar_mat <- matrix(as.numeric(msVar_mat),ncol=3)
    }
    
    ## add new auxiliary variables, two margin calls coming from one margin statement
    varNum3 <- varNum2+msVarNum
    msVarName_mat <- varSplitName_mat[msVar_mat[,1],]
    msVarName_mat[,2] <- msVarName_mat[,1]
    msVarName_mat[,4] <- msVar_mat[,3]
    PasteFun2 <- function(x){
      temp=paste(x,collapse='_')
      return(temp)
    }
    msVarName_vec <- apply(msVarName_mat[,1:3],1,PasteFun2)
    VarName_vec <- c(VarName_vec,msVarName_vec)
    ### end ###############
    
    ############# MODEL SETUP ###########################################
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
    #
    # variable bounds: a < x < x_quantity
    #    specified by constraint 0 and 1. 
    # variable kind: semi-continuous, value below 'a' will automately set to 0
    #
    ######################################################################
    
    # objective function
    operationTemp_vec <- normOperation_vec[idxEli_vec]
    operationObj_vec <-  c(rep(0,varNum),operationTemp_vec*max(callAmount_mat)*10,-operationTemp_vec[msVar_mat[,1]-varNum]*max(callAmount_mat)*10)
    liquidityObj_vec <-  c(minUnitValue_vec[idxEli_vec]*normLiquidity_vec[idxEli_vec],rep(0,varNum3-varNum))
    costObj_vec <-  c(minUnitValue_vec[idxEli_vec]*normCost_vec[idxEli_vec],rep(0,varNum3-varNum))
    
    fObj_vec <- operationObj_vec*pref_vec[1]+liquidityObj_vec*pref_vec[2]+costObj_vec*pref_vec[3]
    names(fObj_vec) <- VarName_vec
    
    # constraints
    fCon0_mat <- matrix(0,nrow=varNum,ncol=varNum3)
    fCon0_mat[cbind(1:varNum,1:varNum)] <- 1
    fDir0_mat <- rep('>=',varNum)
    fRhs0_mat <- rep(0,varNum)
    
    fCon1_mat <- matrix(0,nrow=varNum,ncol=varNum3)
    fCon1_mat[cbind(1:varNum,1:varNum)] <- 1
    fDir1_mat <- rep('<=',varNum)
    fRhs1_mat <- c(eli_vec[idxEli_vec]*minUnitQuantity_vec[idxEli_vec],rep(1,varNum))
    
    fCon2_mat <- matrix(0,nrow=resourceNum,ncol=varNum)
    fConTemp_mat <- matrix(0,nrow=resourceNum,ncol=varNum3-varNum2)
    temp1 <- 1+(0:(callNum-1))*resourceNum
    idxCon2_vec <- rep(temp1,resourceNum)+rep(c(0:(resourceNum-1)),rep(callNum,resourceNum))
    idxCon2_vec <- match(idxCon2_vec,idxEli_vec)
    fCon2_mat[na.omit(cbind(rep(c(1:resourceNum),rep(callNum,resourceNum)),idxCon2_vec))]<-1
    fCon2_mat <- cbind(fCon2_mat,fCon2_mat*0,fConTemp_mat)
    fDir2_mat <- rep('<=',resourceNum)
    fRhs2_mat <- minUnitQuantity_mat[1,]
    
    fCon3_mat <- matrix(0,nrow=callNum,ncol=varNum)
    fConTemp_mat <- matrix(0,nrow=callNum,ncol=varNum3-varNum2)
    idxCon3_vec <- 1:(resourceNum*callNum)
    idxCon3_vec <- match(idxCon3_vec,idxEli_vec)
    fCon3_mat[na.omit(cbind(rep(c(1:callNum),rep(resourceNum,callNum)),idxCon3_vec))] <- minUnitValue_vec[idxEli_vec]*(1-haircut_vec[idxEli_vec])
    fCon3_mat <- cbind(fCon3_mat,fCon3_mat*0,fConTemp_mat)
    fDir3_mat <- rep('>=',callNum)
    fRhs3_mat <- callAmount_mat[,1]
    
    fCon4_mat <- matrix(0,nrow=varNum,ncol=varNum)
    fConTemp_mat <- matrix(0,nrow=varNum,ncol=varNum3-varNum2)
    fCon4_mat[cbind(1:varNum,1:varNum)] <- 1
    fCon4_mat <- cbind(fCon4_mat,fCon4_mat*(-10000000000),fConTemp_mat)
    fDir4_mat <- rep('<=',varNum)
    fRhs4_mat <- rep(0,varNum)
    
    fCon5_mat <- matrix(0,nrow=varNum,ncol=varNum)
    fConTemp_mat <- matrix(0,nrow=varNum,ncol=varNum3-varNum2)
    fCon5_mat[cbind(1:varNum,1:varNum)] <- 1
    fCon5_mat <- cbind(fCon5_mat,-fCon5_mat,fConTemp_mat)
    fDir5_mat <- rep('>=',varNum)
    fRhs5_mat <- rep(0,varNum)
    
    if(varNum3>varNum2){
      fCon6_mat <- matrix(0,nrow=varNum3-varNum2,ncol=varNum3)
      fCon6_mat[cbind(1:(varNum3-varNum2),msVar_mat[,1])] <- 1
      fCon6_mat[cbind(1:(varNum3-varNum2),msVar_mat[,2])] <- 1
      fCon6_mat[cbind(1:(varNum3-varNum2),msVar_mat[,3])] <- -2
      fDir6_mat <- rep(">=",varNum3-varNum2)
      fRhs6_mat <- rep(0,varNum3-varNum2)
    }
    
    # minimum movement quantity of each asset
    minMoveQuantity <- ceiling(minMoveValue/minUnitValue_vec[idxEli_vec])
    if(length(callAmount_vec[which(minMoveValue > callAmount_vec[idxEli_vec]/(1-haircut_vec[idxEli_vec]))])!=0){
      idxTemp <- which(minMoveValue > callAmount_vec[idxEli_vec]/(1-haircut_vec[idxEli_vec]))
      call.eli_vec <- callAmount_vec[idxEli_vec]/(1-haircut_vec[idxEli_vec])
      minUnitValue.eli_vec <- minUnitValue_vec[idxEli_vec]
      minMoveQuantity[idxTemp] <- ceiling(call.eli_vec[idxTemp]/minUnitValue.eli_vec[idxTemp])
    }
    
    ### solver inputs #####
    lpObj_vec <- fObj_vec
    if(varNum3>varNum2){
      lpCon_mat <- rbind(fCon2_mat,fCon3_mat,fCon4_mat,fCon5_mat,fCon6_mat)
      lpDir_vec <- c(fDir2_mat,fDir3_mat,fDir4_mat,fDir5_mat,fDir6_mat)
      lpRhs_vec <- c(fRhs2_mat,fRhs3_mat,fRhs4_mat,fRhs5_mat,fRhs6_mat)
    } else{
      lpCon_mat <- rbind(fCon2_mat,fCon3_mat,fCon4_mat,fCon5_mat)
      lpDir_vec <- c(fDir2_mat,fDir3_mat,fDir4_mat,fDir5_mat)
      lpRhs_vec <- c(fRhs2_mat,fRhs3_mat,fRhs4_mat,fRhs5_mat)      
    }
    
    lpKind_vec <- rep('semi-continuous',varNum3)
    lpType_vec <- rep('real',varNum3)
    lpType_vec[which(minUnitValue_vec[idxEli_vec]>=100)] <- 'integer'
    lpLowerBound_vec <- c(minMoveQuantity,rep(1,varNum3-varNum))
    lpUpperBound_vec <- c(minUnitQuantity_vec[idxEli_vec],rep(1,varNum3-varNum))
    lpBranchMode_vec <- c(rep('floor',varNum),rep('auto',varNum3-varNum))
    
    lpPresolve <- ifelse(callNum<=5,'none','knapsack')
    lpEpsd <- 1e-11
    lpTimeout <- timeLimit
    lpVerbose <- 'normal'
    # bbRule <-  c("pseudononint", "restart","autoorder","stronginit", "dynamic","rcostfixing")
    bbRule <- c("pseudononint", "greedy", "dynamic","rcostfixing") # default
    
    ### end ###############

    ### call lpSolve solver####
    solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                                     lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                                     presolve=lpPresolve,epsd=lpEpsd,timeout=lpTimeout,verbose=lpVerbose,bb.rule=bbRule)
    ### end ##################
    
    #### solver outputs########
    status<- solverOutput_list$resultStatus
    solverSolution_vec <- solverOutput_list$solverSolution_vec
    solverObjValue <- solverOutput_list$solverObjValue
    #### end ##################
    
    #costObj_vec.result <- sum(costObj_vec*solverSolution_vec)
    #liquidityObj_vec.result <- sum(liquidityObj_vec*solverSolution_vec)
    #operationObj_vec.result <- sum(operationObj_vec*solverSolution_vec)
    #total.move <- sum(solverSolution_vec[(varNum+1):varNum2])
    #obj.result <- c(costObj_vec.result,liquidityObj_vec.result,operationObj_vec.result)
    
    # round up the decimal quantity to the nearest integer.
    # if it's larger than 0.5
    result_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
    result_mat <- t(result_mat);     resultDummy_mat <- result_mat
    result_mat[idxEli_vec]<-solverSolution_vec[1:varNum]
    resultDummy_mat[idxEli_vec]<- solverSolution_vec[(varNum+1):varNum2]
    result_mat[which(result_mat>0.5)] <- ceiling(result_mat[which(result_mat>0.5)])
    result_mat <- t(result_mat);  resultDummy_mat<- t(resultDummy_mat)      # convert solution into matrix format
    #print('result_mat: '); print(result_mat)
    #print('resultDummy_mat: '); print(resultDummy_mat)
    
    ##### CHECK ALLOCATION RESULT #############################
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
    ############## END #######################################
    
    for(i in 1:callNum){                          # store the result into select list
      # j, corresponding index of margin statement
      j <- which(msId_vec==callInfo_df$marginStatement[which(callInfo_df$id==callId_vec[i])])
      
      idxSelectResource_vec <- which(result_mat[i,]!=0)
      selectResource_vec <- resource_vec[idxSelectResource_vec]
      selectAssetId_vec <- assetId_vec[idxSelectResource_vec]
      idxSelectAsset_vec <- rep(0,length(idxSelectResource_vec))
      for(m in 1:length(idxSelectResource_vec)){
        idxSelectAsset_vec[m] <- which(assetInfo_df$id==selectAssetId_vec[m])[1]
      }
      selectAssetCustodianAccount_vec <- custodianAccount[idxSelectResource_vec]
      selectAssetVenue_vec <- venue[idxSelectResource_vec]
      selectAssetName_vec <- assetInfo_df$name[idxSelectAsset_vec]
      selectAssetHaircut_vec <- haircut_mat[i,idxSelectResource_vec]
      selectAssetCurrency_vec <- assetInfo_df$currency[idxSelectAsset_vec]
      selectAssetMinUnitQuantity_vec <- result_mat[i,idxSelectResource_vec]
      selectAssetQuantity_vec <- result_mat[i,idxSelectResource_vec]*minUnit_mat[i,idxSelectResource_vec]
      selectMarginType_vec <- rep(callInfo_df$marginType[i],length(idxSelectResource_vec))
      selectMs_vec <- rep(callInfo_df$marginStatement[i],length(idxSelectResource_vec))
      selectCall_vec <- rep(callId_vec[i],length(idxSelectResource_vec))
      
      #### UPDATE THE ASSET QUANTITY ########
      for(k in 1:length(selectAssetId_vec)){
        tempResource <- selectResource_vec[k]
        tempAvailQuantity_vec <- availAsset_df$quantity[which(availAsset_df$assetCustacId==tempResource)]
        tempQuantity_vec <- availAsset_df$totalQuantity[which(availAsset_df$assetCustacId==tempResource)]
        availAsset_df$quantity[which(availAsset_df$assetCustacId==tempResource)]<- tempAvailQuantity_vec-selectAssetQuantity_vec[k]
        availAsset_df$totalQuantity[which(availAsset_df$assetCustacId==tempResource)]<- tempQuantity_vec-selectAssetQuantity_vec[k]
      }
      #### END ##############################
      
      selectAssetUnitValue_vec <- unitValue_mat[i,idxSelectResource_vec]
      selectAssetAmountUSD_vec <- selectAssetQuantity_vec*selectAssetUnitValue_vec
      selectAssetNetAmountUSD_vec <- selectAssetAmountUSD_vec*(1-haircut_mat[i,idxSelectResource_vec])
      selectAssetFX_vec <- assetInfo_df$FXRate[idxSelectAsset_vec]
      selectAssetAmount_vec <- selectAssetAmountUSD_vec*selectAssetFX_vec
      selectAssetNetAmount_vec <- selectAssetNetAmountUSD_vec*selectAssetFX_vec
      #######
      # netAmount(in local currency) is surfacing in UI
      #######
      selectAsset_df <- data.frame(selectAssetId_vec,selectAssetName_vec,selectAssetNetAmount_vec,selectAssetNetAmountUSD_vec,selectAssetFX_vec,selectAssetHaircut_vec,selectAssetAmount_vec,selectAssetAmountUSD_vec,selectAssetCurrency_vec,
                                   selectAssetQuantity_vec,selectAssetCustodianAccount_vec,selectAssetVenue_vec,selectMarginType_vec,selectMs_vec,selectCall_vec)
      colnames(selectAsset_df)<- c('Asset','Name','NetAmount','NetAmount(USD)','FXRate','Haircut','Amount','Amount(USD)','Currency','Quantity','CustodianAccount','venue','marginType','marginStatement','marginCall')
      
      callSelect_list[[callId_vec[i]]] <- selectAsset_df
      if(is.null(msSelect_list[[msId_vec[j]]])){
        msSelect_list[[msId_vec[j]]] <- selectAsset_df
      } else{
        tempAsset_df <- msSelect_list[[msId_vec[j]]]
        selectAsset_df <- rbind(selectAsset_df,tempAsset_df)
        msSelect_list[[msId_vec[j]]] <- selectAsset_df
      }
    }
    callOutput_list <- callSelect_list
    msOutput_list <- msSelect_list
  }
  
  subtotalFulfilled_mat<- matrix(c(coreInput_list$callAmount_mat[,1],rep(0, callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
  for(i in 1:callNum){
    subtotalFulfilled_mat[i,2] <- sum(callSelect_list[[callId_vec[i]]]$`NetAmount(USD)`)
  }
  checkCall_mat <- subtotalFulfilled_mat
  return(list(msOutput_list=msOutput_list,callOutput_list=callOutput_list,checkCall_mat=checkCall_mat,availAsset_df=availAsset_df,status=status,lpsolveRun=lpsolveRun,solverObjValue=solverObjValue))
}

renjinFix <- function(frame, name) {
  d <- data.frame(frame);
  colnames(d) <- gsub(name, "", colnames(d));
  return(d);
}