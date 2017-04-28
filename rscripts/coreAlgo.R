
CoreAlgoV1 <- function(coreInput_list,availAsset_df,timeLimit,pref_vec,minMoveValue,initAllocation_list){
  #### Prepare Parameters Start #############################
  pref_vec <- pref_vec/sum(pref_vec) # Recalculate the parameters weight setting
  callId_vec<-coreInput_list$callId_vec
  resource_vec<-coreInput_list$resource_vec
  
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
  if(callNum==1){
    quantityTotal_vec <- minUnitQuantity_mat
  } else{
    quantityTotal_vec <- apply(minUnitQuantity_mat,2,max)
  }
  
  callAmount_mat <- coreInput_list$callAmount_mat; callAmount_vec <- as.vector(t(callAmount_mat)) 
  
  costBasis_mat <- coreInput_list$cost_mat; costBasis_vec <- coreInput_list$cost_vec 
  
  #### Prepare Parameters END ##############################
  
  #### Output Format Start ######################
  # A list, each element is the allocation result(dataframe) for one margin call
  callSelect_list  <- list()    # store selected assets for each call, list by callId_vec
  msSelect_list <- list()   # store selected assets for each margin statement, list by msId
  #### Output Format End ########################
  
  #### CHECK WHETHER ASSET POOL IS SUFFICIENT START #######
  suffPerCall <- all(apply(eli_mat*(minUnitQuantity_mat*minUnitValue_mat*(1-haircut_mat)),1,sum) > callAmount_mat[,1])
  suffAllCall <- sum(quantityTotal_vec*minUnitValue_mat[1,]*(1-apply(haircut_mat,2,max)))>sum(callAmount_mat[,1])
  if(!(suffPerCall&suffAllCall)){
    stop('Asset inventory is insufficient!')
  }
  #### CHECK WHETHER ASSET POOL IS SUFFICIENT END ############
  
  #### Calculate the Objectives Parameters Start #############
  objParams_list <- ConstructModelObj(callAmount_mat,minUnitValue_mat,haircut_mat,costBasis_mat,eli_mat,callInfo_df,
                                      callId_vec,resource_vec)
  #### Calculate the Objectives Parameters END ##############
  
  #### Calculate the Optimal Asset Sufficiency Start #######
  optimalAsset_mat <- DeriveOptimalAssetsV1(minUnitQuantity_mat,eli_mat,callAmount_mat,haircut_mat,minUnitValue_mat,
                                            pref_vec,objParams_list,callId_vec,resource_vec)
  
  assetSuffQty_mat <- ceiling(callAmount_mat/(1-haircut_mat)/minUnitValue_mat) # quantity needed for a single asset to fulfill each call
  selectUniqueAsset_vec <- unique(optimalAsset_mat[,2]) 
  ifSelectAssetSuff_vec <- rep(0,length(selectUniqueAsset_vec))
  
  for(i in 1:length(selectUniqueAsset_vec)){
    id <- selectUniqueAsset_vec[i]
    idx.temp <- optimalAsset_mat[which(optimalAsset_mat[,2]==id),1] # calls have the least cost asset id
    ifSelectAssetSuff_vec[i] <- 1*(sum(assetSuffQty_mat[idx.temp,id]) < max(minUnitQuantity_mat[,id]))
  }
  #### Calculate the Optimal Asset Sufficiency END ##########
  
  #### ALLOCATION START #####################################
  #### Construct Variable Names Start ######
  varInfo_list <- VarInfo(eli_vec,callInfo_df,resource_vec,callId_vec)
  
  varName_vec <- varInfo_list$varName_vec
  varName_mat <- SplitVarName(varName_vec)
  varNum <- varInfo_list$varNum
  varNum2 <- varInfo_list$varNum2
  varNum3 <- varInfo_list$varNum3
  msVar_mat <- varInfo_list$msVar_mat
  idxEli_vec <- which(eli_vec==1)  
  #### Construct Variable Names END ########
  
  if(!is.element(0,ifSelectAssetSuff_vec)){
    #### Optimal Assets are Sufficient Start ##########
    result_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
    for(k in 1:callNum){
      tempCall <- optimalAsset_mat[k,1]
      tempResource <- optimalAsset_mat[k,2]
      idxTempResource <- which(resource_vec==tempResource)
      result_mat[k,idxTempResource] <- assetSuffQty_mat[k,idxTempResource]
    }
    solverStatus <- 'solved'
    lpsolveRun <- FALSE
    solverObjValue <- -1
    #### Optimal Assets are Sufficient END #############
  } else if(1){
    
    #### Optimal Assets are not Sufficient Start #########
    lpsolveRun<-TRUE
    
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
    costObj_vec <-  c(minUnitValue_vec[idxEli_vec]*objParams_list$cost_vec[idxEli_vec],rep(0,varNum3-varNum))
    liquidityObj_vec <-  c(minUnitValue_vec[idxEli_vec]*objParams_list$liquidity_vec[idxEli_vec],rep(0,varNum3-varNum))
    # consider FX 
    operationTemp_vec <- objParams_list$operation_vec[idxEli_vec]
    operationObj_vec <-  c(rep(0,varNum),operationTemp_vec*max(callAmount_mat)*10,-operationTemp_vec[msVar_mat[,1]-varNum]*max(callAmount_mat)*10)
    
    #### for intern task
    #costObj_vec[19:36] <- 40
    #costObj_vec[18+c(1:3,7:9,13:15)]<- 10
    ####
    
    fObj_vec <- costObj_vec*pref_vec[1]+liquidityObj_vec*pref_vec[2]+operationObj_vec*pref_vec[3]
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
    fRhs2_vec <- quantityTotal_vec
    
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
      callEli_vec <- callAmount_vec[idxEli_vec]/(1-haircut_vec[idxEli_vec])
      minUnitValueEli_vec <- minUnitValue_vec[idxEli_vec]
      minMoveQuantity_vec[idxTemp] <- ceiling(callEli_vec[idxTemp]/minUnitValueEli_vec[idxTemp])
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
    lpLowerBound_vec <- c(minMoveQuantity_vec,rep(0,varNum3-varNum)) # 0 will give less optimal result
    for(k in 1:resourceNum){
      resourceTemp <- resource_vec[k]
      idxTemp_vec <- which(varName_mat[3,]==resourceTemp)
      lowerSumTemp <- sum(lpLowerBound_vec[idxTemp_vec])
      if(lowerSumTemp > quantityTotal_vec[k]){
        lpLowerBound_vec[idxTemp_vec] <- 0
      }
    }
    lpUpperBound_vec <- c(minUnitQuantity_vec[idxEli_vec],rep(1,varNum3-varNum))
    lpBranchMode_vec <- c(rep('auto',varNum),rep('auto',varNum3-varNum))
    
    lpPresolve <- ifelse(callNum<=5,'none','knapsack')
    lpEpsd <- 1e-9
    lpEpsind <- 1e-9
    lpTimeout <- timeLimit
    # bbRule <-  c("pseudononint", "restart","autoorder","stronginit", "dynamic","rcostfixing")
    bbRule <- c("pseudononint", "greedy", "dynamic","rcostfixing") # default
    lpScale <- c("geometric","quadratic","equilibrate", "integers")
    lpImprove <- c("solution","dualfeas","thetagap")
    
    #### INITIAL GUESS BASIS
    lpGuessBasis_vec <- rep(0,varNum3)
    if(!missing(initAllocation_list)){
      # the initial guess must be a feasible point
      lpGuessBasis_vec<-ResultList2Vec(initAllocation_list,callId_vec,minUnit_vec,varName_vec,varNum3,varNum,idxEli_vec)
    }
    
    #### Solver Inputs END ###################
    
    #### Solve the Model Start ###############
    #### Call lpSolve Solver
    solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                                     lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                                     lpGuessBasis_vec=lpGuessBasis_vec,
                                     presolve=lpPresolve,epsd=lpEpsd,timeout=lpTimeout,bb.rule=bbRule,
                                     epsind=lpEpsind, scaling=lpScale,improve=lpImprove)
    
    #### Solver Outputs
    solverStatus<- solverOutput_list$resultStatus
    solverSolution_vec <- solverOutput_list$solverSolution_vec
    solverObjValue <- solverOutput_list$solverObjValue
    
    solverSolution_vec <- AdjustResultVec(solverSolution_vec,varNum,varNum2,varNum3,msVar_mat)
    
    result_mat <- ResultVec2Mat(solverSolution_vec,callId_vec,resource_vec,idxEli_vec,varNum)
    #### Solve the Model END #################
    
    result_mat <- CheckResultVec(result_mat,quantityTotal_vec,callId_vec,callInfo_df$callAmount,minUnitValue_mat,haircut_mat,eli_mat)
    #### Update the Results END ############
    
  } # else if END
  
  #### Prepare Outputs Start #######################
  #### Convert the result_mat to List
  result_list <- ResultMat2List(result_mat,resource_vec,availAsset_df,coreInput_list,callSelect_list,msSelect_list)
  
  callSelect_list <- result_list$callSelect_list
  msSelect_list <- result_list$msSelect_list
  availAsset_df <- result_list$availAsset_df
  
  subtotalFulfilled_mat<- matrix(c(coreInput_list$callAmount_mat[,1],rep(0, callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
  for(i in 1:callNum){
    subtotalFulfilled_mat[i,2] <- sum(callSelect_list[[callId_vec[i]]]$`NetAmount(USD)`)
  }
  checkCall_mat <- subtotalFulfilled_mat
  #### Prepare Outputs END ########################
  
  return(list(msOutput_list=msSelect_list,availAsset_df=availAsset_df,
              varName_vec,varNum,
              callOutput_list=callSelect_list,checkCall_mat=checkCall_mat,
              solverStatus=solverStatus,lpsolveRun=lpsolveRun,solverObjValue=solverObjValue))
}

CoreAlgoV2 <- function(coreInput_list,availAsset_df,timeLimit,pref_vec,operLimit,minMoveValue,initAllocation_list){
  
  #### Prepare Parameters Start #############################
  pref_vec <- pref_vec/sum(pref_vec[1:2]) # Recalculate the parameters weight setting
  callId_vec<-coreInput_list$callId_vec
  resource_vec<-coreInput_list$resource_vec
  
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
  
  #### Persist the Quantity Used in Algo
  if(callNum==1){
    quantityTotal_vec <- minUnitQuantity_mat
  } else{
    quantityTotal_vec <- apply(minUnitQuantity_mat,2,max)
  }
  
  unitValue_mat<-coreInput_list$unitValue_mat; unitValue_vec <- coreInput_list$unitValue_vec     # asset unit value mat & vec
  minUnit_mat <- coreInput_list$minUnit_mat; minUnit_vec <- coreInput_list$minUnit_vec;
  minUnitValue_mat <- coreInput_list$minUnitValue_mat; minUnitValue_vec <- coreInput_list$minUnitValue_vec;
  
  callAmount_mat <- coreInput_list$callAmount_mat; callAmount_vec <- as.vector(t(callAmount_mat)) 
  costBasis_mat <- coreInput_list$cost_mat; costBasis_vec <- coreInput_list$cost_vec 
  
  #### Prepare Parameters END ##############################
  
  #### Output Format Start ######################
  # A list, each element is the allocation result(dataframe) for one margin call
  callSelect_list  <- list()    # store selected assets for each call, list by callId_vec
  msSelect_list <- list()   # store selected assets for each margin statement, list by msId
  #### Output Format End ########################
  
  #### CHECK WHETHER ASSET POOL IS SUFFICIENT START #######
  suffPerCall <- all(apply(eli_mat*(minUnitQuantity_mat*minUnitValue_mat*(1-haircut_mat)),1,sum) > callAmount_mat[,1])
  suffAllCall <- sum(quantityTotal_vec*minUnitValue_mat[1,]*(1-apply(haircut_mat,2,max)))>sum(callAmount_mat[,1])
  if(!(suffPerCall&suffAllCall)){
    stop('Asset inventory is insufficient!')
  }
  #### CHECK WHETHER ASSET POOL IS SUFFICIENT END ############
  
  #### Calculate the Objectives Parameters Start #############
  objParams_list <- ConstructModelObj(callAmount_mat,minUnitValue_mat,haircut_mat,costBasis_mat,eli_mat,callInfo_df,
                                      callId_vec,resource_vec)
  #### Calculate the Objectives Parameters END ##############
  
  #### Calculate the Optimal Asset Sufficiency Start #######
  optimalAsset_mat <- DeriveOptimalAssetsV2(minUnitQuantity_mat,eli_mat,callAmount_mat,haircut_mat,minUnitValue_mat,
                                            pref_vec,objParams_list,callId_vec,resource_vec)
  
  
  assetSuffQty_mat <- ceiling(callAmount_mat/(1-haircut_mat)/minUnitValue_mat) # quantity needed for a single asset to fulfill each call
  selectUniqueAsset_vec <- unique(optimalAsset_mat[,2]) 
  ifSelectAssetSuff_vec <- rep(0,length(selectUniqueAsset_vec))
  
  for(i in 1:length(selectUniqueAsset_vec)){
    id <- selectUniqueAsset_vec[i]
    idx.temp <- optimalAsset_mat[which(optimalAsset_mat[,2]==id),1] # calls have the least cost assetId_vec=id
    ifSelectAssetSuff_vec[i] <- 1*(sum(assetSuffQty_mat[idx.temp,id]) < max(minUnitQuantity_mat[,id]))
  }
  #### Calculate the Optimal Asset Sufficiency END ##########
  
  #### ALLOCATION ########################################
  
  #### Construct Variable Names Start ######
  varInfo_list <- VarInfo(eli_vec,callInfo_df,resource_vec,callId_vec)
  
  varName_vec <- varInfo_list$varName_vec
  varName_mat <- SplitVarName(varName_vec)
  varNum <- varInfo_list$varNum
  varNum2 <- varInfo_list$varNum2
  varNum3 <- varInfo_list$varNum3
  msVar_mat <- varInfo_list$msVar_mat
  idxEli_vec <- which(eli_vec==1)  
  #### Construct Variable Names END ########
  
  if(!is.element(0,ifSelectAssetSuff_vec)){
    
    # exception
    # USD is optimal for VM in ms1, and xxx Equity is optimal for IM in ms1
    # the limit movement per margin statement is 1
    # this will generate two movements
    #
    
    #### Optimal Assets are Sufficient Start ##########
    result_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
    for(k in 1:callNum){
      tempCall <- optimalAsset_mat[k,1]
      tempResource <- optimalAsset_mat[k,2]
      idxTempResource <- which(resource_vec==tempResource)
      result_mat[k,idxTempResource] <- assetSuffQty_mat[k,idxTempResource]
    }
    solverStatus <- 'solved'
    lpsolveRun <- FALSE
    solverObjValue <- -1
    #### Optimal Assets are Sufficient END #############
  } else if(1){
    #### Optimal Assets are not Sufficient Start #########
    lpsolveRun<-TRUE
    
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
    # 6.& 7. in same margin statement
    #   
    # 8. constraint on the asset movements(operLimit)
    #
    # variable bounds: a < x < x_quantity
    #    specified by constraint 0 and 1. 
    # variable kind: semi-continuous, value below 'a' will automately set to 0
    #
    #### MODEL SETUP END ####################################################
    
    #### Build the Optimization Model Start #######
    # objective function
    
    liquidityObj_vec <-  c(minUnitValue_vec[idxEli_vec]*objParams_list$liquidity_vec[idxEli_vec],rep(0,varNum3-varNum))
    costObj_vec <-  c(minUnitValue_vec[idxEli_vec]*objParams_list$cost_vec[idxEli_vec],rep(0,varNum3-varNum))
    #cat('costObj_vec',costObj_vec,'\n'); cat('liquidityObj_vec,',liquidityObj_vec,'\n')
    fObj_vec <- liquidityObj_vec*pref_vec[2]+costObj_vec*pref_vec[1]
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
    fRhs2_vec <- quantityTotal_vec
    
    
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
      # DV[1]+DV[varNum+1]-2*DV[varNum2+x] >=0
      fCon6_mat <- matrix(0,nrow=varNum3-varNum2,ncol=varNum3)
      fCon6_mat[cbind(1:(varNum3-varNum2),msVar_mat[,1])] <- 1
      fCon6_mat[cbind(1:(varNum3-varNum2),msVar_mat[,2])] <- 1
      fCon6_mat[cbind(1:(varNum3-varNum2),msVar_mat[,3])] <- -2
      fDir6_vec <- rep(">=",varNum3-varNum2)
      fRhs6_vec <- rep(0,varNum3-varNum2)
      #cat('fCon6 num:',length(fDir6_vec),'\n')
      
      # DV[1]+DV[varNum+1]-*DV[varNum2+x] <=1 
      fCon7_mat <- matrix(0,nrow=varNum3-varNum2,ncol=varNum3)
      fCon7_mat[cbind(1:(varNum3-varNum2),msVar_mat[,1])] <- 1
      fCon7_mat[cbind(1:(varNum3-varNum2),msVar_mat[,2])] <- 1
      fCon7_mat[cbind(1:(varNum3-varNum2),msVar_mat[,3])] <- -2
      fDir7_vec <- rep(">=",varNum3-varNum2)
      fRhs7_vec <- rep(0,varNum3-varNum2)
      #cat('fCon7 num:',length(fDir7_vec),'\n')
    }
    
    fCon8_mat <- matrix(0,nrow=1,ncol=varNum3)
    fCon8_mat[1,(varNum+1):varNum2] <- 1
    # wrong
    if(varNum3>varNum2){
      fCon8_mat[(varNum2+1):varNum3] <- -1
      
    }
    fDir8_vec <- c('<=')
    fRhs8_vec <- c(operLimit)
    
    #### Build the Optimization Model END ########
    
    #### Solver Inputs Start #####################
    # minimum movement quantity of each asset
    minMoveQuantity_vec <- ceiling(minMoveValue/minUnitValue_vec[idxEli_vec])
    minUnitQuantityEli_vec <- minUnitQuantity_vec[idxEli_vec]
    minMoveQuantity_vec <- pmin(minMoveQuantity_vec,minUnitQuantityEli_vec)
    if(length(callAmount_vec[which(minMoveValue > callAmount_vec[idxEli_vec]/(1-haircut_vec[idxEli_vec]))])!=0){
      idxTemp <- which(minMoveValue > callAmount_vec[idxEli_vec]/(1-haircut_vec[idxEli_vec]))
      callEli_vec <- callAmount_vec[idxEli_vec]/(1-haircut_vec[idxEli_vec])
      minUnitValueEli_vec <- minUnitValue_vec[idxEli_vec]
      minMoveQuantity_vec[idxTemp] <- ceiling(callEli_vec[idxTemp]/minUnitValueEli_vec[idxTemp])
    }
    
    lpObj_vec <- fObj_vec
    if(varNum3>varNum2){
      lpCon_mat <- rbind(fCon2_mat,fCon3_mat,fCon4_mat,fCon5_mat,fCon6_mat,fCon7_mat,fCon8_mat)
      lpDir_vec <- c(fDir2_vec,fDir3_vec,fDir4_vec,fDir5_vec,fDir6_vec,fDir7_vec,fDir8_vec)
      lpRhs_vec <- c(fRhs2_vec,fRhs3_vec,fRhs4_vec,fRhs5_vec,fRhs6_vec,fRhs7_vec,fRhs8_vec)
    } else{
      lpCon_mat <- rbind(fCon2_mat,fCon3_mat,fCon4_mat,fCon5_mat,fCon8_mat)
      lpDir_vec <- c(fDir2_vec,fDir3_vec,fDir4_vec,fDir5_vec,fDir8_vec)
      lpRhs_vec <- c(fRhs2_vec,fRhs3_vec,fRhs4_vec,fRhs5_vec,fRhs8_vec)      
    }
    
    if(length(lpCon_mat[,1])==518){
      #stop('Let us debug!')
    }
    lpKind_vec <- rep('semi-continuous',varNum3)
    lpType_vec <- rep('real',varNum3)
    lpType_vec[which(minUnitValue_vec[idxEli_vec]>=1)] <- 'integer'
    lpType_vec[(varNum+1):varNum3] <- 'integer'
    lpLowerBound_vec <- c(minMoveQuantity_vec,rep(0,varNum3-varNum))
    for(k in 1:resourceNum){
      resourceTemp <- resource_vec[k]
      idxTemp_vec <- which(varName_mat[3,]==resourceTemp)
      lowerSumTemp <- sum(lpLowerBound_vec[idxTemp_vec])
      if(lowerSumTemp > quantityTotal_vec[k]){
        lpLowerBound_vec[idxTemp_vec] <- 0
      }
    }
    #using 0 or 1 is still under the consideration
    #lpLowerBound_vec <- c(minMoveQuantity_vec,rep(1,varNum3-varNum))
    lpUpperBound_vec <- c(minUnitQuantity_vec[idxEli_vec],rep(1,varNum3-varNum))
    lpBranchMode_vec <- c(rep('auto',varNum),rep('auto',varNum3-varNum))
    
    lpPresolve <- ifelse(callNum<=5,'none','knapsack')
    lpEpsd <- 1e-9
    lpEpsind <- 1e-9
    lpTimeout <- timeLimit
    # bbRule <-  c("pseudononint", "restart","autoorder","stronginit", "dynamic","rcostfixing")
    bbRule <- c("pseudononint", "greedy", "dynamic","rcostfixing") # default
    lpScale <- c("geometric","quadratic","equilibrate", "integers")
    lpImprove <- c("solution","dualfeas","thetagap")
    
    
    #### INITIAL GUESS BASIS 
    lpGuessBasis_vec <- rep(0,varNum3)
    if(!missing(initAllocation_list)){
      # the initial guess must be a feasible point
      lpGuessBasis_vec<-ResultList2Vec(initAllocation_list,callId_vec,minUnit_vec,varName_vec,varNum3,varNum,idxEli_vec)
      if(length(lpCon_mat[,1])==518){
        ## constraint pre-check
        cons <- rep(0,518); 
        for(i in 1:518){cons[i]=sum(lpCon_mat[i,]* lpGuessBasis_vec);
        }
        l2 <- length(fRhs2_vec)
        l3 <- length(fRhs3_vec)
        l4 <- length(fRhs4_vec)
        l5 <- length(fRhs5_vec)
        l6 <- length(fRhs6_vec)
        l7 <- length(fRhs7_vec)
        l8 <- length(fRhs8_vec)
        
        
        all(cons[1:l2]<=lpRhs_vec[1:l2]); temp <- l2
        all(cons[(temp+1):(temp+l3)] >= lpRhs_vec[(temp+1):(temp+l3)]); temp<- temp+l3
        all(cons[(temp+1):(temp+l4)] <= lpRhs_vec[(temp+1):(temp+l4)]); temp<- temp+l4
        all(cons[(temp+1):(temp+l5)] >= lpRhs_vec[(temp+1):(temp+l5)]); temp<- temp+l5
        all(cons[(temp+1):(temp+l6)] >= lpRhs_vec[(temp+1):(temp+l6)]); temp<- temp+l6
        all(cons[(temp+1):(temp+l7)] >= lpRhs_vec[(temp+1):(temp+l7)]); temp<- temp+l7
        all(cons[(temp+1):(temp+l8)] <= lpRhs_vec[(temp+1):(temp+l8)]); temp<- temp+l8
      }
      # stop('Let us debug!')
    }
    
    #guessValue <- sum(fObj_vec*lpGuessBasis_vec)
    #cat('lpGuessBasis_vec:',lpGuessBasis_vec,'\n')
    #cat('guess result: ', guessValue,'\n')
    #### Solver Inputs END ###################
    
    #### Solve the Model Start ###############
    #### call lpSolve solver
    
    solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                                     lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                                     lpGuessBasis_vec=lpGuessBasis_vec, 
                                     presolve=lpPresolve,epsd=lpEpsd,timeout=lpTimeout,bbRule=bbRule,
                                     epsind=lpEpsind, scaling=lpScale,improve=lpImprove)
    #### solver outputs
    solverStatus<- solverOutput_list$resultStatus
    solverSolution_vec <- solverOutput_list$solverSolution_vec
    solverObjValue <- solverOutput_list$solverObjValue
    #### Solve the Model END #################
    
    
    #### Exception: Solver time out Start ####
    errStatus <- c(2,5,6,7,10,13)
    if(solverStatus==7){
      #### choose the best alternative
      solverSolution_vec <- lpGuessBasis_vec
    }
    #### Exception: Solver time out END ######
    
    
    #### Adjust & Convert the Solver Result Start ######
    solverSolution_vec <- AdjustResultVec(solverSolution_vec,varNum,varNum2,varNum3,msVar_mat)
    
    result_mat <- ResultVec2Mat(solverSolution_vec,callId_vec,resource_vec,idxEli_vec,varNum)
    #### Adjust & Convert the Solver Result END ######## 
    
  } # else if end
  
  result_mat <- CheckResultVec(result_mat,quantityTotal_vec,callInfo_df$callAmount,minUnitValue_mat,haircut_mat,eli_mat)
  
  #### Prepare Outputs Start #######################
  #### convert the result_mat to list
  
  result_list <- ResultMat2List(result_mat,resource_vec,availAsset_df,coreInput_list,callSelect_list,msSelect_list)
  
  callSelect_list <- result_list$callSelect_list
  msSelect_list <- result_list$msSelect_list
  availAsset_df <- result_list$availAsset_df
  
  subtotalFulfilled_mat<- matrix(c(coreInput_list$callAmount_mat[,1],rep(0, callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
  for(i in 1:callNum){
    subtotalFulfilled_mat[i,2] <- sum(callSelect_list[[callId_vec[i]]]$`NetAmount(USD)`)
  }
  checkCall_mat <- subtotalFulfilled_mat
  #### Prepare Outputs END ########################
  
  return(list(msOutput_list=msSelect_list,availAsset_df=availAsset_df,
              callOutput_list=callSelect_list,checkCall_mat=checkCall_mat,
              solverStatus=solverStatus,lpsolveRun=lpsolveRun,solverObjValue=solverObjValue))
}

