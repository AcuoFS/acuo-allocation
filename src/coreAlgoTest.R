
CoreAlgoV2 <- function(coreInput_list,availAsset_df,timeLimit,pref_vec,operLimit,operLimitMs,fungible,minMoveValue,initAllocation_list){
  
  #### Prepare Parameters Start #############################
  pref_vec <- pref_vec/sum(pref_vec[1:2]) # Recalculate the parameters weight setting
  callId_vec<-coreInput_list$callId_vec
  resource_vec<-coreInput_list$resource_vec
  
  callInfo_df <- renjinFix(coreInput_list$callInfo_df, "callInfo.")
  assetInfo_df <- renjinFix(coreInput_list$assetInfo_df, "assetInfo.")
  availAsset_df <- renjinFix(availAsset_df,"availAsset.")
  
  msId_vec <- unique(callInfo_df$marginStatement)
  custodianAccount <- coreInput_list$custodianAccount  
  venue <- coreInput_list$venue
  
  callNum <- length(callId_vec)            # total margin call number
  resourceNum <- length(resource_vec)          # total asset number
  msNum <- length(msId_vec)
  
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
  varNum2 <- varInfo_list$varNum2
  msVar_mat <- varInfo_list$msVar_mat
  idxEli_vec <- which(eli_vec==1)  
  #### Construct Variable Names END ########
  
  if(1*(!is.element(0,ifSelectAssetSuff_vec))){
    
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
    liquidityObj_vec <-  c(minUnitValue_vec[idxEli_vec]*objParams_list$liquidity_vec[idxEli_vec],rep(0,varNum2-varNum))
    costObj_vec <-  c(minUnitValue_vec[idxEli_vec]*objParams_list$cost_vec[idxEli_vec],rep(0,varNum2-varNum))
    #cat('costObj_vec',costObj_vec,'\n'); cat('liquidityObj_vec,',liquidityObj_vec,'\n')
    fObj_vec <- liquidityObj_vec*pref_vec[2]+costObj_vec*pref_vec[1]
    names(fObj_vec) <- varName_vec
    
    # constraints
    fCon0_mat <- matrix(0,nrow=varNum,ncol=varNum2)
    fCon0_mat[cbind(1:varNum,1:varNum)] <- 1
    fDir0_vec <- rep('>=',varNum)
    fRhs0_vec <- rep(0,varNum)
    
    fCon1_mat <- matrix(0,nrow=varNum,ncol=varNum2)
    fCon1_mat[cbind(1:varNum,1:varNum)] <- 1
    fDir1_vec <- rep('<=',varNum)
    fRhs1_vec <- c(eli_vec[idxEli_vec]*minUnitQuantity_vec[idxEli_vec],rep(1,varNum))
    
    fCon2_list <- QtyConst(varName_vec,varNum,resource_vec,quantityTotal_vec)
    fCon2_mat <- fCon2_list$fCon2_mat
    fDir2_vec <- fCon2_list$fDir2_vec
    fRhs2_vec <- fCon2_list$fRhs2_vec
    
    fCon3_list <- MarginConst(varName_vec,varNum,minUnitValue_vec[idxEli_vec],haircut_vec[idxEli_vec],callInfo_df$id,callInfo_df$callAmount)
    fCon3_mat <- fCon3_list$fCon3_mat
    fDir3_vec <- fCon3_list$fDir3_vec
    fRhs3_vec <- fCon3_list$fRhs3_vec
    
    fCon4_list <- DummyConst(varName_vec,varNum,minUnitQuantity_vec[idxEli_vec])
    fCon4_mat <- fCon4_list$fCon4_mat
    fDir4_vec <- fCon4_list$fDir4_vec
    fRhs4_vec <- fCon4_list$fRhs4_vec
 
    fCon5_list <- MoveConst(varName_vec,varNum,operLimit,operLimitMs,fungible)
    fCon5_mat <- fCon5_list$fCon5_mat
    fDir5_vec <- fCon5_list$fDir5_vec
    fRhs5_vec <- fCon5_list$fRhs5_vec
  
   
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
    lpCon_mat <- rbind(fCon2_mat,fCon3_mat,fCon4_mat,fCon5_mat)
    lpDir_vec <- c(fDir2_vec,fDir3_vec,fDir4_vec,fDir5_vec)
    lpRhs_vec <- c(fRhs2_vec,fRhs3_vec,fRhs4_vec,fRhs5_vec)

  
    lpKind_vec <- rep('semi-continuous',varNum2)
    lpType_vec <- rep('real',varNum2)
    lpType_vec[which(minUnitValue_vec[idxEli_vec]>=1)] <- 'integer'
    lpType_vec[(varNum+1):varNum2] <- 'integer'
    lpLowerBound_vec <- c(minMoveQuantity_vec,rep(0,varNum2-varNum))
    for(k in 1:resourceNum){
      resourceTemp <- resource_vec[k]
      idxTemp_vec <- which(varName_mat[3,]==resourceTemp)
      lowerSumTemp <- sum(lpLowerBound_vec[idxTemp_vec])
      if(lowerSumTemp > quantityTotal_vec[k]){
        lpLowerBound_vec[idxTemp_vec] <- 0
      }
    }
    #using 0 or 1 is still under the consideration
    #lpLowerBound_vec <- c(minMoveQuantity_vec,rep(1,varNum2-varNum))
    lpUpperBound_vec <- c(minUnitQuantity_vec[idxEli_vec],rep(1,varNum2-varNum))
    lpBranchMode_vec <- c(rep('auto',varNum),rep('auto',varNum2-varNum))
    
    lpPresolve <- ifelse(callNum<=10,'none','knapsack')
    lpEpsd <- 1e-9
    lpEpsind <- 1e-9
    lpTimeout <- timeLimit
    bbRule <-  c("pseudononint","autoorder","greedy", "dynamic","rcostfixing")
    #bbRule <- c("pseudononint", "greedy", "dynamic","rcostfixing") # default
    lpScale <- c("geometric","quadratic","equilibrate", "integers")
    lpImprove <- c("solution","dualfeas","thetagap")
    
    #### INITIAL GUESS BASIS 
    lpGuessBasis_vec <- rep(0,varNum2)
    if(!missing(initAllocation_list)){
      # the initial guess must be a feasible point
      lpGuessBasis_vec<-ResultList2Vec(initAllocation_list,callId_vec,minUnit_vec,varName_vec,varNum,idxEli_vec,fCon4_mat)
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
                                     epsint=lpEpsind, scaling=lpScale,improve=lpImprove)
    #### solver outputs
    solverStatus<- solverOutput_list$resultStatus
    solverSolution_vec <- solverOutput_list$solverSolution_vec
    solverObjValue <- solverOutput_list$solverObjValue
    #### Solve the Model END #################
    
    
    #### Exception Start ####
    errStatus <- c(2,5,6,7,10,13)
    
    if(solverStatus==7){ # Solver time out
      #### choose the best alternative
      solverSolution_vec <- lpGuessBasis_vec
    } else if(solverStatus==2 & callNum==1){ # Infeasible model
      rank_vec <- normCost_mat*pref_vec[1]+normLiquidity_mat*pref_vec[2]
      callAmount <- callInfo_df$callAmount
      solverSolution_vec <- AllocateByRank(resource_vec[idxEli_vec],callId,rank_vec,callAmount,minUnitQuantity_vec[idxEli_vec],minUnitValue_vec[idxEli_vec],haircut_vec[idxEli_vec],operLimit)
    }
    
    #### Exception END ######
    
    
    #### Adjust & Convert the Solver Result Start ######
    solverSolution_vec <- AdjustResultVec(solverSolution_vec,varNum,varName_vec,fCon4_mat,
                                          callAmount_vec[idxEli_vec],minUnitQuantity_vec[idxEli_vec],minUnitValue_vec[idxEli_vec])
    
    result_mat <- ResultVec2Mat(solverSolution_vec,callId_vec,resource_vec,idxEli_vec,varNum)
    #### Adjust & Convert the Solver Result END ######## 
    
  } # else if end
  
  result_mat <- CheckResultVec(result_mat,quantityTotal_vec,callId_vec,callInfo_df$callAmount,minUnitValue_mat,haircut_mat,eli_mat)
  
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
