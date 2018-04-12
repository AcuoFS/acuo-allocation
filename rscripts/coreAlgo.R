
CoreAlgoV2 <- function(callInfo_df, resource_df, availInfo_list,
                       timeLimit,pref_vec,operLimit,operLimitMs_vec,fungible,
                       minMoveValue,ifNewAlloc,initAllocation_list,allocated_list){
  
  #### Prepare Parameters Start #############################
  pref_vec <- pref_vec/sum(pref_vec[1:2]) # Recalculate the parameters weight setting
  callId_vec<-callInfo_df$id
  resource_vec<-resource_df$id
  msId_vec <- unique(callInfo_df$marginStatement)
  
  callInfo_df <- renjinFix(callInfo_df, "callInfo.")
  resource_df <- renjinFix(resource_df, "resource.")
  
  callNum <- length(callId_vec)            # total margin call number
  resourceNum <- length(resource_vec)          # total asset number
  msNum <- length(msId_vec)
  
  base_mat <- availInfo_list$base_mat
  eli_mat <- availInfo_list$eli_mat; 
  eli_vec <-  as.vector(t(eli_mat)) # eligibility matrix & vector
  idxEli_vec <- which(eli_vec==1)         # eligible index
  
  haircut_mat<-availInfo_list$haircut_mat; 
  haircut_vec <- as.vector(t(haircut_mat))[idxEli_vec]      # haircut mat & vec
  
  haircutC_mat<-availInfo_list$haircutC_mat
  haircutFX_mat<-availInfo_list$haircutFX_mat; 
  
  costBasis_mat <- availInfo_list$cost_mat; 
  costBasis_vec <- as.vector(t(costBasis_mat))[idxEli_vec]
  
  #### Persist the Quantity Used in Algo
  quantity_mat <- matrix(rep(resource_df$qtyMin,callNum),nrow=callNum,byrow=T)
  quantity_vec <- as.vector(t(quantity_mat))[idxEli_vec]
  
  unitValue_mat<- matrix(rep(resource_df$unitValue/resource_df$FXRate, callNum),nrow=callNum,byrow=T)
  unitValue_vec <- as.vector(t(unitValue_mat))[idxEli_vec]
  
  minUnit_mat <- matrix(rep(resource_df$minUnit,callNum),nrow=callNum,byrow=T); 
  minUnit_vec <- as.vector(t(minUnit_mat))[idxEli_vec]
  
  minUnitValue_mat <- unitValue_mat*minUnit_mat;
  minUnitValue_vec <- as.vector(t(minUnitValue_mat))[idxEli_vec]
  
  callAmount_mat <- matrix(rep(callInfo_df$callAmount,resourceNum),nrow=callNum,byrow=F); 
  callAmount_vec <- as.vector(t(callAmount_mat))[idxEli_vec]
  
  #### Prepare Parameters END ##############################
  
  #### Output Format Start ######################
  # A list, each element is the allocation result(dataframe) for one margin call
  callSelect_list  <- list()    # store selected assets for each call, list by callId_vec
  msSelect_list <- list()   # store selected assets for each margin statement, list by msId
  #### Output Format End ########################
  
  #### CHECK WHETHER ASSET POOL IS SUFFICIENT START #######
  suffPerCall <- all(apply(eli_mat*(quantity_mat*minUnitValue_mat*(1-haircut_mat)),1,sum) > callAmount_mat[,1])
  suffAllCall <- sum(resource_df$qtyMin*minUnitValue_mat[1,]*(1-apply(haircut_mat,2,max)))>sum(callAmount_mat[,1])
  if(!(suffPerCall&suffAllCall)){
    stop('ALERR2003: Asset inventory is insufficient')
  }
  #### CHECK WHETHER ASSET POOL IS SUFFICIENT END ############
  
  #### Calculate the Objectives Parameters Start #############
  objParams_list <- ConstructModelObj(callAmount_mat,minUnitValue_mat,haircut_mat,costBasis_mat,eli_mat,callInfo_df,
                                      callId_vec,resource_vec)
  #### Calculate the Objectives Parameters END ##############
  
  #### Calculate the Optimal Asset Sufficiency Start #######
  optimalAsset_mat <- DeriveOptimalAssetsV2(quantity_mat,eli_mat,callAmount_mat,haircut_mat,minUnitValue_mat,
                                            pref_vec,objParams_list,callId_vec,resource_vec)
  
  
  assetSuffQty_mat <- ceiling(callAmount_mat/(1-haircut_mat)/minUnitValue_mat) # quantity needed for a single asset to fulfill each call
  selectUniqueAsset_vec <- unique(optimalAsset_mat[,2]) 
  ifSelectAssetSuff_vec <- rep(0,length(selectUniqueAsset_vec))
  
  for(i in 1:length(selectUniqueAsset_vec)){
    resource <- selectUniqueAsset_vec[i]
    idxCall_vec <- which(optimalAsset_mat[,2]==resource) 
    idxResource <- which(resource_vec==resource)
    
    ifSelectAssetSuff_vec[i] <- 1*(sum(assetSuffQty_mat[idxCall_vec,idxResource]) < resource_df$qtyMin[idxResource])
  }
  #### Calculate the Optimal Asset Sufficiency END ##########
  
  #### ALLOCATION ########################################
  
  #### Construct Variable Names Start ######
  varInfo_list <- VarInfo(eli_vec,callInfo_df,resource_vec,callId_vec)
  
  varName_vec <- varInfo_list$varName_vec
  varNum <- varInfo_list$varNum
  varNum2 <- varInfo_list$varNum2
  pos_vec <- varInfo_list$pos_vec
  #### Construct Variable Names END ########
  
  if(1*(!is.element(0,ifSelectAssetSuff_vec))){
    
    #### Optimal Assets are Sufficient Start ##########
    result_mat <- matrix(0,nrow=callNum,ncol=resourceNum,dimnames=list(callId_vec,resource_vec))
    for(k in 1:callNum){
      tempCall <- optimalAsset_mat[k,1]
      tempResource <- optimalAsset_mat[k,2]
      idxTempResource <- which(resource_vec==tempResource)
      result_mat[k,idxTempResource] <- assetSuffQty_mat[k,idxTempResource]
    }
    solverStatus <- -1
    solverObjValue <- -1
    #### Optimal Assets are Sufficient END #############
  } else if(1){
    #### Optimal Assets are not Sufficient Start #########
    
    #### Build the Optimization Model Start #######
    #### OBJECTIVE FUNCTION
    liquidityObj_vec <-  c(minUnitValue_vec*objParams_list$liquidity_vec[idxEli_vec],rep(0,varNum2-varNum))
    costObj_vec <-  c(minUnitValue_vec*objParams_list$cost_vec[idxEli_vec],rep(0,varNum2-varNum))
    
    fObj_vec <- liquidityObj_vec*pref_vec[2]+costObj_vec*pref_vec[1]
    names(fObj_vec) <- varName_vec
    
    #### CONSTRAINTS
    
    # deduct some units to avoid overflow in the rounding later
    oriQuantity_vec <- quantity_vec
    quantity_vec <- quantity_vec - max(1,(callNum-1))
    
    fCon2_list <- QtyConst(varName_vec,varNum,resource_vec,resource_df$qtyMin)
    fCon3_list <- MarginConst(varName_vec,varNum,minUnitValue_vec,haircut_vec,callInfo_df$id,callInfo_df$callAmount)
    if(ifNewAlloc){
      fCon4_list <- DummyConst(varName_vec,varNum,quantity_vec,callAmount_vec,minUnitValue_vec)
      fCon5_list <- MoveConst(varName_vec,varNum,operLimit,operLimitMs_vec,fungible)
    } else{
      allocated_vec <- ResultList2Vec(allocated_list,callId_vec,minUnit_vec,varName_vec,varNum,pos_vec)
      allocatedDummy_vec <- allocated_vec[(varNum+1):varNum2]
      fCon4_list <- DummyConstInherit(allocatedDummy_vec,varName_vec,varNum,quantity_vec,callAmount_vec,minUnitValue_vec)
      fCon5_list <- MoveConstInherit(allocatedDummy_vec,varName_vec,varNum,operLimit,operLimitMs_vec,fungible)
    }
    
    #### Build the Optimization Model END ########
    
    #### Solver Inputs Start #####################
    # minimum movement quantity of each asset
    minMoveQty_vec <- ceiling(minMoveValue/minUnitValue_vec)
    minMoveQty_vec <- pmin(minMoveQty_vec,quantity_vec)
    
    if(length(callAmount_vec[which(minMoveValue > callAmount_vec/(1-haircut_vec))])!=0){
      idxTemp <- which(minMoveValue > callAmount_vec/(1-haircut_vec))
      callEli_vec <- callAmount_vec/(1-haircut_vec)
      minMoveQty_vec[idxTemp] <- ceiling(callEli_vec[idxTemp]/minUnitValue_vec[idxTemp])
    }
    
    lpObj_vec <- fObj_vec
    lpCon_mat <- rbind(fCon2_list$fCon2_mat,fCon3_list$fCon3_mat,fCon4_list$fCon4_mat,fCon5_list$fCon5_mat)
    lpDir_vec <- c(fCon2_list$fDir2_vec,fCon3_list$fDir3_vec,fCon4_list$fDir4_vec,fCon5_list$fDir5_vec)
    lpRhs_vec <- c(fCon2_list$fRhs2_vec,fCon3_list$fRhs3_vec,fCon4_list$fRhs4_vec,fCon5_list$fRhs5_vec)
    
    
    lpKind_vec <- rep('semi-continuous',varNum2)
    lpType_vec <- rep('real',varNum2)
    #lpType_vec[which(minUnitValue_vec>=1)] <- 'integer'
    lpType_vec[(varNum+1):varNum2] <- 'integer'
    lpLowerBound_vec <- c(minMoveQty_vec,rep(0,varNum2-varNum))
    
    varNameResource_vec <- SplitVarName(varName_vec,'resource') # resource in varName
    for(k in 1:resourceNum){
      resourceTemp <- resource_vec[k]
      idxTemp_vec <- which(varNameResource_vec==resourceTemp)
      lowerSumTemp <- sum(lpLowerBound_vec[idxTemp_vec])
      if(lowerSumTemp > resource_df$qtyMin[k]){
        lpLowerBound_vec[idxTemp_vec] <- 0
      }
    }
    
    lpUpperBound_vec <- c(quantity_vec,rep(1,varNum2-varNum))
    lpBranchMode_vec <- c(rep('auto',varNum),rep('auto',varNum2-varNum))
    
    #### Control options
    lpPresolve <- ifelse(callNum<=10,'none','knapsack')
    lpEpsd <- 1e-9
    lpEpsint <- 1e-9
    lpTimeout <- timeLimit
    bbRule <-  c("pseudononint","autoorder","greedy", "dynamic","rcostfixing","branchreverse")
    #bbRule <- c("pseudononint", "greedy", "dynamic","rcostfixing") # default
    lpScale <- c("geometric","quadratic","equilibrate", "integers")
    lpImprove <- c("solution","dualfeas","thetagap")
    
    #### INITIAL GUESS BASIS 
    lpGuessBasis_vec <- rep(0,varNum2)
    if(!missing(initAllocation_list)){
      # the initial guess must be a feasible point
      lpGuessBasis_vec<-ResultList2Vec(initAllocation_list,callId_vec,minUnit_vec,varName_vec,varNum,pos_vec)
    }
    #### Solver Inputs END ###################
    
    #### Solve the Model Start ###############
    #### call lpSolve solver
    solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                                     lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                                     lpGuessBasis_vec=lpGuessBasis_vec, 
                                     presolve=lpPresolve,epsd=lpEpsd,timeout=lpTimeout,bbRule=bbRule,
                                     epsint=lpEpsint, scaling=lpScale,improve=lpImprove)
    #### solver outputs
    solverStatus<- solverOutput_list$resultStatus
    solverSolution_vec <- solverOutput_list$solverSolution_vec
    solverObjValue <- solverOutput_list$solverObjValue
    #### Solve the Model END #################
    
    
    #### Exception Start ####
    errStatus <- c(2,5,6,7,10,13)
    if(solverStatus==2){
      errormsg <- paste("ALERR2005: The model constructed by margin calls",paste(callId_vec,collapse = " "),"is infeasible")
      stop(errormsg)
    } else if(is.element(solverStatus,errStatus)){
      if(callNum==1){
        rank_vec <- objParams_list$cost_mat*pref_vec[1]+objParams_list$liquidity_mat*pref_vec[2]
        callAmount <- callInfo_df$callAmount
        solverSolution_vec <- AllocateByRank(resource_vec[idxEli_vec],callInfo_df$id,rank_vec,callAmount,quantity_vec[idxEli_vec],minUnitValue_vec[idxEli_vec],haircut_vec[idxEli_vec],operLimit)
      } else{ # Solver time out
        #### choose the best alternative
        solverSolution_vec <- lpGuessBasis_vec
      }
    }
    #### Exception END ######
    
    quantity_vec <- oriQuantity_vec
    
    #### Adjust & Convert the Solver Result Start ######
    solverSolution_vec <- AdjustResultVec(solverSolution_vec,varNum,varName_vec,fCon4_list$fCon4_mat,
                                          callAmount_vec,quantity_vec,minUnitValue_vec)
    
    result_mat <- ResultVec2Mat(solverSolution_vec,callId_vec,resource_vec,idxEli_vec,varNum)
    #### Adjust & Convert the Solver Result END ######## 
    
  } # else if end
  
  checkResult <- CheckResultVec(result_mat,quantityTotal_vec=resource_df$qtyMin,callId_vec,callInfo_df$callAmount,minUnitValue_mat,haircut_mat,eli_mat)
  result_mat <- checkResult$result_mat
  resource_df$qtyMin <- checkResult$quantityTotal_vec
  
  #### Prepare Outputs Start #######################
  #### convert the result_mat to list
  
  result_list <- ResultMat2List(result_mat,callId_vec,resource_vec,callInfo_df,haircutC_mat,haircutFX_mat,costBasis_mat,resource_df,
                                callSelect_list,msSelect_list)
  
  callSelect_list <- result_list$callSelect_list
  msSelect_list <- result_list$msSelect_list
  
  subtotalFulfilled_mat<- matrix(c(callAmount_mat[,1],rep(0, callNum)),nrow=callNum,ncol=2,dimnames = list(callId_vec,c('callAmount','fulfilledAmount')))
  
  for(i in 1:callNum){
    subtotalFulfilled_mat[i,2] <- sum(callSelect_list[[callId_vec[i]]]$`NetAmount(USD)`)
  }
  checkCall_mat <- subtotalFulfilled_mat
  #### Prepare Outputs END ########################
  
  return(list(msOutput_list=msSelect_list,
              callOutput_list=callSelect_list,checkCall_mat=checkCall_mat,
              solverStatus=solverStatus,solverObjValue=solverObjValue))
}
