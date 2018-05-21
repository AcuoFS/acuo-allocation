
AllocateUnderSufficientOptimalAssets <- function(optimalResource_vec,callInfo_df,availAsset_df,resource_df){
  # Allocate one call at a time using the integral units of optimal resource for this call
  #
  # Returns:
  #   A matrix of the allocated quantity by each call and each resource
  
  ## Haircut Matrix
  haircut_mat <- HaircutVec2Mat(haircut_vec = availAsset_df$haircut + availAsset_df$FXHaircut,
                                availAsset_df,callInfo_df$id,resource_df$id)
  ## Sufficient Resource Quantities for Calls Matrix
  resourceSuffQty_mat <- CalculateIntegralUnit(amount = rep(callInfo_df$callAmount,length(resource_df$id)),
                                               valuePerUnit = matrix(rep(resource_df$minUnitValue, length(callInfo_df$id)),nrow=length(callInfo_df$id),byrow=T),
                                               discount = 1-haircut_mat)
  ## Allocation Matrix
  result_mat <- matrix(0,nrow=length(callInfo_df$id),ncol=length(resource_df$id),dimnames=list(callInfo_df$id,resource_df$id))
  for(i in 1:length(callInfo_df$id)){
    idxResource <- which(resource_df$id==optimalResource_vec[i])
    result_mat[i,idxResource] <- resourceSuffQty_mat[i,idxResource]
  }
  
  return(result_mat)
}

AllocateUnderInsufficientOptimalAssets <- function(costScore_mat,liquidityScore_mat,pref_vec,eli_mat,haircut_mat,
                                                   callInfo_df,resource_df,availAsset_df,
                                                   minMoveValue,operLimitMs,fungible,timeLimit,
                                                   ifNewAlloc,allocated_list,initAllocation_list){
  # Allocate all calls at a time by solving an optimization model with objectives and constraints.
  #
  # Returns:
  #   A matrix of the allocated quantity by each call and each resource
  
  idxEli_vec <- which(t(eli_mat)==1)         # eligible index
  resource_vec <- resource_df$id
  resourceNum <- length(resource_vec)
  callId_vec <- callInfo_df$id
  callNum <- length(callInfo_df$id)
  callAmount_mat <- matrix(rep(callInfo_df$callAmount,resourceNum),nrow=callNum,byrow=F)
  callAmount_vec <- as.vector(t(callAmount_mat))[idxEli_vec]
  
  quantity_mat <- matrix(rep(resource_df$qtyMin,callNum),nrow=callNum,byrow=T)
  quantity_vec <- as.vector(t(quantity_mat))[idxEli_vec]
  
  minUnit_mat <- matrix(rep(resource_df$minUnit, callNum),nrow=callNum,byrow=T)
  minUnit_vec <- as.vector(t(minUnit_mat))[idxEli_vec]
  
  minUnitValue_mat <- matrix(rep(resource_df$minUnitValue, callNum),nrow=callNum,byrow=T)
  minUnitValue_vec <- as.vector(t(minUnitValue_mat))[idxEli_vec]
  
  haircut_vec <- as.vector(t(haircut_mat))[idxEli_vec]
  
  #### Construct Variable Names ######
  varInfo_list <- VarInfo(callInfo_df,availAsset_df)
  
  varName_vec <- varInfo_list$varName_vec
  varNum <- varInfo_list$varNum
  varNum2 <- varInfo_list$varNum2
  pos_vec <- varInfo_list$pos_vec
  
  #### Build the Optimization Model #######
  
  #### OBJECTIVE FUNCTION
  objCoef_mat <- CalculateObjParams(costScore_mat,liquidityScore_mat,pref_vec,"quantity",minUnitValue_mat)
  fObj_vec <- c(t(objCoef_mat)[idxEli_vec],rep(0,varNum2-varNum))
  
  names(fObj_vec) <- varName_vec
  
  #### CONSTRAINTS
  
  # deduct some units to avoid overflow in the rounding later
  oriQuantity_vec <- quantity_vec
  quantity_vec <- quantity_vec - max(1,(callNum-1))
  
  fCon2_list <- QtyConst(varName_vec,varNum,resource_vec,resource_df$qtyMin)
  fCon3_list <- MarginConst(varName_vec,varNum,minUnitValue_vec,haircut_vec,callInfo_df$id,callInfo_df$callAmount)
  if(ifNewAlloc){
    fCon4_list <- DummyConst(varName_vec,varNum,quantity_vec,callAmount_vec,minUnitValue_vec)
    fCon5_list <- MoveConst(varName_vec,varNum,operLimitMs,fungible)
  } else{
    allocated_vec <- ResultList2Vec(allocated_list,callId_vec,minUnit_vec,varName_vec,varNum,pos_vec)
    allocatedDummy_vec <- allocated_vec[(varNum+1):varNum2]
    fCon4_list <- DummyConstInherit(allocatedDummy_vec,varName_vec,varNum,quantity_vec,callAmount_vec,minUnitValue_vec)
    fCon5_list <- MoveConstInherit(allocatedDummy_vec,varName_vec,varNum,operLimitMs,fungible)
  }
  
  
  ## minimum movement quantity of each resource
  minMoveQty_vec <- ceiling(minMoveValue/minUnitValue_vec)
  minMoveQty_vec <- pmin(minMoveQty_vec,quantity_vec)
  
  if(length(callAmount_vec[which(minMoveValue > callAmount_vec/(1-haircut_vec))])!=0){
    idxTemp <- which(minMoveValue > callAmount_vec/(1-haircut_vec))
    callEli_vec <- callAmount_vec/(1-haircut_vec)
    minMoveQty_vec[idxTemp] <- ceiling(callEli_vec[idxTemp]/minUnitValue_vec[idxTemp])
  }
  
  lpObj_vec <- fObj_vec
  lpCon_mat <- rbind(fCon2_list$coef_mat,fCon3_list$coef_mat,fCon4_list$coef_mat,fCon5_list$coef_mat)
  lpDir_vec <- c(fCon2_list$dir_vec,fCon3_list$dir_vec,fCon4_list$dir_vec,fCon5_list$dir_vec)
  lpRhs_vec <- c(fCon2_list$rhs_vec,fCon3_list$rhs_vec,fCon4_list$rhs_vec,fCon5_list$rhs_vec)
  
  
  lpKind_vec <- rep('semi-continuous',varNum2)
  lpType_vec <- rep('real',varNum2)
  #lpType_vec[which(minUnitValue_vec>=1)] <- 'integer'
  lpType_vec[(varNum+1):varNum2] <- 'integer'
  
  lpLowerBound_vec <- DeriveLowerBound(minMoveQty_vec,varName_vec,varNum,resource_vec,resource_df$qtyMin)

  lpUpperBound_vec <- c(quantity_vec,rep(1,varNum2-varNum))
  lpBranchMode_vec <- c(rep('auto',varNum),rep('auto',varNum2-varNum))
  
  #### Control Options in Solver
  lpPresolve <- ifelse(callNum<=10,'none','knapsack')
  lpTimeout <- timeLimit
  
  #### Initial Guess Basis 
  lpGuessBasis_vec <- rep(0,varNum2)
  if(!missing(initAllocation_list)){
    # the initial guess must be a feasible point
    lpGuessBasis_vec <- ResultList2Vec(initAllocation_list,callId_vec,minUnit_vec,varName_vec,varNum,pos_vec)
  }
  
  #### Solve the Model ###############
  #### call lpSolve solver
  solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                                   lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                                   lpGuessBasis_vec=lpGuessBasis_vec, 
                                   presolve=lpPresolve,timeout=lpTimeout)
  #### solver outputs
  solverStatus<- solverOutput_list$resultStatus
  solverSolution_vec <- solverOutput_list$solverSolution_vec
  solverObjValue <- solverOutput_list$solverObjValue
  
  
  #### Exception Handling ####
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
  
  quantity_vec <- oriQuantity_vec
  
  #### Adjust & Convert the Solver Result Start ######
  solverSolution_vec <- AdjustResultVec(solverSolution_vec,varNum,varName_vec,fCon4_list$coef_mat,
                                        callAmount_vec,quantity_vec,minUnitValue_vec)
  
  result_mat <- ResultVec2Mat(solverSolution_vec,callId_vec,resource_vec,idxEli_vec,varNum)
  
  return(result_mat)
}
