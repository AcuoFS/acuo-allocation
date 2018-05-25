
AllocateUnderSufficientOptimalAssets <- function(optimalResource_vec,callInfo_df,availAsset_df,resource_df){
  # Allocate one call at a time using the integral units of optimal resource for this call
  #
  # Returns:
  #   A matrix of the allocated quantity by each call and each resource
  
  ## Haircut Matrix
  haircut_mat <- HaircutVec2Mat(haircutVar_vec = availAsset_df$haircut + availAsset_df$FXHaircut,
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

AllocateUnderInsufficientOptimalAssets <- function(costScore_mat,liquidityScore_mat,pref_vec,
                                                   callInfo_df,resource_df,availAsset_df,
                                                   minMoveValue,operLimitMs,fungible,timeLimit,
                                                   ifNewAlloc,allocated_list,initAllocation_list){
  # Allocate all calls at a time by solving an optimization model with objectives and constraints.
  # 
  # * The decision variables are in the order of call Ids, and then resource Ids
  #
  # Returns:
  #   A matrix of the allocated quantity by each call and each resource
  
  #### Construct Variable Names ######
  varInfo_list <- VarInfo(callInfo_df,availAsset_df)
  
  varName_vec <- varInfo_list$varName_vec
  varNum <- GetQtyVarNum(varName_vec)
  varNum2 <- length(varName_vec)
  
  #### Eligibility Index in Matrix ####
  eli_mat <- EliMat(availAsset_df,callInfo_df$id,resource_df$id)
  
  ## idxEli_vec: the positions of decision variables in the matrix formed by callId and resourceId
  resourceNum <- length(resource_df$id)
  callNum <- length(callInfo_df$id)
  idx_mat <- matrix(1:(resourceNum*callNum),nrow = callNum)
  idxEli_vec <- idx_mat[cbind(match(SplitVarName(varName_vec[1:varNum],"call"),callInfo_df$id),
                              match(SplitVarName(varName_vec[1:varNum],"resource"),resource_df$id))]
  
  #### Deduct Quantity in resource_df ####
  # deduct some units in to avoid overflow in the rounding later 
  oriResourceQtyMin_vec <- resource_df$qtyMin
  resource_df$qtyMin <- resource_df$qtyMin - max(1,(length(callInfo_df$id)-1))
  
  #### Resuable Variables ####
  callAmountVar_vec <- matrix(rep(callInfo_df$callAmount,resourceNum),nrow=callNum,byrow=T)[idxEli_vec]
  quantityVar_vec <- matrix(rep(resource_df$qtyMin,callNum),nrow=callNum,byrow=T)[idxEli_vec]
  minUnitVar_vec <- matrix(rep(resource_df$minUnit, callNum),nrow=callNum,byrow=T)[idxEli_vec]
  minUnitValueVar_vec <- matrix(rep(resource_df$minUnitValue, callNum),nrow=callNum,byrow = T)[idxEli_vec]
  haircutVar_vec <- availAsset_df$haircut + availAsset_df$FXHaircut

  #### Build the Optimization Model #######
  
  #### OBJECTIVE FUNCTION
  objCoef_vec <- CalculateObjParams(t(costScore_mat)[idxEli_vec],t(liquidityScore_mat)[idxEli_vec],pref_vec,"quantity",minUnitValueVar_vec)
  fObj_vec <- c(objCoef_vec,rep(0,varNum2-varNum))
  
  names(fObj_vec) <- varName_vec
  
  #### CONSTRAINTS
  fCon2_list <- QtyConst(varName_vec,varNum,resource_df$id,resource_df$qtyMin)
  fCon3_list <- MarginConst(varName_vec,varNum,minUnitValueVar_vec,haircutVar_vec,callInfo_df$id,callInfo_df$callAmount)
  if(ifNewAlloc){
    fCon4_list <- DummyConst(varName_vec,varNum,quantityVar_vec)
    fCon5_list <- MovementConst(varName_vec,varNum,operLimitMs,fungible)
  } else{
    allocated_vec <- ResultList2Vec(allocated_list,callInfo_df$id,minUnitVar_vec,varName_vec,varNum,varInfo_list$pos_vec)
    allocatedDummy_vec <- allocated_vec[(varNum+1):varNum2]
    fCon4_list <- DummyConstInherit(allocatedDummy_vec,varName_vec,varNum,quantityVar_vec)
    fCon5_list <- MoveConstInherit(allocatedDummy_vec,varName_vec,varNum,operLimitMs,fungible)
  }
  
  #### Objectives & Constraints & Others
  lpObj_vec <- fObj_vec
  lpCon_mat <- rbind(fCon2_list$coef_mat,fCon3_list$coef_mat,fCon4_list$coef_mat,fCon5_list$coef_mat)
  lpDir_vec <- c(fCon2_list$dir_vec,fCon3_list$dir_vec,fCon4_list$dir_vec,fCon5_list$dir_vec)
  lpRhs_vec <- c(fCon2_list$rhs_vec,fCon3_list$rhs_vec,fCon4_list$rhs_vec,fCon5_list$rhs_vec)
  
  lpKind_vec <- rep('semi-continuous',varNum2)
  lpType_vec <- rep('real',varNum2)
  #lpType_vec[which(minUnitValueVar_vec>=1)] <- 'integer'
  lpType_vec[(varNum+1):varNum2] <- 'integer'
  lpLowerBound_vec <- DeriveLowerBound(minMoveValue,varName_vec,resource_df$id,resource_df$qtyMin,quantityVar_vec,minUnitValueVar_vec,callAmountVar_vec,haircutVar_vec)
  lpUpperBound_vec <- c(quantityVar_vec,rep(1,varNum2-varNum))
  lpBranchMode_vec <- c(rep('auto',varNum),rep('auto',varNum2-varNum))
  
  #### Control Options in Solver
  lpPresolve <- ifelse(callNum<=10,'none','knapsack')
  lpTimeout <- timeLimit
  
  #### Initial Guess Basis 
  lpGuessBasis_vec <- rep(0,varNum2)
  if(!missing(initAllocation_list)){
    # the initial guess must be a feasible point
    lpGuessBasis_vec <- ResultList2Vec(initAllocation_list,callInfo_df$id,minUnitVar_vec,varName_vec,varNum,varInfo_list$pos_vec)
  }
  
  #### Call Solver to Solve the Model ###############
  solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                                   lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                                   lpGuessBasis_vec=lpGuessBasis_vec, 
                                   presolve=lpPresolve,timeout=lpTimeout)
  solverStatus<- solverOutput_list$resultStatus
  solverSolution_vec <- solverOutput_list$solverSolution_vec
  solverObjValue <- solverOutput_list$solverObjValue
  
  #### Solver Exception Handling ####
  errStatus <- c(2,5,6,7,10,13)
  if(solverStatus==2){
    errormsg <- paste("ALERR2005: The model constructed by margin calls",paste(callInfo_df$id,collapse = " "),"is infeasible")
    stop(errormsg)
  } else if(is.element(solverStatus,errStatus)){
    # should stop and use another approach to solve the problem
    if(callNum==1){
      rank_vec <- CalculateObjParams(t(costScore_mat)[idxEli_vec],t(liquidityScore_mat)[idxEli_vec],pref_vec,"amount",minUnitValueVar_vec)
      solverSolution_vec <- AllocateByRank(resource_df$id,callInfo_df$id,rank_vec,callInfo_df$callAmount,quantityVar_vec,minUnitValueVar_vec,haircutVar_vec,operLimit)
    } else{ # Solver time out
      ## choose the best alternative
      solverSolution_vec <- lpGuessBasis_vec
    }
  }
  
  #### Restore Quantity in resource_df ####
  resource_df$qtyMin <- oriResourceQtyMin_vec
  
  #### Round Up the Decimal Solver Result ####
  # The quantity decision variable type input to the solver is decimal to improve the performance
  # So we need to round them to integral number
  solution_vec <- RoundUpQuantityVariable(solverSolution_vec,varName_vec)
  solution_vec <- UpdateDummyVariable(solution_vec,varName_vec)
  
  #### Check Whether Solver Result Meet All Criteria ######
  # Convert the solution to Matrix format
  result_mat <- ResultVec2Mat(solution_vec,callInfo_df$id,resource_df$id,varName_vec)
  
  # Checking
  minUnitValue_mat <- matrix(rep(resource_df$minUnitValue, callNum),nrow=callNum,byrow = T)
  haircut_mat <- HaircutVec2Mat(haircutVar_vec,availAsset_df,callInfo_df$id,resource_df$id)
  CheckSolverResult(solution_vec,result_mat,varName_vec,resource_df$qtyMin,callInfo_df$callAmount,minUnitValue_mat,haircut_mat,
                                lpLowerBound_vec,lpUpperBound_vec,operLimitMs,fungible,callInfo_df)
  
  #### Adjust Solver Result ###########
  result_mat <- AdjustSolverResult(result_mat,resource_df$qtyMin,callInfo_df$callAmount,haircut_mat,minUnitValue_mat,eli_mat)
  
  return(result_mat)
}

AllocateByRank <- function(resource_vec,callId,rank_vec,callAmount,quantity_vec,minUnitValue_vec,haircut_vec,operLimit){
  ## rank_mat(scores assume using purely each asset)
  ## names(rank_mat)=resources, value: score
  
  # process
  # find the optimal resources within number of (operLimit) which are sufficient for the call
  solution_vec <- rep(0,length(resource_vec))
  leftCallAmount <- callAmount
  integralSuffQty_vec <- ceiling(callAmount/(1-haircut_vec)/minUnitValue_vec)
  suffIdx_vec <- which(quantity_vec >= integralSuffQty_vec)
  if(length(suffIdx_vec)>=1){
    suffResource_vec <- resource_vec[suffIdx_vec]
    tempIdx <- which.max(rank_vec[suffIdx_vec])
    optimalResource <- suffResource_vec[tempIdx]
    optimalIdx <- which(resource_vec==optimalResource)
    quantity <- integralSuffQty_vec[optimalIdx]
    solution_vec[optimalIdx] <- quantity
  } else{
    if(operLimit<=1){ # operLimit=1
      errormsg <- paste('ALERR2004: It is not sufficient to allocate',floor(operLimit),'assets for',callId)
      stop(errormsg)
    } else{
      for(i in 1:operLimit){
        amount_vec <- floor(quantity_vec)*(1-haircut_vec)*minUnitValue_vec
        suffIdx_vec <- which(amount_vec >= callAmount)
        if(length(suffIdx_vec)==0){
          oriIdx_vec <- which(amount_vec==amount_vec[which.max(amount_vec)])
          tempIdx <- which.max(rank_vec[oriIdx_vec])
          optimalResource <- resource_vec[oriIdx_vec[tempIdx]]
          optimalIdx <- which(resource_vec==optimalResource)
          adjAmount <- quantity_vec[optimalIdx]*minUnitValue_vec[optimalIdx]*(1-haircut_vec[optimalIdx])
          if(leftCallAmount>adjAmount){
            quantity <- quantity_vec[optimalIdx]
            solution_vec[optimalIdx] <- quantity
            quantity_vec[optimalIdx] <- 0
            amount_vec <- floor(quantity_vec)*(1-haircut_vec)*minUnitValue_vec
            leftCallAmount <- leftCallAmount-adjAmount
          } else{
            quantity <- ceiling(leftCallAmount/(1-haircut_vec[optimalIdx])/minUnitValue_vec[optimalIdx])
            solution_vec[optimalIdx] <- quantity
            quantity_vec[optimalIdx] <- quantity_vec[optimalIdx] - quantity
            amount_vec <- floor(quantity_vec)*(1-haircut_vec)*minUnitValue_vec
            leftCallAmount <- leftCallAmount-adjAmount
            break
          }
        }
      }
      if(leftCallAmount > 0){
        errormsg <- paste('ALERR2004: It is not sufficient to allocate',floor(operLimit),'assets for',callId)
        stop(errormsg)
      }
    }
  }
  return(solution_vec)
}
