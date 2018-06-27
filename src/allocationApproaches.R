
AllocateUnderSufficientOptimalAssets <- function(optimalResource_vec,callInfo_df,availAsset_df,resource_df){
  # Allocate one call at a time using the integral units of optimal resource for this call
  #
  # Returns:
  #   A matrix of the allocated quantity by each call and each resource
  
  ## Haircut Matrix
  haircut_mat <- HaircutMat(availAsset_df,callInfo_df$id,resource_df$id)
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

AllocateUnderInsufficientOptimalAssets <- function(configurations,costScore_mat,liquidityScore_mat,pref_vec,
                                                   callInfo_df,resource_df,availAsset_df,
                                                   minMoveValue,operLimitMs,fungible,timeLimit,
                                                   ifNewAlloc,allocated_list,initAllocation_mat){
  # Allocate all calls at a time by solving an optimization model with objectives and constraints.
  # 
  # * The decision variables are in the order of call Ids, and then resource Ids
  #
  # Returns:
  #   A matrix of the allocated quantity by each call and each resource
  
  #### Construct Variable Names ######
  varName_vec <- DeriveVarName(callInfo_df,availAsset_df)
  
  qtyVarNum <- GetQtyVarNum(varName_vec)
  totalVarNum <- length(varName_vec)
  
  #### Quantity Decision Variables Positions in Matrix ####
  ## idxEli_vec: the positions of decision variables in the matrix formed by callId and resourceId
  resourceNum <- length(resource_df$id)
  callNum <- length(callInfo_df$id)
  idx_mat <- matrix(1:(resourceNum*callNum),nrow = callNum)
  idxEli_vec <- idx_mat[cbind(match(SplitVarName(varName_vec[1:qtyVarNum],"call"),callInfo_df$id),
                              match(SplitVarName(varName_vec[1:qtyVarNum],"resource"),resource_df$id))]
  
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
  
  minUnitValue_mat <- matrix(rep(resource_df$minUnitValue, callNum),nrow=callNum,byrow = T)
  
  #### Build the Optimization Model Start #######
  #### Objective Function ######
  objCoef_vec <- CalculateObjParams(costScore_mat[idxEli_vec],liquidityScore_mat[idxEli_vec],pref_vec,"quantity",minUnitValueVar_vec)
  fObj_vec <- c(objCoef_vec,rep(0,totalVarNum-qtyVarNum))
  
  names(fObj_vec) <- varName_vec
  
  #### Constraints #############
  fCon2_list <- QtyConst(varName_vec,qtyVarNum,resource_df$id,resource_df$qtyMin)
  fCon3_list <- MarginConst(varName_vec,qtyVarNum,minUnitValueVar_vec,haircutVar_vec,callInfo_df$id,callInfo_df$callAmount)
  if(ifNewAlloc){
    fCon4_list <- DummyConst(varName_vec,qtyVarNum,quantityVar_vec)
    fCon5_list <- MovementConst(varName_vec,qtyVarNum,operLimitMs,fungible)
  } else{
    allocated_vec <- ResultList2Vec(allocated_list,callInfo_df$id,minUnitVar_vec,varName_vec,qtyVarNum)
    allocatedDummy_vec <- allocated_vec[(qtyVarNum+1):totalVarNum]
    fCon4_list <- DummyConstInherit(allocatedDummy_vec,varName_vec,qtyVarNum,quantityVar_vec)
    fCon5_list <- MoveConstInherit(allocatedDummy_vec,varName_vec,qtyVarNum,operLimitMs,fungible)
  }
  
  lpObj_vec <- fObj_vec
  lpCon_mat <- rbind(fCon2_list$coef_mat,fCon3_list$coef_mat,fCon4_list$coef_mat,fCon5_list$coef_mat)
  lpDir_vec <- c(fCon2_list$dir_vec,fCon3_list$dir_vec,fCon4_list$dir_vec,fCon5_list$dir_vec)
  lpRhs_vec <- c(fCon2_list$rhs_vec,fCon3_list$rhs_vec,fCon4_list$rhs_vec,fCon5_list$rhs_vec)
  
  #### Lower Bound and Upper Bound #####
  lpLowerBound_vec <- DeriveLowerBound(minMoveValue,varName_vec,quantityVar_vec,minUnitValueVar_vec,callAmountVar_vec,haircutVar_vec)
  lpUpperBound_vec <- c(quantityVar_vec,rep(1,totalVarNum-qtyVarNum))
  
  #### Other Parameters in Solver ########
  lpKind_vec <- rep('semi-continuous',totalVarNum)
  lpType_vec <- rep('real',totalVarNum)
  #lpType_vec[which(minUnitValueVar_vec>=1)] <- 'integer'
  lpType_vec[(qtyVarNum+1):totalVarNum] <- 'integer'
  lpBranchMode_vec <- c(rep('auto',qtyVarNum),rep('auto',totalVarNum-qtyVarNum))
  
  #### Control Options in Solver #########
  lpPresolve <- ifelse(callNum<=10,'none','knapsack')
  lpTimeout <- timeLimit
  
  #### Initial Guess Basis in Solver ########
  lpGuessBasis_vec <- rep(0,totalVarNum)
  if(!missing(initAllocation_mat)){
    # the initial guess must be a feasible point
	lpGuessBasis_vec[1:qtyVarNum] <- initAllocation_mat[idxEli_vec]
  }
  
  #### Build the Optimization Model End ##########
  #### Call Solver to Solve the Model ###############
  solverOutput_list <- CallLpSolve(configurations, lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                                   lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                                   lpGuessBasis_vec=lpGuessBasis_vec, 
                                   presolve=lpPresolve,timeout=lpTimeout)
  solverStatus<- solverOutput_list$resultStatus
  solverSolution_vec <- solverOutput_list$solverSolution_vec
  solverObjValue <- solverOutput_list$solverObjValue
  
  #### Solver Exception Handling ####
  errStatus <- c(2,5,6,7,10,13)
  solverStatus <- 5
  if(solverStatus==2){
    errormsg <- paste("ALERR2005: The model constructed by margin calls",paste(callInfo_df$id,collapse = " "),"is infeasible")
    stop(errormsg)
  } else if(is.element(solverStatus,errStatus)){
    # should stop and use another approach to solve the problem
    result_mat <- AllocateByRank(costScore_mat,liquidityScore_mat,pref_vec,callInfo_df,resource_df,availAsset_df,
                                 operLimitMs,fungible)
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
  haircut_mat <- HaircutMat(availAsset_df,callInfo_df$id,resource_df$id)
  CheckSolverResult(solution_vec,result_mat,varName_vec,callInfo_df,resource_df$qtyMin,minUnitValue_mat,haircut_mat,
                                lpLowerBound_vec,lpUpperBound_vec,operLimitMs,fungible)

  #### Adjust Solver Result ###########
  result_mat <- AdjustSolverResult(result_mat,resource_df$qtyMin,callInfo_df$callAmount,haircut_mat,minUnitValue_mat,
                                   eli_mat=EliMat(availAsset_df[c('callId','resource')],callInfo_df$id,resource_df$id))
  
  return(result_mat)
}

