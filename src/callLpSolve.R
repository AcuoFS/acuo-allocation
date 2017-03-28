CallLpSolve <- function(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                        lpType_vec,lpKind_vec,lpLowerBound_vec,lpUpperBound_vec,lpBranchMode_vec,
                        ...){
  library(lpSolveAPI)
  # input variables
  # must have: lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec
  # optional: lpType_vec,lpKind_vec,lpLowerBound_vec,lpUpperBound_vec,lpBranchMode_vec
  # optional: ...
  # if optional, then the default parameters will apply
  
  # number of decision variables
  varnum <- length(lpCon_mat[1,])
  
  # make model
  lpModel <- make.lp(0,varnum)  
  name.lp(lpModel, 'Optimal Allocation')
  
  # set objective
  set.objfn(lpModel,lpObj_vec)                    
  
  # set constraints
  for (i in 1:length(lpCon_mat[,1])){              
    add.constraint(lpModel,lpCon_mat[i,],lpDir_vec[i],lpRhs_vec[i])
  }
  
  if(!missing(lpKind_vec)){
    # set semi-continuous variables
    idxSemi_vec <- which(lpKind_vec=='semi-continuous')
    set.semicont(lpModel,idxSemi_vec,TRUE)        
  }
  
  if(!missing(lpType_vec)){
    # set integer variables
    idxInt_vec <- which(lpType_vec=='integer')
    set.type(lpModel,idxInt_vec,'integer')
  }
  
  if(!(missing(lpLowerBound_vec)|| missing(lpUpperBound_vec))){
    # set variables bounds
    set.bounds(lpModel,lower=lpLowerBound_vec,upper=lpUpperBound_vec)
  }
  
  if(!missing(lpBranchMode_vec)){
    # set branch mode
    for(k in 1:length(lpBranchMode_vec)){
      set.branch.mode(lpModel,k,lpBranchMode_vec[k])
    }  
  }
  
  # set control options
  lp.control(lpModel,...)
  
  # solve the problem
  resultStatus <- solve(lpModel)  
  
  
  # get the variables(minUnitQuantity)
  solverSolution_vec <- get.variables(lpModel)
  
  # get the objective
  solverObjValue <- get.objective(lpModel)
  
  return(list(resultStatus=resultStatus,solverSolution_vec=solverSolution_vec,solverObjValue=solverObjValue))
}