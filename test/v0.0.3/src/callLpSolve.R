CallLpSolve <- function(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                        lpType_vec,lpKind_vec,lpLowerBound_vec,lpUpperBound_vec,lpBranchMode_vec,
                        lpGuessBasis_vec,
                        presolve,epsd,timeout,bbRule,epsint,
                        scaling,improve){
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
    t1 = sum(is.na(lpCon_mat[i,]))
    t2 = sum(is.na(lpDir_vec[i]))
    t3 = sum(is.na(lpRhs_vec[i]))
    if((t1+t2+t3>=1)){
      errormsg <- paste(paste('constraint ',i), 'contains NA!')
      stop(errormsg)
    }
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
  
  # guess basis
  if(!missing(lpGuessBasis_vec)){
    if(!all(lpGuessBasis_vec==0)){
      guess.basis(lpModel,lpGuessBasis_vec)
    }
  }
  
  # set control options
  lp.control(lpModel,presolve=presolve,epsd=epsd,timeout=timeout,bb.rule=bbRule,epsint=epsint,
             scaling=scaling,improve=improve,verbose='severe',negrange=-1e-6)
  
  # solve the problem
  resultStatus <- solve(lpModel)  
  
  #print('constraint: ')
  #print(get.constraints(lpModel))
  
  
  # write the model to output file
  date <- format(Sys.time(), "%d%b%Y")
  dir <- 'lpSolve/'
  filename <- paste(dir,'lpModel',date,'.lp',sep='')
  write.lp(lpModel,filename,'lp')
  
  
  # get the variables(minUnitQuantity)
  solverSolution_vec <- get.variables(lpModel)
  
  # get the objective
  solverObjValue <- get.objective(lpModel)
  
  return(list(resultStatus=resultStatus,solverSolution_vec=solverSolution_vec,solverObjValue=solverObjValue))
}