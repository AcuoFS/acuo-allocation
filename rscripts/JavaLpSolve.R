import(lpsolve.LpSolve)

CallLpSolve <- function(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                        lpType_vec,lpKind_vec,lpLowerBound_vec,lpUpperBound_vec,lpBranchMode_vec,
                        lpGuessBasis_vec,
                        presolve,epsd,timeout,bbRule,epsind=lpEpsind,
                        scaling,improve){
  # input variables
  # must have: lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec
  # optional: lpType_vec,lpKind_vec,lpLowerBound_vec,lpUpperBound_vec,lpBranchMode_vec
  # optional: ...
  # if optional, then the default parameters will apply

  # number of decision variables
  varNum <- length(lpCon_mat[1,])

  # make model
  lpModel <- LpSolve$makeLp(0L, varNum)

#  if(!missing(verbose)){
#    lpModel$setVerbose(as.integer(verbose));
#  }

  # set objective
  strObj <- paste(lpObj_vec,collapse=' ')
  lpModel$strSetObjFn(strObj)

  dirType = c("<=", ">=", "=")

  # set constraints
 for (i in 1:length(lpCon_mat[,1])){
   dir <- lpDir_vec[i]
   if(is.character(dir)) {
     dir <- match(dir, dirType)
   }
   if(is.na(dir)){
     stop(sQuote("dir"), " must must be one of ", paste(dirType,collapse=' ,'))
   }
  # cat('lpCon_mat','[',i,']');print(lpCon_mat[i,])
   strCon <- paste(lpCon_mat[i,],collapse=' ')
  # print(strCon)
   dir <- as.integer(dir)
   rhs <- as.double(lpRhs_vec[i])
   lpModel$strAddConstraint(strCon, dir, rhs)
 }

  if(!missing(lpKind_vec)){
      # set semi-continuous variables
      idxSemi_vec <- which(lpKind_vec=='semi-continuous')
      for(i in 1:varNum){
          lpModel$setSemicont(as.integer(idxSemi_vec[i]),TRUE)
      }
  }

  if(!missing(lpType_vec)){
    # set integer variables
    idxInt_vec <- which(lpType_vec=='integer')
    for(i in  idxInt_vec){
      lpModel$setInt(as.integer(i), as.logical(TRUE))
    }

    #set.type(lpModel,idxInt_vec,'integer')
  }

  if(!(missing(lpLowerBound_vec))){
    # set variables lower bounds
    if(length(lpLowerBound_vec) != varNum){
      stop(sQuote("lower"), " must contain one element for each column", " in the model")
    }
    for(i in 1:varNum){
      lpModel$setLowbo(i, as.double(lpLowerBound_vec[i]))
    }
  }

  if(!(missing(lpUpperBound_vec))){
    # set variables upper bounds
    if(length(lpUpperBound_vec) != varNum){
      stop(sQuote("upper"), " must contain one element for each column", " in the model")
    }
    for(i in 1:varNum){
      lpModel$setUpbo(i, as.double(lpUpperBound_vec[i]))
    }
  }

  if(!missing(lpBranchMode_vec)){
    # set branch mode
    if(varNum != length(lpBranchMode_vec)){
      stop(sQuote("varNum"), " and ", sQuote("lpBranchMode_vec"), " must be the same length")
    }
    for(i in 1:length(lpBranchMode_vec)){
      mode <- lpBranchMode_vec[i]
      if(is.character(mode)) {
        mode <- pmatch(mode, c("ceiling", "floor", "auto", "default"), nomatch = NA)
        if(any(is.na(mode)))
          stop("invalid mode")
        else
          mode <- mode - 1
      }
      lpModel$setVarBranch(as.integer(i), as.integer(mode))
    }
  }

  # set control options
  if(!missing(epsd)){
    lpModel$setEpsd(epsd)
  }
    
  if(!missing(epsd)){
    lpModel$setEpsint(epsint)
  }
  
  if(!missing(presolve)){
    if(presolve=='none'){
        lpModel$setPresolve(LpSolve$PRESOLVE_NONE,as.integer(1e6))
    } else if(presolve=='knapsack'){
        lpModel$setPresolve(LpSolve$PRESOLVE_KNAPSACK,as.integer(1e6))
    }
  }

  if(!missing(timeout)){
    lpModel$setTimeout(timeout)
  }

  if(!missing(bbRule)){
  #  lpModel$setBbRule(bbRule)
  }

  if(!missing(scaling)){
  #  lpModel$setScaling(scaling)
  }
  
  if(!missing(improve)){
  #  lpModel$setScaling(improve)
  }
  
  # solve the problem
  resultStatus <- lpModel$solve()

  # get the variables(minUnitQuantity)
  solverSolution_vec <- lpModel$getPtrVariables()

  # get the objective
  solverObjValue <- lpModel$getObjective()

  lpModel$deleteLp()


  return(list(resultStatus=resultStatus,solverSolution_vec=solverSolution_vec,solverObjValue=solverObjValue))
}










add.constraint <- function(lprec, xt, type = c("<=", "=", ">="), rhs)
{
  if(is.character(type)) {
    type <- match.arg(type)
    type <- match(type, c("<=", ">=", "="))
  }

  lprec$addConstraint(xt, type, rhs)

  invisible()
}

set.type <- function(lprec, columns, type = c("integer", "binary", "real"))
{
  type <- match.arg(type)

  switch(type,
         integer = lprec$setInt(as.integer(columns), as.logical(TRUE)),
         binary = lprec$setBinary(as.integer(columns), as.logical(TRUE)),
         real = lprec$setInt(as.integer(columns), as.logical(FALSE))
  )

  invisible()
}

set.bounds <- function(lprec, lower = NULL, upper = NULL, columns = 1:n)
{
  n <- lprec$getNcolumns()
  ncol <- length(columns)

  if(!is.null(lower)) {
    if(length(lower) != ncol)
      stop(sQuote("lower"), " must contain one element for each column", " in the model")

    lprec$setLowbo(as.integer(columns), as.double(lower))
  }

  if(!is.null(upper)) {
    if(length(upper) != ncol)
      stop(sQuote("upper"), " must contain one element for each column", " in the model")

    lprec$setUpbo(as.integer(columns), as.double(upper))
  }

  invisible()
}

set.branch.mode <- function(lprec, columns, modes)
{
  if(length(columns) != length(modes))
    stop(sQuote("columns"), " and ", sQuote("modes"), " must be the same length")

  if(is.character(modes)) {
    modes <- pmatch(modes, c("ceiling", "floor", "auto", "default"), nomatch = NA)
    if(any(is.na(modes)))
      stop("invalid mode")
    else
      modes <- modes - 1
  }

  lprec$setVarBranch(as.integer(columns), as.integer(modes))

  invisible()
}