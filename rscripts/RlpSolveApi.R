callLpSolve <- function(lp.obj,lp.con,lp.dir,lp.rhs,
                        lp.type=lp.type,lp.kind=lp.kind,lp.bounds.lower=lp.bounds.lower,lp.bounds.upper=lp.bounds.upper,lp.branch.mode=lp.branch.mode,
                        ...){
  # input variables
  # must have: lp.obj,lp.con,lp.dir,lp.rhs
  # optional: lp.type,lp.kind,lp.bounds.lower,lp.bounds.upper,lp.branch.mode
  # optional: ...
  # if optional, then the default parameters will apply

  # number of decision variables
  var.num <- length(lp.con[1,])

  # make model
  lps.model <- make.lp(0,var.num)

  # set objective
  set.objfn(lps.model,lp.obj)

  # set constraints
  for (i in 1:length(lp.con[,1])){
    add.constraint(lps.model,lp.con[i,],lp.dir[i],lp.rhs[i])
  }

  if(!missing(lp.kind)){
    # set semi-continuous variables
    semi.idx <- which(lp.kind=='semi-continuous')
    set.semicont(lps.model,semi.idx,TRUE)
  }

  if(!missing(lp.type)){
    # set integer variables
    int.idx <- which(lp.type=='integer')
    set.type(lps.model,int.idx,'integer')
  }

  if(!(missing(lp.bounds.lower)|| missing(lp.bounds.upper))){
    # set variables bounds
    set.bounds(lps.model,lower=lp.bounds.lower,upper=lp.bounds.upper)
  }

  if(!missing(lp.branch.mode)){
    # set branch mode
    for(k in 1:length(lp.branch.mode)){
      set.branch.mode(lps.model,k,lp.branch.mode[k])
    }
  }

  # set control options
  lp.control(lps.model,...)

  # solve the problem
  result.status <- solve(lps.model)

  # get the variables(minUnitQuantity)
  lpSolveAPI.solution <- get.variables(lps.model)

  # get the objective
  result.objective <- get.objective(lps.model)

  return(list(result.status=result.status,lpSolveAPI.solution=lpSolveAPI.solution,result.objective=result.objective))
}