source("src/coreAlgo.R")

### UNIT TESTS ##############
#
# function: CallLpSolve()
# 

# @ TEST 1, solved
# only required inputs
testCallLpSolveFunctionWithStaticInput1 <- function(){
  ### solver inputs #######
  lpCon_mat <- rbind(c(6,2,4),c(1,1,6),c(4,5,4))
  lpDir_vec <- c("<=",">=","=")
  lpRhs_vec <- c(150,0,48)
  lpObj_vec <- c(-3,-4,-3)
  ### end ##################
  
  ### call lpSolve solver####
  solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec)
  ### end ##################
  
  #### solver outputs########
  resultStatus<- solverOutput_list$resultStatus
  solverSolution_vec <- solverOutput_list$solverSolution_vec
  solverObjValue <- solverOutput_list$solverObjValue
  #### end ##################
  
  ### result checkings ####
  checkEquals(resultStatus,0)
  checkEquals(solverSolution_vec,c(0,9.6,0))
  checkEquals(solverObjValue,-38.4)
}

# @ TEST 2, solved
# integer type
testCallLpSolveFunctionWithStaticInput2 <- function(){
  ### solver inputs #######
  lpCon_mat <- rbind(c(6,2,4),c(1,1,6),c(4,5,4))
  lpDir_vec <- c("<=",">=","=")
  lpRhs_vec <- c(150,0,48)
  lpObj_vec <- c(-3,-4,-3)
  lpType_vec <- rep('integer',3)
  ### end ##################
  
  ### call lpSolve solver####
  solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,lpType_vec=lpType_vec)
  ### end ##################
  
  #### solver outputs########
  resultStatus<- solverOutput_list$resultStatus
  solverSolution_vec <- solverOutput_list$solverSolution_vec
  solverObjValue <- solverOutput_list$solverObjValue
  #### end ##################
  
  ### result checkings ####
  checkEquals(resultStatus,0)
  checkEquals(solverSolution_vec,c(2,8,0))
  checkEquals(solverObjValue,-38)
}

# @ TEST 3, infeasible
testCallLpSolveFunctionWithStaticInput3 <- function(){
  ### solver inputs #######
  lpCon_mat <- rbind(c(1,0,1),c(1,1,0),c(0,-1,1))
  lpDir_vec <- c("<=",">=","=")
  lpRhs_vec <- c(100,60,50)
  lpObj_vec <- c(1,1,1)
  ### end ##################
  
  ### call lpSolve solver####
  solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec)
  ### end ##################
  
  #### solver outputs########
  resultStatus<- solverOutput_list$resultStatus
  solverSolution_vec <- solverOutput_list$solverSolution_vec
  solverObjValue <- solverOutput_list$solverObjValue
  #### end ##################
  
  ### result checkings ####
  checkEquals(resultStatus,2)
}

# @ TEST 4, solved
# callIds = c("mcp1","mcp5","mcp7","mcp38","mcp50")
testCallLpSolveFunctionWithStaticInput4 <- function(){
  ### solver inputs ########
  data.dir <- 'test/testCallLpSolve/input1/' 
  lpCon_mat <- read.csv(paste(data.dir,'cons.csv',sep=''));  lpCon_mat <- lpCon_mat[,-1]
  lpDir_vec <- read.csv(paste(data.dir,'dir.csv',sep=''));  lpDir_vec <- lpDir_vec[,-1]
  lpRhs_vec <- read.csv(paste(data.dir,'rhs.csv',sep=''));  lpRhs_vec <- lpRhs_vec[,-1]
  lpObj_vec <- read.csv(paste(data.dir,'obj.csv',sep=''));  lpObj_vec <- lpObj_vec[,-1]
  lpKind_vec <- read.csv(paste(data.dir,'kind.csv',sep=''));  lpKind_vec <- lpKind_vec[,-1]
  lpType_vec <- read.csv(paste(data.dir,'type.csv',sep=''));  lpType_vec <- lpType_vec[,-1]
  lpLowerBound_vec <- read.csv(paste(data.dir,'bounds.lower.csv',sep=''));  lpLowerBound_vec <- lpLowerBound_vec[,-1]
  lpUpperBound_vec <- read.csv(paste(data.dir,'bounds.upper.csv',sep=''));  lpUpperBound_vec <- lpUpperBound_vec[,-1]
  lpBranchMode_vec <- read.csv(paste(data.dir,'branch.mode.csv',sep='')); lpBranchMode_vec <- lpBranchMode_vec[,-1]
  
  lpPresolve <- 'none'
  lpEpsd <- 1e-11
  lpTimeout <- 1
  ### end ##################
  
  ### call lpSolve solver####
  solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                        lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                        presolve=lpPresolve,epsd=lpEpsd,timeout=lpTimeout)
  ### end ##################
  
  #### solver outputs########
  resultStatus<- solverOutput_list$resultStatus
  solverSolution_vec <- solverOutput_list$solverSolution_vec
  solverObjValue <- solverOutput_list$solverObjValue
  #### end ##################
  
  ### result checkings ####
  checkEquals(resultStatus,0)
  result<- read.csv(paste(data.dir,'lpSolveAPI.solution.csv',sep='')); result<- result[,-1]
  checkEquals(solverSolution_vec,result)
  checkEquals(solverObjValue,138404376)
}

# @ TEST 5, numerical failure
# callIds = c("mcp1","mcp5","mcp7","mcp38","mcp20","mcp22","mcp15","mcp30","mcp50","mcp51")
testCallLpSolveFunctionWithStaticInput5 <- function(){
  ### solver inputs ########
  data.dir <- 'test/testCallLpSolve/input2/' 
  lpCon_mat <- read.csv(paste(data.dir,'cons.csv',sep=''));  lpCon_mat <- lpCon_mat[,-1]
  lpDir_vec <- read.csv(paste(data.dir,'dir.csv',sep=''));  lpDir_vec <- lpDir_vec[,-1]
  lpRhs_vec <- read.csv(paste(data.dir,'rhs.csv',sep=''));  lpRhs_vec <- lpRhs_vec[,-1]
  lpObj_vec <- read.csv(paste(data.dir,'obj.csv',sep=''));  lpObj_vec <- lpObj_vec[,-1]
  lpKind_vec <- read.csv(paste(data.dir,'kind.csv',sep=''));  lpKind_vec <- lpKind_vec[,-1]
  lpType_vec <- read.csv(paste(data.dir,'type.csv',sep=''));  lpType_vec <- lpType_vec[,-1]
  lpLowerBound_vec <- read.csv(paste(data.dir,'bounds.lower.csv',sep=''));  lpLowerBound_vec <- lpLowerBound_vec[,-1]
  lpUpperBound_vec <- read.csv(paste(data.dir,'bounds.upper.csv',sep=''));  lpUpperBound_vec <- lpUpperBound_vec[,-1]
  lpBranchMode_vec <- read.csv(paste(data.dir,'branch.mode.csv',sep='')); lpBranchMode_vec <- lpBranchMode_vec[,-1]
  
  lpPresolve <- 'knapsack'
  lpEpsd <- 1e-11
  lpTimeout <- 1
  ### end ##################
  
  ### call lpSolve solver####
  solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                        lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                        presolve=lpPresolve,epsd=lpEpsd,timeout=lpTimeout)
  ### end ##################
  
  #### solver outputs########
  resultStatus<- solverOutput_list$resultStatus
  solverSolution_vec <- solverOutput_list$solverSolution_vec
  solverObjValue <- solverOutput_list$solverObjValue
  #### end ##################
  
  ### result checkings ####
  checkEquals(resultStatus,5)
}

# @ TEST 6, solved
# callIds = c("mcp1","mcp5","mcp7","mcp38","mcp20","mcp22","mcp15","mcp30","mcp50","mcp51")
testCallLpSolveFunctionWithStaticInput6 <- function(){
  ### solver inputs ########
  data.dir <- 'test/testCallLpSolve/input2/' 
  lpCon_mat <- read.csv(paste(data.dir,'cons.csv',sep=''));  lpCon_mat <- lpCon_mat[,-1]
  lpDir_vec <- read.csv(paste(data.dir,'dir.csv',sep=''));  lpDir_vec <- lpDir_vec[,-1]
  lpRhs_vec <- read.csv(paste(data.dir,'rhs.csv',sep=''));  lpRhs_vec <- lpRhs_vec[,-1]
  lpObj_vec <- read.csv(paste(data.dir,'obj.csv',sep=''));  lpObj_vec <- lpObj_vec[,-1]
  lpKind_vec <- read.csv(paste(data.dir,'kind.csv',sep=''));  lpKind_vec <- lpKind_vec[,-1]
  lpType_vec <- read.csv(paste(data.dir,'type.csv',sep=''));  lpType_vec <- lpType_vec[,-1]
  lpLowerBound_vec <- read.csv(paste(data.dir,'bounds.lower.csv',sep=''));  lpLowerBound_vec <- lpLowerBound_vec[,-1]
  lpUpperBound_vec <- read.csv(paste(data.dir,'bounds.upper.csv',sep=''));  lpUpperBound_vec <- lpUpperBound_vec[,-1]
  lpBranchMode_vec <- read.csv(paste(data.dir,'branch.mode.csv',sep='')); lpBranchMode_vec <- lpBranchMode_vec[,-1]
  
  lpPresolve <- 'knapsack'
  lpEpsd <- 1e-10
  lpTimeout <- 1
  ### end ##################
  
  ### call lpSolve solver####
  solverOutput_list <- CallLpSolve(lpObj_vec,lpCon_mat,lpDir_vec,lpRhs_vec,
                        lpType_vec=lpType_vec,lpKind_vec=lpKind_vec,lpLowerBound_vec=lpLowerBound_vec,lpUpperBound_vec=lpUpperBound_vec,lpBranchMode_vec=lpBranchMode_vec,
                        presolve=lpPresolve,epsd=lpEpsd,timeout=lpTimeout)
  ### end ##################
  
  #### solver outputs########
  resultStatus<- solverOutput_list$resultStatus
  solverSolution_vec <- solverOutput_list$solverSolution_vec
  solverObjValue <- solverOutput_list$solverObjValue
  #### end ##################
  
  ### result checkings ####
  checkEquals(resultStatus,0)
  result<- read.csv(paste(data.dir,'lpSolveAPI.solution.csv',sep='')); result<- result[,-1]
  checkEquals(solverSolution_vec,result)
  checkEquals(solverObjValue,839952314)
}

