source("src/coreAlgo.R")

### UNIT TESTS ##############
#
# function: callLpSolve()
# 

# @ TEST 1, solved
# only required inputs
testCallLpSolveFunctionWithStaticInput1 <- function(){
  ### solver inputs #######
  lp.con <- rbind(c(6,2,4),c(1,1,6),c(4,5,4))
  lp.dir <- c("<=",">=","=")
  lp.rhs <- c(150,0,48)
  lp.obj <- c(-3,-4,-3)
  ### end ##################
  
  ### call lpSolve solver####
  lpSolve.output <- callLpSolve(lp.obj,lp.con,lp.dir,lp.rhs)
  ### end ##################
  
  #### solver outputs########
  result.status<- lpSolve.output$result.status
  lpSolveAPI.solution <- lpSolve.output$lpSolveAPI.solution
  result.objective <- lpSolve.output$result.objective
  #### end ##################
  
  ### result checkings ####
  checkEquals(result.status,0)
  checkEquals(lpSolveAPI.solution,c(0,9.6,0))
  checkEquals(result.objective,-38.4)
}

# @ TEST 2, solved
# integer type
testCallLpSolveFunctionWithStaticInput2 <- function(){
  ### solver inputs #######
  lp.con <- rbind(c(6,2,4),c(1,1,6),c(4,5,4))
  lp.dir <- c("<=",">=","=")
  lp.rhs <- c(150,0,48)
  lp.obj <- c(-3,-4,-3)
  lp.type <- rep('integer',3)
  ### end ##################
  
  ### call lpSolve solver####
  lpSolve.output <- callLpSolve(lp.obj,lp.con,lp.dir,lp.rhs,lp.type=lp.type)
  ### end ##################
  
  #### solver outputs########
  result.status<- lpSolve.output$result.status
  lpSolveAPI.solution <- lpSolve.output$lpSolveAPI.solution
  result.objective <- lpSolve.output$result.objective
  #### end ##################
  
  ### result checkings ####
  checkEquals(result.status,0)
  checkEquals(lpSolveAPI.solution,c(2,8,0))
  checkEquals(result.objective,-38)
}

# @ TEST 3, infeasible
testCallLpSolveFunctionWithStaticInput3 <- function(){
  ### solver inputs #######
  lp.con <- rbind(c(1,0,1),c(1,1,0),c(0,-1,1))
  lp.dir <- c("<=",">=","=")
  lp.rhs <- c(100,60,50)
  lp.obj <- c(1,1,1)
  ### end ##################
  
  ### call lpSolve solver####
  lpSolve.output <- callLpSolve(lp.obj,lp.con,lp.dir,lp.rhs)
  ### end ##################
  
  #### solver outputs########
  result.status<- lpSolve.output$result.status
  lpSolveAPI.solution <- lpSolve.output$lpSolveAPI.solution
  result.objective <- lpSolve.output$result.objective
  #### end ##################
  
  ### result checkings ####
  checkEquals(result.status,2)
}

# @ TEST 4, solved
# callIds = c("mcp1","mcp5","mcp7","mcp38","mcp50")
testCallLpSolveFunctionWithStaticInput4 <- function(){
  ### solver inputs ########
  data.dir <- 'test/testCallLpSolve/input1/' 
  lp.con <- read.csv(paste(data.dir,'cons.csv',sep=''));  lp.con <- lp.con[,-1]
  lp.dir <- read.csv(paste(data.dir,'dir.csv',sep=''));  lp.dir <- lp.dir[,-1]
  lp.rhs <- read.csv(paste(data.dir,'rhs.csv',sep=''));  lp.rhs <- lp.rhs[,-1]
  lp.obj <- read.csv(paste(data.dir,'obj.csv',sep=''));  lp.obj <- lp.obj[,-1]
  lp.kind <- read.csv(paste(data.dir,'kind.csv',sep=''));  lp.kind <- lp.kind[,-1]
  lp.type <- read.csv(paste(data.dir,'type.csv',sep=''));  lp.type <- lp.type[,-1]
  lp.bounds.lower <- read.csv(paste(data.dir,'bounds.lower.csv',sep=''));  lp.bounds.lower <- lp.bounds.lower[,-1]
  lp.bounds.upper <- read.csv(paste(data.dir,'bounds.upper.csv',sep=''));  lp.bounds.upper <- lp.bounds.upper[,-1]
  lp.branch.mode <- read.csv(paste(data.dir,'branch.mode.csv',sep='')); lp.branch.mode <- lp.branch.mode[,-1]
  
  lp.presolve <- 'none'
  lp.epsd <- 1e-11
  lp.timeout <- 1
  ### end ##################
  
  ### call lpSolve solver####
  lpSolve.output <- callLpSolve(lp.obj,lp.con,lp.dir,lp.rhs,
                        lp.type=lp.type,lp.kind=lp.kind,lp.bounds.lower=lp.bounds.lower,lp.bounds.upper=lp.bounds.upper,lp.branch.mode=lp.branch.mode,
                        presolve=lp.presolve,epsd=lp.epsd,timeout=lp.timeout)
  ### end ##################
  
  #### solver outputs########
  result.status<- lpSolve.output$result.status
  lpSolveAPI.solution <- lpSolve.output$lpSolveAPI.solution
  result.objective <- lpSolve.output$result.objective
  #### end ##################
  
  ### result checkings ####
  checkEquals(result.status,0)
  result<- read.csv(paste(data.dir,'lpSolveAPI.solution.csv',sep='')); result<- result[,-1]
  checkEquals(lpSolveAPI.solution,result)
  checkEquals(result.objective,138404376)
}

# @ TEST 5, numerical failure
# callIds = c("mcp1","mcp5","mcp7","mcp38","mcp20","mcp22","mcp15","mcp30","mcp50","mcp51")
testCallLpSolveFunctionWithStaticInput5 <- function(){
  ### solver inputs ########
  data.dir <- 'test/testCallLpSolve/input2/' 
  lp.con <- read.csv(paste(data.dir,'cons.csv',sep=''));  lp.con <- lp.con[,-1]
  lp.dir <- read.csv(paste(data.dir,'dir.csv',sep=''));  lp.dir <- lp.dir[,-1]
  lp.rhs <- read.csv(paste(data.dir,'rhs.csv',sep=''));  lp.rhs <- lp.rhs[,-1]
  lp.obj <- read.csv(paste(data.dir,'obj.csv',sep=''));  lp.obj <- lp.obj[,-1]
  lp.kind <- read.csv(paste(data.dir,'kind.csv',sep=''));  lp.kind <- lp.kind[,-1]
  lp.type <- read.csv(paste(data.dir,'type.csv',sep=''));  lp.type <- lp.type[,-1]
  lp.bounds.lower <- read.csv(paste(data.dir,'bounds.lower.csv',sep=''));  lp.bounds.lower <- lp.bounds.lower[,-1]
  lp.bounds.upper <- read.csv(paste(data.dir,'bounds.upper.csv',sep=''));  lp.bounds.upper <- lp.bounds.upper[,-1]
  lp.branch.mode <- read.csv(paste(data.dir,'branch.mode.csv',sep='')); lp.branch.mode <- lp.branch.mode[,-1]
  
  lp.presolve <- 'knapsack'
  lp.epsd <- 1e-11
  lp.timeout <- 1
  ### end ##################
  
  ### call lpSolve solver####
  lpSolve.output <- callLpSolve(lp.obj,lp.con,lp.dir,lp.rhs,
                        lp.type=lp.type,lp.kind=lp.kind,lp.bounds.lower=lp.bounds.lower,lp.bounds.upper=lp.bounds.upper,lp.branch.mode=lp.branch.mode,
                        presolve=lp.presolve,epsd=lp.epsd,timeout=lp.timeout)
  ### end ##################
  
  #### solver outputs########
  result.status<- lpSolve.output$result.status
  lpSolveAPI.solution <- lpSolve.output$lpSolveAPI.solution
  result.objective <- lpSolve.output$result.objective
  #### end ##################
  
  ### result checkings ####
  checkEquals(result.status,5)
}

# @ TEST 6, solved
# callIds = c("mcp1","mcp5","mcp7","mcp38","mcp20","mcp22","mcp15","mcp30","mcp50","mcp51")
testCallLpSolveFunctionWithStaticInput6 <- function(){
  ### solver inputs ########
  data.dir <- 'test/testCallLpSolve/input2/' 
  lp.con <- read.csv(paste(data.dir,'cons.csv',sep=''));  lp.con <- lp.con[,-1]
  lp.dir <- read.csv(paste(data.dir,'dir.csv',sep=''));  lp.dir <- lp.dir[,-1]
  lp.rhs <- read.csv(paste(data.dir,'rhs.csv',sep=''));  lp.rhs <- lp.rhs[,-1]
  lp.obj <- read.csv(paste(data.dir,'obj.csv',sep=''));  lp.obj <- lp.obj[,-1]
  lp.kind <- read.csv(paste(data.dir,'kind.csv',sep=''));  lp.kind <- lp.kind[,-1]
  lp.type <- read.csv(paste(data.dir,'type.csv',sep=''));  lp.type <- lp.type[,-1]
  lp.bounds.lower <- read.csv(paste(data.dir,'bounds.lower.csv',sep=''));  lp.bounds.lower <- lp.bounds.lower[,-1]
  lp.bounds.upper <- read.csv(paste(data.dir,'bounds.upper.csv',sep=''));  lp.bounds.upper <- lp.bounds.upper[,-1]
  lp.branch.mode <- read.csv(paste(data.dir,'branch.mode.csv',sep='')); lp.branch.mode <- lp.branch.mode[,-1]
  
  lp.presolve <- 'knapsack'
  lp.epsd <- 1e-10
  lp.timeout <- 1
  ### end ##################
  
  ### call lpSolve solver####
  lpSolve.output <- callLpSolve(lp.obj,lp.con,lp.dir,lp.rhs,
                        lp.type=lp.type,lp.kind=lp.kind,lp.bounds.lower=lp.bounds.lower,lp.bounds.upper=lp.bounds.upper,lp.branch.mode=lp.branch.mode,
                        presolve=lp.presolve,epsd=lp.epsd,timeout=lp.timeout)
  ### end ##################
  
  #### solver outputs########
  result.status<- lpSolve.output$result.status
  lpSolveAPI.solution <- lpSolve.output$lpSolveAPI.solution
  result.objective <- lpSolve.output$result.objective
  #### end ##################
  
  ### result checkings ####
  checkEquals(result.status,0)
  result<- read.csv(paste(data.dir,'lpSolveAPI.solution.csv',sep='')); result<- result[,-1]
  checkEquals(lpSolveAPI.solution,result)
  checkEquals(result.objective,839952314)
}

