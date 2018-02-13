

# test running time

testRunTime <- function(){
  res <- 1
  start.time <- proc.time()[3]
  for(i in 1:10000000){
    res <- res*i
  }
  end.time <- proc.time()[3]
  run.time <- end.time-start.time
  return(run.time)
}

testRunTime()
