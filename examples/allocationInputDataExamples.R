library('RNeo4j')

source('src/allocationInputData.R')

allocationInputDataEx1 <- function(){
  callId <- c('mc1','mc2','mc5')
  clientId <- '999'
  result <- allocationInputData(callId,clientId)
  return(result)
}

allocationInputDataEx1()

