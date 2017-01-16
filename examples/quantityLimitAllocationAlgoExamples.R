library("RNeo4j")
source('src/functionsOfDBRequestByExecutingCypher.R')
source('src/allocationFunction.R')

#### CONSTANTS, PLEASE DO NOT CHANGE #####
callId1 <- c('mcp41','mcp42','mcp46','mcp47'); clientId1 <- '999'
callId2 <- c('mcp41','mcp42','mcp43','mcp44','mcp45','mcp48') ; clientId2 <- '999'
callId3 <- c('mcp44','mcp48','mcp52','mcp53') ; clientId3 <- '999'
callId4 <- c('mcp42','mcp49','mcp50','mcp51','mcp54','mcp55','mcp57','mcp59','mcp60') ; clientId4 <- '999'
callId5 <- c('mcp10','mcp11','mcp12','mcp13','mcp14','mcp15',
             'mcp41','mcp42','mcp46','mcp47','mcp48','mcp49',
             'mcp5','mcp50','mcp51','mcp55','mcp56','mcp57','mcp58','mcp59',
             'mcp6','mcp60','mcp61','mcp64') ; clientId5 <- '999'
callId6 <- c('mcp1','mcp3','mcp4','mcp5','mcp7','mcp8','mcp10','mcp14','mcp15','mcp16',
             'mcp19','mcp20','mcp22','mcp23','mcp24','mcp26','mcp27','mcp30'); clientId6 <- '999'

#### EXAMPLE FUNCTIONS ##################
quantityLimitAllocationAlgoEx1 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId1,clientId=clientId1,pref=c(2,3,5))
  
  return(allocation.result)
}
quantityLimitAllocationAlgoEx2 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId2,clientId=clientId2,pref=c(8,5,4))
  
  return(allocation.result)
}
quantityLimitAllocationAlgoEx3 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId3,clientId=clientId3,pref=c(4,9,2))
  
  return(allocation.result)
}
quantityLimitAllocationAlgoEx4 <- function(){

  allocation.result <- allocationAlgo(callId=callId4,clientId=clientId4,pref=c(9,1,7))
  
  return(allocation.result)
}
quantityLimitAllocationAlgoEx5 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId5,clientId=clientId5,pref=c(9,1,7))
  
  return(allocation.result)
}
quantityLimitAllocationAlgoEx6 <- function(){
  
  allocation.result <- allocationAlgo(callId=callId6,clientId=clientId6,pref=c(9,1,7))
  
  return(allocation.result)
}
#### EXAMPLES RESULTS ####################
quantityLimitAllocationAlgoEx1()
quantityLimitAllocationAlgoEx2()
quantityLimitAllocationAlgoEx3()
quantityLimitAllocationAlgoEx4()
quantityLimitAllocationAlgoEx5()

