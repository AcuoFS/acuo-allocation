library('RNeo4j')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
callId1 <- c('mc1')
callId2 <- c('mc2','mc3')
callId3 <- c('mc1','mc2','mc4','mc5','mc6','mc8','mc9','mc10','mc11')
callId4 <- c('mc13','mc14','mc16','mc18','mc19','mc20')

#### EXAMPLE FUNCTIONS ##################
callInfoByCallIdEx1 <- function(){
  callInfoByCallId(callId1)
}
callInfoByCallIdEx2 <- function(){
  callInfoByCallId(callId2)
}
callInfoByCallIdEx3 <- function(){
  callInfoByCallId(callId3)
}
callInfoByCallIdEx4 <- function(){
  callInfoByCallId(callId4)
}
#### EXAMPLES RESULTS ####################
callInfoByCallIdEx1()
callInfoByCallIdEx2()
callInfoByCallIdEx3()
callInfoByCallIdEx4()
