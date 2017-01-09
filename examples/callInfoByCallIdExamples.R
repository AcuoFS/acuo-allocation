library('RNeo4j')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
callId1 <- c('mc45')
callId2 <- c('mc46','mc43')
callId3 <- c('mc45','mc46','mc48','mc49','mc50','mc51')
callId4 <- c('mc10','mc11','mc12','mc13','mc14','mc15','mc41','mc42','mc46','mc47','mc48','mc49',
             'mc5','mc50','mc51','mc55','mc56','mc57','mc58','mc59',
             'mc6','mc60','mc61','mc64') 

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

