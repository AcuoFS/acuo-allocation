library('RNeo4j')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
callId1 <- c('mc45')
clientId1 <- '999'

callId2 <- c('mc46','mc43')
clientId2 <- '999'

callId3 <- c('mc45','mc46','mc48','mc49','mc50','mc51')
clientId3 <- '999'

callId4 <- c('mc51','mc52','mc54','mc55','mc57','mc59','mc61') ; clientId4 <- '999'

callId5 <- c('mc53','mc54','mc56','mc58','mc59','mc60')
clientId5 <- '999'

callId6 <- c('mc10','mc11','mc12','mc13','mc14','mc15',
             'mc41','mc42','mc46','mc47','mc48','mc49',
             'mc5','mc50','mc51','mc55','mc56','mc57','mc58','mc59',
             'mc6','mc60','mc61','mc64')
clientId6 <- '999'

#### EXAMPLE FUNCTIONS ##################
availAssetByCallIdAndClientIdEx1 <- function(){
  availAssetByCallIdAndClientId(callId1,clientId1)
}
availAssetByCallIdAndClientIdEx2 <- function(){
  availAssetByCallIdAndClientId(callId2,clientId2)
}
availAssetByCallIdAndClientIdEx3 <- function(){
  availAssetByCallIdAndClientId(callId3,clientId3)
}
availAssetByCallIdAndClientIdEx4 <- function(){
  availAssetByCallIdAndClientId(callId4,clientId4)
}
availAssetByCallIdAndClientIdEx5 <- function(){
  availAssetByCallIdAndClientId(callId5,clientId5)
}
availAssetByCallIdAndClientIdEx6 <- function(){
  availAssetByCallIdAndClientId(callId6,clientId6)
}
#### EXAMPLES RESULTS ####################
availAssetByCallIdAndClientIdEx1()
availAssetByCallIdAndClientIdEx2()
availAssetByCallIdAndClientIdEx3()
availAssetByCallIdAndClientIdEx4()
availAssetByCallIdAndClientIdEx5()
availAssetByCallIdAndClientIdEx6()
