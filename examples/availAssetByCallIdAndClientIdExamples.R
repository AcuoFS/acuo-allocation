library('RNeo4j')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
callId1 <- c('mc45')
clientId1 <- '999'

callId2 <- c('mc46','mc43')
clientId2 <- '999'

callId3 <- c('mc45','mc46','mc48','mc49','mc50','mc51')
clientId3 <- '999'

callId5 <- c('mc53','mc54','mc56','mc58','mc59','mc60')
clientId5 <- '999'


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
availAssetByCallIdAndClientIdEx5 <- function(){
  availAssetByCallIdAndClientId(callId5,clientId5)
}

#### EXAMPLES RESULTS ####################
availAssetByCallIdAndClientIdEx1()
availAssetByCallIdAndClientIdEx2()
availAssetByCallIdAndClientIdEx3()
availAssetByCallIdAndClientIdEx5()
