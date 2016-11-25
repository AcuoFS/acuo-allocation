library('RNeo4j')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
callId1 <- c('mc1')
clientId1 <- 'c1'

callId2 <- c('mc2','mc3')
clientId2 <- 'c1'

callId3 <- c('mc1','mc2','mc4','mc5','mc6','mc8','mc9','mc10','mc11')
clientId3 <- 'c1'

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

#### EXAMPLES RESULTS ####################
availAssetByCallIdAndClientIdEx1()
availAssetByCallIdAndClientIdEx2()
availAssetByCallIdAndClientIdEx3()