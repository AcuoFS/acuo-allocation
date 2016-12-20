library('RNeo4j')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
callId1 <- c('mc1')
clientId1 <- '999'
order1 <- 'callId'

callId2 <- c('mc2','mc3')
clientId2 <- '999'
order2 <- 'callId'

callId3 <- c('mc1','mc2','mc4','mc5','mc6','mc8','mc9','mc10','mc11')
clientId3 <- '999'
order3 <- 'callId'

callId4 <- c('mc12','mc15')
clientId4 <- '999'
order4 <- 'callId'

callId5 <- c('mc13','mc14','mc16','mc18','mc19','mc20')
clientId5 <- '999'
order5 <- 'callId'

callId6 <- c('mc2','mc3','mc7','mc13','mc14','mc16','mc18','mc19','mc20')
clientId6 <- '999'
order6 <- 'callId'


#### EXAMPLE FUNCTIONS ##################
availAssetByCallIdAndClientIdEx1 <- function(){
  availAssetByCallIdAndClientId(callId1,clientId1,order1)
}
availAssetByCallIdAndClientIdEx2 <- function(){
  availAssetByCallIdAndClientId(callId2,clientId2,order2)
}
availAssetByCallIdAndClientIdEx3 <- function(){
  availAssetByCallIdAndClientId(callId3,clientId3,order3)
}
availAssetByCallIdAndClientIdEx4 <- function(){
  availAssetByCallIdAndClientId(callId4,clientId4,order4)
}
availAssetByCallIdAndClientIdEx5 <- function(){
  availAssetByCallIdAndClientId(callId5,clientId5,order5)
}
availAssetByCallIdAndClientIdEx6 <- function(){
  availAssetByCallIdAndClientId(callId6,clientId6,order6)
}

#### EXAMPLES RESULTS ####################
availAssetByCallIdAndClientIdEx1()
availAssetByCallIdAndClientIdEx2()
availAssetByCallIdAndClientIdEx3()
availAssetByCallIdAndClientIdEx4()
availAssetByCallIdAndClientIdEx5()
availAssetByCallIdAndClientIdEx6()
