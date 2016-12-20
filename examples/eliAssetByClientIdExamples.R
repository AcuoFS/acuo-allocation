library('RNeo4j')
source('src/functionsOfDBRequestByExecutingCypher.R')

#### CONSTANTS, PLASE DO NOT CHANGE #####
clientId1 <- '999'
clientId2 <- '999'

#### EXAMPLE FUNCTIONS ##################
eliAssetByClientIdEx1<-function(){
  eliAssetByClientId(clientId1)
}
eliAssetByClientIdEx2<-function(){
  eliAssetByClientId(clientId2)
}

#### EXAMPLES RESULTS ####################
eliAssetByClientIdEx1()
eliAssetByClientIdEx2()
